let
  awsKeyId = "deadpager"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name
  awsResourcePrefix = "deadpager";
  region = "eu-central-1";

  pkgs = import <nixpkgs> {};

  makeDeadpagerMachineSpec = { zone ? "a" }: { resources, nodes, config, ... }: {

    # Cloud provider settings; here for AWS
    deployment.targetEnv = "ec2";
    deployment.ec2 = {
      accessKeyId = awsKeyId; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name
      region = region;
      zone = "${region}${zone}";
      instanceType = "t3.nano";
      ebsInitialRootDiskSize = 20; # GB
      keyPair = resources.ec2KeyPairs."${awsResourcePrefix}-key-pair";
      associatePublicIpAddress = true;
      subnetId = resources.vpcSubnets."${awsResourcePrefix}-nixops-vpc-subnet-${zone}";
      securityGroups = []; # we don't want its default `[ "default" ]`
      securityGroupIds = [ resources.ec2SecurityGroups."${awsResourcePrefix}-nixops-sg".name ];
    };

    # Packages available in SSH sessions to the machine
    environment.systemPackages = with pkgs; [
      bind.dnsutils # for `dig` etc.
      consul
      htop
      jq
    ];

    networking.firewall.allowedTCPPorts = [
      80 # HTTP
      443 # HTTPs
      8300 # Consul Server RPC
      8301 # Consul Serf LAN
      8302 # Consul Serf WAN
    ];

    services.consul =
      let
        thisConsensusServerHost = config.networking.privateIPv4;
        allConsensusServerHosts =
          map (n: n.config.networking.privateIPv4) (builtins.attrValues nodes);
        numConsensusServers = builtins.length allConsensusServerHosts;

        defaultExtraConfig = {
          bind_addr = thisConsensusServerHost;
          retry_interval = "1s";
          performance.raft_multiplier = 1;
          # We want check outputs to be reflected immediately.
          # Unfortunately due to https://github.com/hashicorp/consul/issues/1057,
          # "0s" doesn't actually work, so we use "1ns" instead.
          check_update_interval = "1ns";
          gossip_lan = {
            # Despite being named "interval", this actually controls a timeout:
            # https://github.com/hashicorp/memberlist/issues/175
            probe_interval = "1000ms";
          };
        };
      in
        {
          enable = true;
          webUi = true;
          extraConfig = defaultExtraConfig // {
            server = true;
            bootstrap_expect = numConsensusServers;
            retry_join =
              # If there's only 1 node in the network, we allow self-join;
              # otherwise, the node must not try to join itself, and join only the other servers.
              # See https://github.com/hashicorp/consul/issues/2868
              if numConsensusServers == 1
                then allConsensusServerHosts
                else builtins.filter (h: h != thisConsensusServerHost) allConsensusServerHosts;
          };
        };

  };

  baseNetworkSpec = import ./aws-networking.nix {
    prefix = awsResourcePrefix;
    inherit pkgs awsKeyId region;
  };

  networkSpec = baseNetworkSpec // {

    network.description = "DeadPager deployment";
    network.enableRollback = true;

    # Machines
    machine1 = makeDeadpagerMachineSpec { zone = "a"; };
    machine2 = makeDeadpagerMachineSpec { zone = "b"; };
    machine3 = makeDeadpagerMachineSpec { zone = "c"; };
  };
in
  networkSpec
