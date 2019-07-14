let
  awsKeyId = "deadpager"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name
  domain = "deadpager.com";
  awsResourcePrefix = "deadpager";
  region = "eu-central-1";

  pkgs = import <nixpkgs> {};

  # Get name of a service. Complains when it doesn't exist. Example:
  #   serviceUnitOf cfg.systemd.services.myservice == "myservice.service"
  serviceUnitOf = service: "${service._module.args.name}.service";

  deadpager-server = import ../default.nix { inherit pkgs; };

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
    # Note: This only creates per-machine DNS entries.
    # You must currently add the Route53 DNS entries that point from
    # the main `domain` to the per-machine entries manually in Route53, e.g.
    # using the the "Weighted" Route53 Routing Policy and Alias targets like:
    #
    #     Name            Type  Alias  Alias Target             Routing Policy  Weight  Set ID
    #     deadpager.com.  A     Yes    machine1.deadpager.com.  Weighted        1       machine1
    #     deadpager.com.  A     Yes    machine2.deadpager.com.  Weighted        1       machine2
    #     deadpager.com.  A     Yes    machine3.deadpager.com.  Weighted        1       machine3
    #
    # With this setup, `dig deadpager.com` returns one of the machine IPs
    # round-robin.
    # You may also associate a Route53 Health Check with each Alias,
    # so that unhealthy machines are not returned.
    #
    # Even better:
    # Instead of Alias Targets, you may alternatively choose the "Failover"
    # Routing Policy, and define corresponding Health Checks,
    # which allows to return multiple DNS results for the
    # `deadpager.com` domain, containing only the healthy subset.
    # This allows browsers to fallback faster if one isn't reachable.
    #
    # This cannot currently be done with nixops, because it supports
    # neither Alias Targets nor the "Failover" Routing Policy.
    #
    # Also, do not try to define a Route53 Hosted Zone with nixops
    # using `resources.route53HostedZones` if your domain is hosted
    # by a domain provider outside of AWS (and you just use nameserver
    # delegation to Route53), because then a new Hosted Zone with new
    # generated nameserver domains will be created that you have to tell
    # your domain prodiver about each time you destroy and re-create
    # the deployment with nixops.
    deployment.route53 = pkgs.lib.mkIf (domain != null) {
      accessKeyId = awsKeyId;
      hostName = "${config.networking.hostName}.${domain}";
      ttl = 60;
    };

    # Packages available in SSH sessions to the machine
    environment.systemPackages = with pkgs; [
      bind.dnsutils # for `dig` etc.
      consul
      htop
      jq
      deadpager-server
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

    systemd.services.deadpager-server = {
      requiredBy = [ "multi-user.target" ];
      bindsTo = [ (serviceUnitOf config.systemd.services.consul) ];
      after = [ (serviceUnitOf config.systemd.services.consul) ];
      # TODO Remove once deadpager-server no longer needs the `config` dir.
      preStart = ''
        mkdir -p config
      '';
      environment = {
        YESOD_APPROOT = "https://${domain}";
      };
      serviceConfig = {
        ExecStart = ''${deadpager-server}/bin/deadpager-server'';
        Restart = "always";
        RestartSec = "1s";
      };
      unitConfig = {
        StartLimitIntervalSec = 0; # ensure Restart=always is always honoured
      };
    };

    # Nginx reverse-proxies to deadpager-server.
    services.nginx = {
      enable = true;
      virtualHosts."${domain}" = {
        # TODO: HTTP-based LetsEncrypt validation for domains that
        #       are served by multiple machine is fishy, because they
        #       will all query LetsEncrypt at the same time and the
        #       LetsEncrypt validation server may reach any of them,
        #       thus often observing the wrong "nonce" (of another server).
        #       Either set up a shared file system to address that, or
        #       use DNS-based validation.
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:${toString 3000}";
      };
    };

  };

  baseNetworkSpec = import ./aws-networking.nix {
    prefix = awsResourcePrefix;
    inherit pkgs awsKeyId region;
  };

  networkSpec = pkgs.lib.recursiveUpdate baseNetworkSpec {

    network.description = "DeadPager deployment";
    network.enableRollback = true;

    # Machines
    machine1 = makeDeadpagerMachineSpec { zone = "a"; };
    machine2 = makeDeadpagerMachineSpec { zone = "b"; };
    machine3 = makeDeadpagerMachineSpec { zone = "c"; };
  };
in
  networkSpec
