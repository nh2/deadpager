{
  pkgs,

  prefix ? "my", # Prepended to the name of all AWS resources; should be nonempty.
  awsKeyId,
  region,
  # We must declare an AWS Subnet for each Availability Zone
  # because Subnets cannot span AZs.
  # Note: If you override this, consider that below we have a `sourceIp = "10.0.0.0/8";`
  #       firewall rule, so you probably want to stay within that subnet.
  #       This note can be removed once the TODO down below is implemented.
  subnets ? [
    { name = "${prefix}-nixops-vpc-subnet-a"; cidr = "10.0.0.0/19"; zone = "${region}a"; }
    { name = "${prefix}-nixops-vpc-subnet-b"; cidr = "10.0.32.0/19"; zone = "${region}b"; }
    { name = "${prefix}-nixops-vpc-subnet-c"; cidr = "10.0.64.0/19"; zone = "${region}c"; }
  ],
}:
assert builtins.stringLength prefix > 0;
{
  resources.ec2KeyPairs."${prefix}-key-pair" = {
    accessKeyId = awsKeyId;
    inherit region;
  };

  resources.vpc."${prefix}-nixops-vpc" = {
    accessKeyId = awsKeyId;
    inherit region;
    instanceTenancy = "default";
    enableDnsSupport = true;
    enableDnsHostnames = true;
    cidrBlock = "10.0.0.0/16";
    tags.Source = "NixOps";
  };

  resources.vpcSubnets =
    let
      makeSubnet = { cidr, zone }: { resources, ... }: {
        accessKeyId = awsKeyId;
        inherit region zone;
        vpcId = resources.vpc."${prefix}-nixops-vpc";
        cidrBlock = cidr;
        mapPublicIpOnLaunch = true;
        tags.Source = "NixOps";
      };
    in
      # We must declare a Subnet for each Availability Zone
      # because Subnets cannot span AZs.
      builtins.listToAttrs
        (map
          ({ name, cidr, zone }: pkgs.lib.nameValuePair name (makeSubnet { inherit cidr zone; }) )
          subnets
        );

  resources.ec2SecurityGroups."${prefix}-nixops-sg" = { resources, lib, ... }: {
    accessKeyId = awsKeyId;
    inherit region;
    vpcId = resources.vpc."${prefix}-nixops-vpc";
    rules = [
      { protocol = "tcp"; toPort = 22; fromPort = 22; sourceIp = "0.0.0.0/0"; } # SSH
      { protocol = "tcp"; toPort = 80; fromPort = 80; sourceIp = "0.0.0.0/0"; } # HTTP
      { protocol = "tcp"; toPort = 443; fromPort = 443; sourceIp = "0.0.0.0/0"; } # HTTPS
      # TODO: This should better use `sourceGroup`, but hat needs
      #       https://github.com/NixOS/nixops/issues/456 first as we can't query the owner id.
      { protocol = "-1"; toPort = -1; fromPort = -1; sourceIp = "10.0.0.0/8"; } # All internal traffic
    ];
  };

  resources.vpcRouteTables = {
    route-table = { resources, ... }: {
      accessKeyId = awsKeyId;
      inherit region;
      vpcId = resources.vpc."${prefix}-nixops-vpc";
    };
  };

  resources.vpcRoutes = {
    igw-route = { resources, ... }: {
      accessKeyId = awsKeyId;
      inherit region;
      routeTableId = resources.vpcRouteTables.route-table;
      destinationCidrBlock = "0.0.0.0/0";
      gatewayId = resources.vpcInternetGateways."${prefix}-nixops-igw";
    };
  };

  resources.vpcRouteTableAssociations =
    let
      association = subnetName: { resources, ... }: {
        accessKeyId = awsKeyId;
        inherit region;
        subnetId = resources.vpcSubnets."${subnetName}";
        routeTableId = resources.vpcRouteTables.route-table;
      };
    in
      builtins.listToAttrs
        (map
          ({ name, ... }: pkgs.lib.nameValuePair "association-${name}" (association name) )
          subnets
        );

  resources.vpcInternetGateways."${prefix}-nixops-igw" = { resources, ... }: {
    accessKeyId = awsKeyId;
    inherit region;
    vpcId = resources.vpc."${prefix}-nixops-vpc";
  };
}
