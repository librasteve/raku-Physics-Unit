use Data::Dump::Tree;

#use YAMLish;
#
## Specify the path to the YAML file
#my $file-path = "spike.yaml";
#
## Read the YAML file
#my $yaml-content = slurp $file-path;
#
## Parse YAML content into a data structure
#my $data = load-yaml($yaml-content);
#
## Print the data structure
#ddt $data;

use Physics::Unit::Definitions::en_SI;

Physics::Unit::Definitions::en_SI.new