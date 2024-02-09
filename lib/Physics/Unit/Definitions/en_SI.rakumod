use v6.d;

use Data::Dump::Tree;
use YAMLish;

class Physics::Unit::Definitions::en_SI {      #adjust to is Loader?
    has @!parts = <base derived prefix>;

    has %!yobs;

    submethod TWEAK {
        my $path = $?CLASS.^name.split("::").[*-3..*-1].join('/');

        for @!parts -> $part {
            %!yobs{$part} = %?RESOURCES{"$path/$part.yaml"}.slurp.&load-yaml
        }
        ddt %!yobs;


    }
}
