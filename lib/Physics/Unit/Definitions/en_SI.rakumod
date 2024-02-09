use v6.d;

use Data::Dump::Tree;
use YAMLish;

sub debool($s is copy) {                           # FIXME export me
    my @boolies = <y Y yes Yes YES n N no No NO
            true True TRUE false False FALSE 
            on On ON off Off OFF>;

    $s ~~ s:g/<|w>(<@boolies>)<|w>/\"$0\"/;
    $s
}

class Physics::Unit::Definitions::en_SI {      # FIXME adjust to is Loader?
    has @!parts = <base derived prefix>;

    has %!yobs;

    submethod TWEAK {
        my $path = $?CLASS.^name.split("::").[*-3..*-1].join('/');

        for @!parts -> $part {
            %!yobs{$part} = %?RESOURCES{"$path/$part.yaml"}.&slurp.&debool.&load-yaml;
        }

        ddt %!yobs;
    }
}
