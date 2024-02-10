use v6.d;

use Data::Dump::Tree;
use YAMLish;

sub debool(Str $s is copy --> Str) {                           # pr to yamlish 09-Feb-24
    # avoid yaml taking certain bare strings as Bools by quoting them

    my @booleys =  <y Y yes Yes YES
                    n N no  No  NO
                    true  True  TRUE
                    false False FALSE
                    on    On    ON
                    off   Off   OFF>;

    $s ~~ s:g/<|w>(<@booleys>)<|w>/\"$0\"/;
    $s
}


class Physics::Unit::Definitions::en_SI {      # FIXME adjust to is Loader?
    #viz. https://en.wikipedia.org/wiki/Dimensional_analysis#Definition

    has @!parts = <base derived prefix types dims units>;

    has %!yobs;

    submethod TWEAK {
        my $path = $?CLASS.^name.split("::").[*-3..*-1].join('/');

        for @!parts -> $part {
            #e.g. /Unit/Definitions/en_SI/base.yaml
            %!yobs{$part} = %?RESOURCES{"$path/$part.yaml"}.slurp.&debool.&load-yaml;
        }

        ddt %!yobs.sort;
    }
}
