use Data::Dump::Tree;
use YAMLish;

#sub debool(Str $s is copy --> Str) {
#    # pre-empt yaml taking certain bare strings as Bools by quoting them
#
#    my @booleys =  <y Y yes Yes YES
#                    n N no  No  NO
#                    true  True  TRUE
#                    false False FALSE
#                    on    On    ON
#                    off   Off   OFF>;
#
#    $s ~~ s:g/<|w>(<@booleys>)<|w>/\"$0\"/;
#    $s
#}

grammar Schema::Core::NoBools is Schema::Core {
    token element:<yes> {
        $<value>=[ :i y | yes | true | on ] <|w>
        { make ~$<value> }
    }
    token element:<no> {
        $<value>=[ :i n | no | false | off ] <|w>
        { make ~$<value> }
    }
}

class Physics::Unit::Definitions::en_SI {      # FIXME adjust to is Loader?
    #viz. https://en.wikipedia.org/wiki/Dimensional_analysis#Definition

    has @!parts = <prefix>;
#    has @!parts = <prefix base derived types dims units>;

    has %.yobs;

    submethod TWEAK {
        my $path = $?CLASS.^name.split("::").[*-3..*-1].join('/');

        for @!parts -> $part {
            #e.g. /Unit/Definitions/en_SI/base.yaml
            %!yobs{$part} = %?RESOURCES{"$path/$part.yaml"}
                    .slurp.&load-yaml: :schema(Schema::Core::NoBools);
        }
    }
}
