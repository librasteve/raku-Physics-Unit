use YAMLish;

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

    my $raph = '.raph-config';

    has @!parts = <base types dims derived prefix units>;

    has %.config;

    submethod TWEAK {
        my $path = $?CLASS.^name.split("::").[*-3..*-1].join('/');

        for @!parts -> $part {
            #e.g. /Unit/Definitions/en_SI/base.yaml
            %!config{$part} = "$*HOME/$raph/$path/$part.yaml".IO.slurp.&load-yaml: :schema(Schema::Core::NoBools);
        }
    }
}
