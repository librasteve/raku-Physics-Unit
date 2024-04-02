use YAMLish;

#use Physics::Unit::Config;

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

class Physics::Unit::en_SI {

    my $raph = '.raph-config';

    #| parts is also specified in Build.rakumod
    has @.parts = <base types dims derived prefix general>;

    has %.config;

    submethod TWEAK {
        my $path = 'Unit/Definitions/en_SI';

        for @!parts -> $part {
            #e.g. /Unit/Definitions/en_SI/base.yaml
            %!config{$part} = "$*HOME/$raph/$path/$part.yaml".IO.slurp.&load-yaml: :schema(Schema::Core::NoBools);
        }

        for @!parts -> $part {
            my $fqn = "Physics::Unit::" ~ $part.tc;
            require ::($fqn);

            my $pqn = "Unit::" ~ $part.tc;
            ::($pqn).new.load: %!config{$part};
        }


    }

}
