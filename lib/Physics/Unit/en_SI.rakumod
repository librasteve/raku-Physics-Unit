use YAMLish;

use Physics::Unit;
use Physics::Unit::Postfix;

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

class Physics::Unit::en_SI is Unit {
    has $.raph;
    has $.path;
    has @.parts;

    method load {
        my %data;

        # read the .yaml files
        for @!parts -> $part {
            #e.g. /Unit/Definitions/en_SI/base.yaml
            %data{$part} =
                "$*HOME/$!raph/$!path/$part.yaml".IO.slurp.&load-yaml:
                    :schema(Schema::Core::NoBools);
        }

        # run each Unit::Part loader
        for @!parts -> $part {
            my $fqn = "Physics::Unit::" ~ $part.tc;
            require ::($fqn);

            my $pqn = "Unit::" ~ $part.tc;
            ::($pqn).new.load: %data{$part};
        }

        # don't forget to load postfix
        Unit::Postfix.new.load;
    }
}
