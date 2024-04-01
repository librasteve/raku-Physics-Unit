use YAMLish;
use Physics::Unit;

use Physics::Unit::Dims;
use Physics::Unit::Base;
use Physics::Unit::Derived;
use Physics::Unit::Prefix;
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

class Physics::Unit::en_SI {

    my $raph = '.raph-config';

    has @.parts = <base types dims derived prefix units>;

    has %.config;

    submethod TWEAK {
        my $path = 'Unit/Definitions/en_SI';

        for @!parts -> $part {
            #e.g. /Unit/Definitions/en_SI/base.yaml
            %!config{$part} = "$*HOME/$raph/$path/$part.yaml".IO.slurp.&load-yaml: :schema(Schema::Core::NoBools);
        }

        # core type info
        Unit::Types.new.load:   %!config<types>;
        Unit::Dims.new.load:    %!config<dims>;

        # unit children
        Unit::Base.new.load:    %!config<base>;
        Unit::Derived.new.load: %!config<derived>;
        Unit::Prefix.new.load:  %!config<prefix>;

        # load dx for non-core units
        Unit.new.load:          %!config<units>;

        # prep for postfix exports
        Unit::Postfix.new.load;
    }

}
