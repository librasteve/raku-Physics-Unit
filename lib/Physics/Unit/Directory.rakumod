#| Directory Service for Units

use Physics::Unit::Config;

class Directory {
    my $cg = Config.new;

    ### Singleton ###
    my Directory $instance;
    method new {!!!}
    method instance {
        unless $instance {
            $instance = Directory.bless;
            $cg.load;
        }
        $instance;
    }

    ### Classes ###
    # a microcosm #

    my class Dx::Unit {
        has %.by-name{Name};  # of Unit;
        has %.to-defn{Name}     of Defn();  #known names incl. postfix (values may be dupes)
        has %.to-syns{Name}     of Syns();  #list of synonyms (excl. user defined, incl. plurals)

        method names( --> Names() ) {
            %.by-name.keys.sort
        }
    }

    my class Dx::Types {
        has %.to-name{Type}     of Name();
        has %.to-dims{Type}     of Dims();

        method names( --> Names() ) {
            %.to-name.keys.sort
        }
    }

    my class Dx::Base {
        has @.names             of Name;
        has %.by-type{Type};  # of Unit;
    }

    my class Dx::Prefix {
        has %.to-unit{Name};  # of Unit;
        has %.to-factor{Name}   of Real;
        has %.by-symbol{Symbol} of Name();  #prefix has one symbol plus one name
    }

    my class Dx::Postfix {
        has %.to-defn{Name}     of Defn();  #extended defn (eg. cm => 'centimetre') to decongest Grammar namespace
        has %.to-syns{Name}     of Syns();  #list of synonyms [n, nano] X~ [m, metre, meter, metres, meters]
    }

    my class Dx::Classes {
        has @.names             of Name;   #iamerejh make me a % and delete Derived
    }

    ### Attributes ###
    has Dx::Unit    $.unit    .= new;
    has Dx::Base    $.bases   .= new;
    has Dx::Types   $.types   .= new;
    has Dx::Prefix  $.prefix  .= new;
    has Dx::Postfix $.postfix .= new;
    has Dx::Classes $.classes .= new;
}

