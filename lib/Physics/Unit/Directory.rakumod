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
        has %.by-name{Name};  # of Unit;    #instantiated Units only

        has %.to-defn{Name}     of Defn();  #all known names incl. postfix (values may be dupes)
        has %.to-syns{Name}     of Syns();  #list of synonyms (excl. user defined, incl. plurals)

        method names( --> Names() ) {
            %.to-defn.keys.sort
        }
    }

    my class Dx::Base {
        has @.names             of Name;
        has %.by-type{Type};  # of Unit;
    }

    my class Dx::Type {
        has %.to-name{Type}     of Name();
        has %.to-dims{Type}     of Dims();

        method names( --> Names() ) {
            %.to-name.keys.sort
        }
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

    ### Attributes ###
    has Dx::Unit    $.unit    .= new;
    has Dx::Base    $.base    .= new;
    has Dx::Type    $.type    .= new;
    has Dx::Prefix  $.prefix  .= new;
    has Dx::Postfix $.postfix .= new;
}

