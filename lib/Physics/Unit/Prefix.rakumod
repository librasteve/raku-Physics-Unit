use Physics::Unit;
use Physics::Unit::Config;

class Unit::Prefix is Unit {
    my $cg = Config.new;

    #| new for Unit::Prefix
    #| skips Grammar, no dims, no dmix
    multi method new( :$factor!, :$defn!, :@names!, :$type! where * ~~ 'prefix' ) {
        callsame
        }

    method load( %config ) {
        my @a = |%config<Prefix>;

        for @a -> %h {
            my ($code, $name) = %h<names>;

            my $u = Unit.new(
                factor => %h<defn>,
                defn   => %h<defn>,
                names  => [$name],
                type   => 'prefix',
                );

            $.dx.prefix.to-unit{$name} = $u;
            $.dx.prefix.by-symbol{$code} = $u;
            $.dx.prefix.to-factor{$name} = %h<defn>;

            say "Initialized Prefix $name" if $cg.db;
        }
    }
}