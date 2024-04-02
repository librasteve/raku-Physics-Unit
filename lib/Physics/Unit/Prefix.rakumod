use Physics::Unit;

class Unit::Prefix is Unit {

    #| new for Unit::Prefix skips Grammar, no dims, no dmix
    multi method new( :$factor!, :$defn!, :@names!, :$type! where * ~~ 'prefix' ) {
        callsame
    }

    method load( %data ) {
        my @a = |%data<Prefix>;

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
        }
    }
}