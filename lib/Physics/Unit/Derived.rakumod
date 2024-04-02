use Physics::Unit;

class Unit::Derived is Unit {

    method load( %config ) {
        my @a = |%config<Derived>;

        for @a -> %h {
            my ($defn, $names) = %h<defn>, %h<names>;

            my @synonyms = |$names;

            $.dx.postfix.to-defn{@synonyms[0]} = @synonyms[1];
            $.dx.postfix.to-syns{@synonyms[0]} = @synonyms;
        }

        callsame
    }
}