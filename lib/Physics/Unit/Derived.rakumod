use Physics::Unit;

class Unit::Derived is Unit {

    method load( @a ) {

        for @a -> %h {
            my ($defn, $names) = %h<defn>, %h<names>;

            my @synonyms = |$names;

            $.dx.postfix.to-defn{@synonyms[0]} = @synonyms[1];    # FIXME not really a defn, eh?
            $.dx.postfix.to-syns{@synonyms[0]} = @synonyms;
        }

        callwith {Derived => @a}
    }
}
