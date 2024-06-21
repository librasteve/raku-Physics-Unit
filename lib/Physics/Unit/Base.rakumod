use Physics::Unit;

class Unit::Base is Unit {

    method load( @a ) {

        my $i = 0;
        for @a -> %h {
            my ( $type, $names ) = %h.kv;

            my @synonyms = |$names;

            #| for each name (ie. synonym)
            for |$names -> $singular {
                if Unit.name-plural($singular) -> $plural {
                    @synonyms.push: $plural;
                }
            }
            @synonyms.map({ $.dx.unit.to-syns{$_} = |@synonyms });

            my $u = Unit.new;
            $u.names: @synonyms;
            $u.defn: $u.name;

            #dimension vector has zeros in all but one place
            $u.dims[$i++] = 1;
            $u.dmix{$u.name} = 1;
            $u.type: $type;

            # update Directory
            $.dx.type.to-name{$type} = $u.name;
            $.dx.base.by-type{$type} = $u;

            $.dx.base.names.push: $u.name;

            $.dx.postfix.to-defn{$u.name} = @synonyms[1];    #extended name as value
            $.dx.postfix.to-syns{$u.name} = @synonyms;       #all synonyms as value

            $.dx.postfix.to-defn;
        }
    }

}