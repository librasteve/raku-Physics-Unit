use Physics::Unit;
use Physics::Unit::Config;

class Unit::Base is Unit {   #iamerejh
    my $cg = Config.new;

    method load( @a ) {

        my $i = 0;
        for @a -> %h {
            my ( $type, $names ) = %h.kv;

            my @synonyms = |$names;

            #| for each name (ie. synonym)
            for |$names -> $singular {                  # FIXME do this in a Plurals class?
                if $cg.plural($singular) -> $plural {
                    @synonyms.push: $plural;
                }
            }
            @synonyms.map({ $.dx.unit.to-syns{$_} = |@synonyms });

            my $u = Unit.new;                           #iamerejh
            $u.names: @synonyms;
            $u.defn: $u.name;

            #dimension vector has zeros in all but one place
            $u.dims[$i++] = 1;
            $u.dmix{$u.name} = 1;
            $u.type: $type;

            # update Directory
            $.dx.types.to-name{$type} = $u.name;
            $.dx.bases.by-type{$type} = $u;

            $.dx.bases.names.push: $u.name;

            $.dx.postfix.to-defn{$u.name} = @synonyms[1];    #extended name as value
            $.dx.postfix.to-syns{$u.name} = @synonyms;       #all synonyms as value

            $.dx.postfix.to-defn;
            say "Initialized Base $names[0]" if $cg.db;
        }
    }
}