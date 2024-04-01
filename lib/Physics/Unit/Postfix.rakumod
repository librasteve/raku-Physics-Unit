use Physics::Unit::Directory;

class Unit::Postfix {
    has $.dx = Directory.instance;

    #Load SI Prefix code / Unit combos to data map hashes for postfix operators
    method load {
        # so far %.postfix.to-defn has been initialized with base and derived unit names

        # replace kg with g
        $.dx.postfix.to-defn<kg>:delete;
        $.dx.postfix.to-syns<kg>:delete;
        $.dx.postfix.to-defn<g> = 'gram';
        $.dx.postfix.to-syns<g> = <g gram grams gramme grammes>;

        # delete non-declining singletons from %.postfix.to-defn so that they do not generate unwanted postfixes
        # leave them in %postfix-unit.to-syns as we will want the syns for the singletons in do-postfix
        #$.dx.postfix.to-defn<°>:delete;   (Angle does not make it to %.postfix.to-defn)
        $.dx.postfix.to-defn<°C>:delete;
        $.dx.postfix.to-defn<radian>:delete;
        $.dx.postfix.to-defn<steradian>:delete;

        # Angle does not make it to %postfix-unit.to-syns ?!
        $.dx.postfix.to-syns<°> = <° degree degrees deg degs º>;

        # pour in 'l' ie. ml, cl, etc quite common
        $.dx.postfix.to-defn<l> = 'litre';
        $.dx.postfix.to-syns<l> = <l L litre litres liter liters>;

        # now %.postfix.to-defn has the right simple-names
        # so now can copy these across and us them to spin up all the combos
        my %simple-names = $.dx.postfix.to-defn;

        for %simple-names.keys -> $n {
            for $.dx.prefix.by-symbol.kv -> $c, $p {

                # combine short keys and values, then extend both codes & names to decongest namespace
                my $combo = $c ~ $n;                                                #eg. 'ml' (used by custom Postfix op)
                $.dx.postfix.to-defn{$combo} = $.dx.prefix.by-symbol{$c} ~ %simple-names{$n};   #eg. 'millilitres' (used by Grammar)

                # set up synonym list for population of Unit object name
                my $syns = $.dx.postfix.to-syns{$n};
                $syns = [ $p X~ @$syns ];   # using @$ to prevent ~ from stringifying the whole array
                $syns.shift;                # drop eg. 'millil'
                $syns.unshift: $combo;      # insert eg. 'ml'

                $.dx.postfix.to-syns{$combo} = $syns;
            }
        }
    }
}