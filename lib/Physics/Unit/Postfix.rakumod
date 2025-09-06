use Physics::Unit::Directory;

class Unit::Postfix {
    has $.dx = Directory.instance;

    #Load SI Prefix Symbol / Unit combos to hashes for postfix operators
    method load {
        # so far %.postfix.to-defn, .to-syns have been initialized with all base and derived unit names
        # (by Unit::Base.load and Unit::Derived.load)

        # first we are going to fine tune %.postfix.to-defn
        # generally we populate %.postfix.to-syns since that is used in Measure sub do-postfix()

        # pull out 'b', load in 'B' - handling binaries below
        $.dx.binary.to-defn<b>  = $.dx.postfix.to-defn<b>:delete;
        $.dx.binary.to-defn<B>  = 'byte';
        $.dx.postfix.to-syns<B> = <B byte bytes>;

        # pull out Binary Prefixes also
        my %binary-by-symbol;
        for $.dx.prefix.by-symbol.kv -> $s, $p {
            if $s ~~ /i/ {                          #binary
                %binary-by-symbol{$s} = $p;
                $.dx.prefix.by-symbol{$s}:delete;
            }
        }

        # exclude 'USD' - handling currency in Measure
        $.dx.postfix.to-defn<USD>:delete;

        # replace kg with g - kkg? no thanks
        $.dx.postfix.to-defn<kg>:delete;
        $.dx.postfix.to-syns<kg>:delete;
        $.dx.postfix.to-defn<g> = 'gram';
        $.dx.postfix.to-syns<g> = <g gram grams gramme grammes>;

        # delete non-declining singletons from %.postfix.to-defn so that they do not generate unwanted postfixes
        # leave them in %postfix.to-syns as we will want the syns for the singletons in Measure sub do-postfix

        #$.dx.postfix.to-defn<°>:delete;   (Angle does not make it to %.postfix.to-defn)
        $.dx.postfix.to-defn<°C>:delete;
        $.dx.postfix.to-defn<radian>:delete;
        $.dx.postfix.to-defn<steradian>:delete;

        # Angle does not make it to %postfix.to-syns ?!
        $.dx.postfix.to-syns<°> = <° degree degrees deg degs º>;

        # pour in 'l' ie. ml, cl, etc quite common
        $.dx.postfix.to-defn<l> = 'litre';
        $.dx.postfix.to-syns<l> = <l L litre litres liter liters>;

        # now %.postfix.to-defn has the right simple-names (eg 1m, 2J and so on)
        # so we now can copy these across and use them to spin up all the combos

        # note we combine prefix symbol and simple unit name to make the operator name,
        # then extend both codes & names to decongest namespace for Parser

        # handle SI Prefixes
        my %simple-names = $.dx.postfix.to-defn;    # copy

        for %simple-names.keys -> $n {
            for $.dx.prefix.by-symbol.kv -> $s, $p {

                my $combo = $s ~ $n;                                                            #eg. 'ml' (op name)
                $.dx.postfix.to-defn{$combo} = $.dx.prefix.by-symbol{$s} ~ %simple-names{$n};   #eg. 'millilitres'
                populate-syns($n, $p, $combo);
            }
        }

        # handle Binary Prefixes
        my %binary-names = $.dx.binary.to-defn;     # copy

        for %binary-names.keys -> $n {
            for %binary-by-symbol.kv -> $s, $p {

                my $combo = $s ~ $n;
                $.dx.binary.to-defn{$combo} = %binary-by-symbol{$s} ~ %binary-names{$n};

                populate-syns($n, $p, $combo);
            }
        }

        #| set up synonym list for use in Measure sub do-postfix()
        sub populate-syns($n, $p, $combo) {
            # set up synonym list for use in Measure sub do-postfix()
            my $syns = $.dx.postfix.to-syns{$n};
            $syns = [ $p X~ @$syns ];   # using @$ to prevent ~ from stringifying the whole array
            $syns.shift;                # drop eg. 'millil'
            $syns.unshift: $combo;      # insert eg. 'ml'
            $.dx.postfix.to-syns{$combo} = $syns;
        }
    }
}
