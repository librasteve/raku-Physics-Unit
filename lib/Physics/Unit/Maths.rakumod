role Physics::Unit::Maths[::Unit] {

    ### mathematical methods ###
    multi method times( Real $t ) {
        self.factor: self.factor * $t;
        return self
    }
    multi method times( Unit $t ) {
        self.factor: self.factor * $t.factor;
        self.dims >>+=<< $t.dims;
        self.dmix = ( self.dmix (+) $t.dmix ).MixHash;
        self.type: '';
        return self
    }
    method invert {
        self.factor: 1 / self.factor;
        self.dims = -<< self.dims;
        self.dmix = ( ∅ (-) self.dmix ).MixHash;
        return self
    }
    multi method share( Real $d ) {
        self.factor: self.factor / $d;
        return self
    }
    multi method share( Unit $d ) {
        my $u = self.get-unit($d).clone;
        self.times: $u.invert;
        return self
    }
    method raise( $d, $e ) {
        #raise a one-element unit ($e) to power of $d digits
        self.factor: self.factor ** $d;
        self.dims >>*=>> $d;

        #    my $e-can = %syns-by-name{$e}[0];		#lookup the canonical name
        my $e-can = $.dictionary.get-syns(name => $e)[0];		#lookup the canonical name
        self.dmix{$e-can} = $d;
        return self
    }

    #### convert & compare methods ####

    #| used to provide shortest name
    #| note the equal factor constraint
    #| should be private (when sub are folded in)
    method same-dims( Unit $u ) {
        return 0 unless $u.dmix  eqv self.dmix;
        return 0 unless $u.factor == self.factor;
        return 1
    }

    #| used by Measure cmp
    #| maybe rename to method cmp?
    method same-unit( Unit $u ) {
        return 0 unless $u.dims  eqv self.dims;
        return 0 unless $u.factor == self.factor;
        return 1
    }

    ### Units part of Measure operations ###
    method multiply( Unit $r ) {
        my $l = self.clone;
        my $x = $l.times( $r );
        my $t = $x.type;
        my $p = self.get-prototype( $t );
        return( $t, $p )
    }
    method divide( Unit $r ) {
        my $l = self.clone;
        my $x = $l.share( $r );
        my $t = $x.type;
        my $p = self.get-prototype( $t );
        return( $t, $p )
    }
    method root-extract( Int $n where 1 <= * <= 4 ) {
        #only when all dims divisible by root
        my $l = self.clone;
        die "rebase failed" unless $l.factor == 1;
        $l.defn: '';
        $l.type: '';
        $l.dims = $l.dims.map({($_/$n).Int});

        for $l.dmix.kv -> $k,$v {
            $l.dmix{$k} = $v/$n
        }

        my $t = $l.type;
        my $p = self.get-prototype( $t );
        return( $t, $p )
    }
}

