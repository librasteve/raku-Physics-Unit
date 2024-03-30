#| Mathematical Methods for Units

role Maths[::Unit] {

    ### Scaling by Real factor ###
    multi submethod times( Real $t ) {
        self.factor: self.factor * $t;
        self
    }

    multi submethod share( Real $d ) {
        self.factor: self.factor / $d;
        self
    }

    ### Combining with another Unit ###
    multi submethod times( Unit $t ) {
        self.clear;

        self.factor: self.factor * $t.factor;
        self.dims >>+=<< $t.dims;
        self.dmix = ( self.dmix (+) $t.dmix ).MixHash;

        self
    }

    submethod invert {
        self.clear;

        self.factor: 1 / self.factor;
        self.dims = -<< self.dims;
        self.dmix = ( ∅ (-) self.dmix ).MixHash;

        self
    }

    multi submethod share( Unit $d ) {
        self.clear;

        my $u = Unit.find($d).clone;
        self.times: $u.invert;

        self
    }

    #| raise a one-element unit $e to power of $d digits
    #| used for assembling compound units in the Grammar
    method raise( $d, $e ) {
        self.clear;

        self.factor: self.factor ** $d;
        self.dims >>*=>> $d;

        my $e-can = $.dx.unit.to-syns{$e}[0];   #lookup the canonical name
        self.dmix{$e-can} = $d;

        self
    }

    ### Units part of Measure operations ###
    method multiply( Unit $r ) {
        my $l = self.clone;
        my $x = $l.times( $r );

        my $t = $x.type;
        my $u = Unit.type-to-unit( $t );
        return( $t, $u )
    }

    method divide( Unit $r ) {
        my $l = self.clone;
        my $x = $l.share( $r );

        my $t = $x.type;
        my $u = Unit.type-to-unit( $t );
        return( $t, $u )
    }

    method root-extract( Int $n where 1 <= * <= 4 ) {
        #only when all dims divisible by root
        my $l = self.clone;
           $l.clear;
        die 'Taking roots only works where factor == 1' unless $l.factor == 1;

        $l.dims = $l.dims.map({($_/$n).Int});

        for $l.dmix.kv -> $k,$v {
            $l.dmix{$k} = $v/$n
        }

        my $t = $l.type;
        my $u = Unit.type-to-unit( $t );
        return( $t, $u )
    }

    #### convert & compare methods ####

    #| used to provide shortest name
    #| note the equal factor constraint
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
}

