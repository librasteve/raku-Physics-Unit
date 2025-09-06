#| Mathematical Methods for Units

role Maths[::Unit] {

    # linear combinations - eg. Distance / Time -> Speed

    #| times helps multiply and Parser
    multi method times( Real $r ) {

        self.factor: self.factor * $r;

        return self;
    }

    multi method times( Unit $r ) {
        self.clear;

        self.factor: self.factor * $r.factor;
        self.dims >>+=<< $r.dims;
        self.dmix = ( self.dmix (+) $r.dmix ).MixHash;

        return self;
    }

    #| invert and share help divide and Parser
    method invert {
        self.clear;

        self.factor: 1 / self.factor;
        self.dims = -<< self.dims;
        self.dmix = ( ∅ (-) self.dmix ).MixHash;

        return self;
    }

    multi method share( Real $r ) {

        self.factor: self.factor / $r;

        return self;
    }

    multi method share( Unit $r ) {
        self.clear;

        self.times: $r.invert;

        return self;
    }

    #| raise a one-element unit $e to power of $d digits
    #| used for assembling compound units in the Grammar
    method raise( $d, $e ) {
        self.clear;

        self.factor: self.factor ** $d;
        self.dims >>*=>> $d;

        my $e-can = $.dx.unit.to-syns{$e}[0];   #lookup the canonical name (first synonym)
        self.dmix{$e-can} = $d;

        return self;
    }

    ### Units part of Measure operations ###
    method multiply( Unit $r ) {
        my $l = self.clone;

        my $x = $l.times( $r.clone );    #clone r too, otherwise subtle bug if self eqv $r

        my $t = $x.type;
        my $u = Unit.type-to-unit( $t );

        return( $t, $u )
    }

    method divide( Unit $r ) {
        my $l = self.clone;

        my $x = $l.share( $r.clone );    #clone r too, otherwise subtle bug if self eqv $r

        my $t = $x.type;
        my $u = Unit.type-to-unit( $t );

        return( $t, $u );
    }

    method root-extract( Int $n where 1 <= * <= 4 ) {
        #only when all dims divisible by root
        my $l = self.clone;
           $l.clear;

        die 'Taking roots with factor != 1 is not implemented' unless $l.factor == 1;

        $l.dims = $l.dims.map({($_/$n).Int});

        for $l.dmix.kv -> $k,$v {
            $l.dmix{$k} = $v/$n
        }

        my $t = $l.type;
        my $u = Unit.type-to-unit( $t );

        return( $t, $u );
    }

    #### convert & compare methods ####

    #| used to provide shortest name
    #| note the equal factor constraint
    method same-dmix( Unit $u ) {
        return 0 unless $u.dmix  eqv self.dmix;
        return 0 unless $u.factor == self.factor;
        return 1;
    }

    #| used by Measure cmp
    method same-unit( Unit $u ) {
        return 0 unless $u.dims  eqv self.dims;
        return 0 unless $u.factor == self.factor;
        return 0 unless $u.offset == self.offset;
        return 1;
    }
}

