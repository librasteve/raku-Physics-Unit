use Physics::Unit::Config;
use Physics::Unit::Maths;
use Physics::Unit::Parser;
use Physics::Unit::Directory;

#| viz. https://en.wikipedia.org/wiki/International_System_of_Units
#| viz. https://en.wikipedia.org/wiki/Dimensional_analysis#Definition
class Unit {
    also does Maths[Unit];
    also does Parser[Unit];

    my $cg = Config.new;
    has $.dx = Directory.instance;

    my \NumBases = 8;

    has Bool    $!final  = False;
    has Real    $!factor = 1;
    has Real    $!offset = 0;
    has Defn()  $!defn is default('');
    has Type    $!type;
    has Name    @!names  = [];
    has Int     @.dims   = 0 xx NumBases;
    has MixHash $.dmix is rw = ∅.MixHash;

    ### accessor methods ###

    method check-final      {
        #type and names are exempt and may be manually set at any time
        die "You are not allowed to change a finalized Unit!" if $!final;
    }
    method finalize         { $!final = True }

    # i.e. use 'self.attr: 42' not 'self.attr = 42'
    multi method factor($f) { self.check-final; $!factor = $f }
    multi method factor     { $!factor }

    multi method offset($o) { self.check-final; $!offset = $o }
    multi method offset     { $!offset }

    multi method defn($d)   { self.check-final; $!defn = $d }
    multi method defn       { $!defn }

    #| make new synthetic Unit when we find no definition
    method make-synth {
        my $defn = self.canonical;
        my $type = 'synthetic:' ~ $defn;

        my $u = Unit.new(:$defn, names => [$defn]);
        $u.type: $type;

        $.dx.types.to-name{$type} = $defn;

        return $type;
    }

    multi method type($t)   { $!type = $t }
    multi method type       {

        #1 type has been set
        #eg. prefix & synthetic or explicitly to clarify an ambiguity
        return when $!type;

        #2 check if name (if set) is a base type
        with $.name {
            for $.dx.types.to-name.kv -> $k, $v {
                return $k when $v;
            }
        }

        #3 look up types with matching dims
        {
            when * == 0 { self.make-synth}
            when * == 1 { .first }
            when * >= 2 { self.type-hint($_) }

        }(
            gather {
                for $.dx.types.to-dims.kv -> $key, $value {
                    take $key if $value eqv self.dims;
                }
            }
        )

    }

    multi method names(@n)  {

        if @n {
            if $.dx.unit.to-syns{@n.first} -> @syns {
                #1 predefined Unit, assign synonyms
                @!names = @syns;

            } else {
                #2 user defined Unit, assign names provided
                @!names = @n;
            }
        } else {
            #3 lookup defn in the postfix synonyms (eg. 'mm')
            for $.dx.postfix.to-syns.kv -> $k, $v {
                if $v.grep($.defn) {
                    @!names = |$v;
                }
            }
            #4 otherwise, just assign defn
            @!names = [$.defn] unless @!names;
        }

        @!names.map( { $.dx.unit.by-name{$_} =   self } );
        @!names.map( { $.dx.unit.to-defn{$_} = $.defn } );

        say "load-names: {@!names}" if $cg.db;
    }
    multi method names      { @!names || '' }

    method name             { @!names.first || '' }

    ### new & clone methods ###

    #| new by parsing defn
    multi method new( :$defn!,  :@names ) {
        my $n = self.parse( $defn, Directory.instance );
        $n.names: @names;
        $n.finalize;
        return $n
    }

    #| new by cloning
    multi method new( Unit:D $u: @names ) {
        my $n = $u.clone;
        $n.names: @names;
        $n.finalize;
        return $n
    }

    #| deep clone
    method clone {
        nextwith :names([]), :dims(@!dims.clone), :dmix($!dmix.clone);   # FIXME rm names?
    }

    #| clear non-numeric attrs (used by role Maths)
    method clear {
        $!final = False;
        $!defn  = Nil;
        $!type  = Nil;
        @!names = [];
    }

    #| loader populates Directory, but does not (yet) instatiate Unit objects
    method load( %data ) {

        my @a;
        for %data.keys -> $k {
            @a.append: |%data{$k};
        }

        #eg. ['N',  'newton'],           'kg m / s^2',
        #      |     ^^^^^^ synonyms      ^^^^^^^^^^ defn
        #	   |
        #	   > canonical name

        #| iterate over each unit line
        for @a -> %h {
            my ($defn, $names) = %h<defn>, %h<names>;

            my @synonyms = |$names;

            #| for each name (ie. synonym)
            for |$names -> $singular {
                if Unit.name-plural($singular) -> $plural {
                    @synonyms.push: $plural;
                }
            }
            for @synonyms -> $name {
                $.dx.unit.to-defn{$name} = $defn;
                $.dx.unit.to-syns{$name} = @synonyms;
            }
        }

    }

    ### output methods ###

    method Str       { self.name }

    method gist      { self.Str }

    method raku      {
        my $name-string = $.name ?? qq|[{@.names.map( {"'$_'"} ).join(',')}]| !! '[]';

        return qq:to/END/;
          Unit.new( factor => $.factor, offset => $.offset, defn => '$.defn', type => '{$.type}',
          dims => [{@!dims.join(',')}], dmix => {$!dmix.raku}, names => $name-string );
        END
    }

    method canonical {
        #reset to SI base names
        my ( $ds, @dim-str );
        for 0 ..^ NumBases -> $i {
            given @.dims[$i] {
                when 0  { $ds = '' }
                when 1  { $ds = "{$.dx.bases.names[$i]}" }
                default { $ds = "{$.dx.bases.names[$i]}$_" }
            }
            @dim-str.push: $ds if $ds;
        }
        return @dim-str.join('.')
    }

    method pretty    {
        #following SI recommendation
        my %pwr-sup-rev = $cg.pwr-superscript.kv.reverse;
        my ( $ds, @dim-str );
        for 0 ..^ NumBases -> $i {
            given @.dims[$i] {
                when 0  { $ds = '' }
                when 1  { $ds = "{$.dx.bases.names[$i]}" }
                default { $ds = "{$.dx.bases.names[$i]}%pwr-sup-rev{$_}" }
            }
            @dim-str.push: $ds if $ds;
        }
        return @dim-str.join('⋅')
    }

    ### general & class methods ###

    sub subst-shortest( $u ) {
        my $dx = Directory.instance;

        # substitutes shortest name if >1 unit name has same dimensions
        # ... so that eg. 'J' beats 'kg m^2 / s^2'
        # ... needs 'J' to be instantiated first

        my @same-dmix;
        for $dx.unit.by-name.kv -> $k,$v {
            @same-dmix.push($k) if $v.same-dmix($u)
        }

        if @same-dmix {
            my @sort-by-size = @same-dmix.sort({$^a.chars cmp $^b.chars});
            return $dx.unit.by-name{@sort-by-size[0]};  #shortest
        } else {
            return $u;
        }
    }

    multi method find( Unit:U: Unit:D $u ) {
        my $dx := Directory.instance;    # class method cant use attr

        #1 if Unit, eg. from Measure.new( ... unit => $u ), just return it
        say "UF1 from $u" if $cg.db;

        return $u;
    }

    multi method find( Unit:U: Str() $u ) {
        my $dx = Directory.instance;    # class method cant use attr

        #2 if unit or prefix already instantiated
        say "UF2 from $u"if $cg.db;

        return $_ with $dx.unit.by-name{$u};
        return $_ with $dx.prefix.to-unit{$u};

        #3 if name in our defns, instantiate it
        say "UF3 from $u" if $cg.db;

        for $dx.unit.to-defn -> %p {
            if %p.key.grep($u) {
                return Unit.new( defn => %p.value, names => [%p.key] );
            }
        }

        #4 if no match, instantiate new Unit as (shortest) object from definition
        say "UF4 from $u" if $cg.db;

        subst-shortest(Unit.new( defn => $u ));
    }

    multi method type-to-unit(Unit:U: Any:U) {         # FIXME does test need this multi?
        die "Cannot get the Unit of an undefined Type";
    }

    multi method type-to-unit(Unit:U: Type:D $t ) {
        my $dx := Directory.instance;    # class method cant use attr

        Unit.find: $dx.types.to-name{ $t };
    }

    multi method type-to-unit(Unit:D:) {
        Unit.find: $.dx.types.to-name{ $.type };
    }

    multi method ubn(Unit:U:) {
        my $dx := Directory.instance;
        $dx.unit.by-name;
    }

    multi method prefix-to-factor(Unit:U:) {
        my $dx := Directory.instance;    # class method cant use attr

        $dx.prefix.to-factor;
    }

    multi method postfix-to-defn(Unit:U:) {
        my $dx := Directory.instance;    # class method cant use attr

        $dx.postfix.to-defn;
    }

    multi method postfix-to-syns(Unit:U:) {
        my $dx := Directory.instance;    # class method cant use attr

        $dx.postfix.to-syns;
    }

    #| Bind new type eg. m-1
    method type-bind( Str $type-name ) {
        for @!names -> $name {
            $.dx.types.to-name{$type-name} = $name;
        }
        $.dx.types.to-dims{$type-name} = self.dims;
    }

    #| Apply or provide type hints
    multi method type-hint( @t ) {
        for $cg.type-hint.kv -> $k,$v {
            return $k if @t.sort eq $v.sort
        }
    }
    multi method type-hint {
        $cg.type-hint
    }

    #| Naive plurals - append 's' ...
    method name-plural(Unit:U: Name $n ) {
        unless $n.chars <= 2                  #...too short
            || $n.comb.first(:end) eq 's'	  #...already ends with 's'
            || $n.comb.first(:end) eq 'z'     #...already ends with 'z'
            || $n ~~ /<[\d\/^*]>/             #...contains a digit or a symbol
        {
            return $n ~ 's';
        }
    }

}
