unit module Physics::Unit:ver<1.1.26>:auth<Steve Roe (librasteve@furnival.net)>;
#viz. https://en.wikipedia.org/wiki/International_System_of_Units

use Physics::Unit::Config;
use Physics::Unit::Maths;
use Physics::Unit::Parser;

# FIXME TYPE HINTS TO TYPE CLASS METHOD
#some units have the same dimensions but are different types - type hints steer type inference
our %type-hints = %(
    Area           => <Area FuelConsumption>,
    Energy         => <Energy Torque>,
    Momentum       => <Momentum Impulse>,
    Frequency      => <Frequency Radioactivity>,
    SpecificEnergy => <SpecificEnergy Dose>,
);

#-------------------------- NEW SHIT

# todo
# externailze all but Unit
# appenders
# synthetics
# spike => synopsis => README
# FIXME s

#class Unit {...}
class Directory is export {...}

class Unit {
    also does Maths[Unit];
    also does Parser[Unit];

    my $cg = Config.new;
    has $.dx = Directory.instance;

    constant \NumBases = 8;

    has Bool    $!final  = False;
    has Real    $!factor = 1;
    has Real    $!offset = 0;
    has Defn()  $!defn   = '';
    has Type    $!type;
    has Name    @!names  = [];
    has Int     @.dims = 0 xx NumBases;
    has MixHash $.dmix is rw = ∅.MixHash;

    ### accessor methods ###

    method check-final      {
        #type and names are exempt and may be manually set at any time
        die "You're not allowed to change a finalized Unit!" if $!final;
    }
    method finalize         { $!final = True }

    # i.e. use 'self.attr: 42' not 'self.attr = 42'
    multi method factor($f) { self.check-final; $!factor = $f }
    multi method factor     { $!factor }

    multi method offset($o) { self.check-final; $!offset = $o }
    multi method offset     { $!offset }

    multi method defn($d)   { self.check-final; $!defn = $d }
    multi method defn       { $!defn }

    multi method type($t)   { $!type = $t }
    multi method type       {

        #1 type has been set
        #eg. on Prefix.load or explicitly to avoid ambiguous state
        return when $!type;

        #2 check if name (if set) is a base type
        with $.name {
            for $.dx.types.to-name.kv -> $k, $v {
                return $k when $v;
            }
        }

        #3 look up types with matching dims
        {
            when * == 0 { '' }
            when * == 1 { .first }
            when * >= 2 { .&type-hint }
        }(
            gather {
                for $.dx.types.to-dims.kv -> $key, $value {
                    take $key if $value eqv self.dims
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

        @!names.map( { $.dx.unit.to-defn{$_} = $.defn } );
        @!names.map( { $.dx.unit.by-name{$_} =   self } );

        say "load-names: {@!names}" if $cg.db;
    }
    multi method names      { @!names }

    method name             { @!names.first }

    ### new & clone methods ###

    #| new by parsing defn
    multi method new( :$defn!,  :@names ) {
#        my $n = CreateUnit( $defn );
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
        nextwith :names([]), :dims(@!dims.clone), :dmix($!dmix.clone);
    }

    #| clear all but dims and dmix (used by role Maths)
    method clear {
        $!final = False;
        $!defn  = Nil;
        $!type  = Nil;
        @!names = [];
    }

    #| loader
    method load( %config ) {

        #| just ignore the outer keys FIXME autoload Measure classes (ie for localization)
        my @a;
        for %config.keys -> $k {
            @a.append: |%config{$k};
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
                if plural($singular) -> $plural {
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
        return qq:to/END/;
          Unit.new( factor => $!factor, offset => $!offset, defn => '$.defn', type => {$.type},
          dims => [{@!dims.join(',')}], dmix => {$!dmix.raku}, names => [{@!names.map( ->$n {"'$n'"}).join(',')}] );
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
        # ... requires eg. 'J' to be instantiated first

        my @same-dims;
        for $dx.unit.by-name.kv -> $k,$v {
            @same-dims.push($k) if $v.same-dims($u)
        }

        if @same-dims {
            my @sort-by-size = @same-dims.sort({$^a.chars cmp $^b.chars});
            return $dx.unit.by-name{@sort-by-size[0]};  #shortest
        } else {
            return $u;
        }
    }
    multi method find( Unit:U: Unit:D $u ) {
        my $dx := Directory.instance;    # no instance means no attrs

        #1 if Unit, eg. from Measure.new( ... unit => $u ), just return it
        say "UF1 from $u" if $cg.db;

        return $u;
    }
    multi method find( Unit:U: Str()  $u ) {
        my $dx = Directory.instance;    # no instance means no attrs

        #2 if name or prefix already instantiated
        say "UF2 from $u" if $cg.db;

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

        return subst-shortest(Unit.new( defn => $u ));
    }

    multi method type-to-unit(Unit:U: Type $t ) {
        my $dx := Directory.instance;    # no instance means no attrs

        $dx.types.to-unit( $t );
    }
    multi method type-to-unit(Unit:D:) {
        $.dx.types.to-unit( $.type );
    }

    multi method prefix-to-factor(Unit:U:) {
        my $dx := Directory.instance;    # no instance means no attrs

        return $dx.prefix.to-factor;
    }
    multi method postfix-to-defn(Unit:U:) {
        my $dx := Directory.instance;    # no instance means no attrs

        return $dx.postfix.to-defn;
    }
    multi method postfix-to-syns(Unit:U:) {
        my $dx := Directory.instance;    # no instance means no attrs

        return $dx.postfix.to-syns;
    }

    #| Manually attach type-bind when no preset type, eg. m-1   #iamerejh - rename me
    method type-bind( Str $type-name ) {
        for @!names -> $name {
            $.dx.types.to-name{$type-name} = $name;
        }
        $.dx.types.to-dims{$type-name} = self.dims;
    }
}

class Unit::Bases {
    my $cg = Config.new;
    has $.dx = Directory.instance;

    method load( @a ) {

        my $i = 0;
        for @a -> %h {
            my ( $type, $names ) = %h.kv;

            my @synonyms = |$names;

            #| for each name (ie. synonym)
            for |$names -> $singular {                  # FIXME do this in a Plurals class?
                if plural($singular) -> $plural {
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

class Unit::Types {
    has $.dx = Directory.instance;

    method load( @a ) {
        for @a -> %h {
            $.dx.types.to-name{%h.keys} = %h.values;
        }
    }
}

class Unit::Dims {
    has $.dx = Directory.instance;

    method load( @a ) {
        for @a -> %h {
            $.dx.types.to-dims{%h.keys} = %h.values;
        }
    }
}

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

class Unit::Prefix is Unit {
    my $cg = Config.new;

    #| new for Unit::Prefix
    #| skips Grammar, no dims, no dmix
    multi method new( :$factor!, :$defn!, :@names!, :$type! where * ~~ 'prefix' ) {
        callsame
    }

    method load( %config ) {
        my @a = |%config<Prefix>;

        for @a -> %h {
            my ($code, $name) = %h<names>;

            my $u = Unit.new(
                factor => %h<defn>,
                defn   => %h<defn>,
                names  => [$name],
                type   => 'prefix',
            );

            $.dx.prefix.to-unit{$name} = $u;
            $.dx.prefix.by-symbol{$code} = $u;
            $.dx.prefix.to-factor{$name} = %h<defn>;

            say "Initialized Prefix $name" if $cg.db;
        }
    }
}

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

class Directory {
    my $cg = Config.new;

    ### Singleton ###
    my Directory $instance;
    method new {!!!}
    method instance {
        unless $instance {
            $instance = Directory.bless;
            $instance.load;
        }
        $instance;
    }

    ### Classes ###
    # a microcosm #

    my class Dx::Unit {
        has %.by-name{Name}     of Unit;
        has %.to-defn{Name}     of Defn();  #known names incl. postfix (values may be dupes)
        has %.to-syns{Name}     of Syns();  #list of synonyms (excl. user defined, incl. plurals)

        method names( --> Names() ) {
            %.by-name.keys.sort
        }
    }

    my class Dx::Bases {
        has @.names             of Name;
        has %.by-type{Type}     of Unit;
    }

    my class Dx::Types {
        has %.to-name{Type}     of Name();
        has %.to-dims{Type}     of Dims();

        method to-unit(Type $t --> Unit ) {
            Unit.find: %.to-name{$t}
        }

        method names( --> Names() ) {
            %.to-name.keys.sort
        }
    }

    my class Dx::Prefix {
        has %.to-unit{Name}     of Unit;
        has %.to-factor{Name}   of Real;
        has %.by-symbol{Symbol} of Name();  #prefix has one symbol plus one name
    }

    my class Dx::Postfix {
        has %.to-defn{Name}     of Defn();  #extended defn (eg. cm => 'centimetre') to decongest Grammar namespace
        has %.to-syns{Name}     of Syns();  #list of synonyms [n, nano] X~ [m, metre, meter, metres, meters]
    }

    ### Attributes ###
    has Dx::Unit    $.unit    .= new;
    has Dx::Bases   $.bases   .= new;
    has Dx::Types   $.types   .= new;
    has Dx::Prefix  $.prefix  .= new;
    has Dx::Postfix $.postfix .= new;

    ### Main Loader ###
    method load {
        # FIXME - load general config & inject to loader

        require Physics::Unit::Definitions::en_SI;
        my $load = Physics::Unit::Definitions::en_SI.new;

        # core type info
        Unit::Bases.new.load:    $load.config<bases>;
        Unit::Types.new.load:   $load.config<types>;
        Unit::Dims.new.load:    $load.config<dims>;

        # unit children
        Unit::Derived.new.load: $load.config<derived>;
        Unit::Prefix.new.load:  $load.config<prefix>;

        # prep for postfix exports
        Unit::Postfix.new.load;

        # load dx for non-core units
        Unit.new.load:          $load.config<units>;

        if $cg.db {
            say "+++++++++++++++++++";
            say "INITIALIZATION DONE";
            say "+++++++++++++++++++";
            say "";
        }
    }
}



######## Subroutines (Internal) #######

sub type-hint( @t ) {
	#type hints help when multiple types are found
	for %type-hints.kv -> $k,$v {
		return $k if @t.sort eq $v.sort
	}
}

sub plural( $n ) {
    #naive plurals - append 's' ...
    unless $n.chars <= 2                #...too short
        || $n.comb.first(:end) eq 's'	  #...already ends with 's'
        || $n.comb.first(:end) eq 'z'     #...already ends with 'z'
        || $n ~~ /<[\d\/^*]>/             #...contains a digit or a symbol
    {
        return $n ~ 's';
    }
}

#EOF
