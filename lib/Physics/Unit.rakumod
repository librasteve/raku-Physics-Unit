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
            for $.dx.type-to-basename.kv -> $k, $v {
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
                for $.dx.type-to-dims.kv -> $key, $value {
                    take $key if $value eqv self.dims
                }
            }
        )

    }

    multi method names(@n)  {

        if @n {
            if $.dx.get-syns(name => @n.first) -> @syns {
                #1 predefined Unit, assign synonyms
                @!names = @syns;

            } else {
                #2 user defined Unit, assign names provided
                @!names = @n;
            }
        } else {
            #3 lookup defn in the postfix synonyms (eg. 'mm')
            for $.dx.postsyns-by-name.kv -> $k, $v {
                if $v.grep($.defn) {
                    @!names = |$v;
                }
            }
            #4 otherwise, just assign defn
            @!names = [$.defn] unless @!names;
        }

        @!names.map( { $.dx.defn-by-name{$_} = $.defn } );
        @!names.map( { $.dx.unit-by-name{$_} =   self } );

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

        #| just ignore the outer keys FIXME autoload Measure classes
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
                if naive-plural($singular) -> $plural {
                    @synonyms.push: $plural;
                }
            }
            for @synonyms -> $name {
                $.dx.defn-by-name{$name} = $defn;
                $.dx.syns-by-name{$name} = @synonyms;
            }
        }

    }

    ### behavioural methods ###

    #| Manually attach NewType when no preset type, eg. m-1
    #| FIXME - put in Type class (reverse args)
    method NewType( Str $type-name ) {
        for @!names -> $name {
            $.dx.type-to-basename{$type-name} = $name;
        }
        $.dx.type-to-basetype{$type-name} = self;
        $.dx.type-to-dims{$type-name} = self.dims;
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

    ### class methods ###

    sub subst-shortest( Unit $u ) {
        my $dx = Directory.instance;

        # substitutes shortest name if >1 unit name has same dimensions
        # ... so that eg. 'J' beats 'kg m^2 / s^2'
        # ... requires eg. 'J' to be instantiated first

        my @same-dims;
        for $dx.unit-by-name.kv -> $k,$v {
            @same-dims.push($k) if $v.same-dims($u)
        }

        if @same-dims {
            my @sort-by-size = @same-dims.sort({$^a.chars cmp $^b.chars});
            return $dx.unit-by-name{@sort-by-size[0]};  #shortest
        } else {
            return $u;
        }
    }

    multi method find( Unit:U: $u ) {
        my $dx = Directory.instance;    # no instance means no attrs

        #1 if Unit, eg. from Measure.new( ... unit => $u ), just return it
        say "UF1 from $u" if $cg.db;

        return $u if $u ~~ Unit;

        #2 if name or prefix already instantiated
        say "UF2 from $u" if $cg.db;

        return $_ with $dx.unit-by-name{~$u};
        return $_ with $dx.prefix.by-name{~$u};

        #3 if name in our defns, instantiate it
        say "UF3 from $u" if $cg.db;

        for $dx.defn-by-name -> %p {
            if %p.key.grep($u) {
                return Unit.new( defn => %p.value, names => [%p.key] );
            }
        }

        #4 if no match, instantiate new Unit as (shortest) object from definition
        say "UF4 from $u" if $cg.db;

        return subst-shortest(Unit.new( defn => $u ));
    }

    #iamerejh
    method get-basetype( Type $t ) {
        GetBase( $t );
    }

    method rebase {
        GetBase( $.type );
    }
}

class Unit::Bases {
    my $cg = Config.new;
    has $.dx = Directory.instance;

    method load( @a ) {

        my $i = 0;
        for @a -> %h {
            my ( $type, $names ) = %h.kv;

            my @synonyms = |$names;                     # FIXME do this in a Synonym class?

            #| for each name (ie. synonym)
            for |$names -> $singular {                  # FIXME do this in a Plurals class?
                if naive-plural($singular) -> $plural {
                    @synonyms.push: $plural;
                }
            }
            @synonyms.map({ $.dx.syns-by-name{$_} = |@synonyms });

            my $u = Unit.new;
            $u.names: @synonyms;
            $u.defn: $u.name;

            #dimension vector has zeros in all but one place
            $u.dims[$i++] = 1;
            $u.dmix{$u.name} = 1;
            $u.type: $type;

            $.dx.type-to-basename{$type} = $u.name;
            $.dx.type-to-basetype{$type} = $u;

            $.dx.bases.names.push: $u.name;
            
            $.dx.postfix-by-name{$u.name} = @synonyms[1];    #extended name as value
            $.dx.postsyns-by-name{$u.name} = @synonyms;       #all synonyms as value

            $.dx.postfix-by-name;
            say "Initialized Base $names[0]" if $cg.db;
        }
    }
}

class Unit::Types {
    has $.dx = Directory.instance;

    method load( @a ) {
        for @a -> %h {
            $.dx.type-to-basename{%h.keys} = %h.values;
        }
    }
}

class Unit::Dims {
    has $.dx = Directory.instance;

    method load( @a ) {
        for @a -> %h {
            $.dx.type-to-dims{%h.keys} = %h.values;
        }
    }
}

class Unit::Derived is Unit {
    method load( %config ) {
        my @a = |%config<Derived>;

        for @a -> %h {
            my ($defn, $names) = %h<defn>, %h<names>;

            my @synonyms = |$names;

            $.dx.postfix-by-name{@synonyms[0]} = @synonyms[1];
            $.dx.postsyns-by-name{@synonyms[0]} = @synonyms;
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

            $.dx.prefix.by-name{$name} = $u;
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
        # so far %postfix-by-name has been initialized with base and derived unit names
        # redo THIS!!! ^^^

        # replace kg with g
        $.dx.postfix-by-name<kg>:delete;
        $.dx.postsyns-by-name<kg>:delete;
        $.dx.postfix-by-name<g> = 'gram';
        $.dx.postsyns-by-name<g> = <g gram grams gramme grammes>;

        # delete non-declining singletons from %postfix-by-name so that they do not generate unwanted postfixes
        # leave them in %postfix-syns-by-name as we will want the syns for the singletons in do-postfix
        #$.dx.postfix-by-name<°>:delete;   (Angle does not make it to %postfix-by-name)
        $.dx.postfix-by-name<°C>:delete;
        $.dx.postfix-by-name<radian>:delete;
        $.dx.postfix-by-name<steradian>:delete;

        # Angle does not make it to %postfix-syns-by-name ?!
        $.dx.postsyns-by-name<°> = <° degree degrees deg degs º>;

        # pour in 'l' ie. ml, cl, etc quite common
        $.dx.postfix-by-name<l> = 'litre';
        $.dx.postsyns-by-name<l> = <l L litre litres liter liters>;

        # now %postfix-by-name has the right simple-names
        # so now can copy these across and us them to spin up all the combos
        my %simple-names = $.dx.postfix-by-name;

        for %simple-names.keys -> $n {
            for $.dx.prefix.by-symbol.kv -> $c, $p {

                # combine short keys and values, then extend both codes & names to decongest namespace
                my $combo = $c ~ $n;                                                #eg. 'ml' (used by custom Postfix op)
                $.dx.postfix-by-name{$combo} = $.dx.prefix.by-symbol{$c} ~ %simple-names{$n};   #eg. 'millilitres' (used by Grammar)

                # set up synonym list for population of Unit object name
                my $syns = $.dx.postsyns-by-name{$n};
                $syns = [ $p X~ @$syns ];   # using @$ to prevent ~ from stringifying the whole array
                $syns.shift;                # drop eg. 'millil'
                $syns.unshift: $combo;      # insert eg. 'ml'

                $.dx.postsyns-by-name{$combo} = $syns;
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

    my class Dx::Bases {
        has @.names of Name;
    }

    my class Dx::Prefix {
        has %.by-name{Name}     of Unit;
        has %.by-symbol{Symbol} of Name();   #ie. Prefix may have only one name
        has %.to-factor{Name}   of Real;
    }

    my class Dx::Types {

    }

    ### Attributes ###
    has Dx::Bases  $.bases  .= new;
    has Dx::Prefix $.prefix .= new;


    #types
    has %.type-to-basename;    #type => basetype name
    has %.type-to-basetype;    #type => basetype Unit object (when instantiated)
    has Array[Int]() %.type-to-dims;		    #type => dims vector

    #postfix
    has %.postfix-by-name;      #name => extended postfix defn (eg. cm => 'centimetre') to decongest Grammar namespace
    has %.postsyns-by-name;     #name => list of synonyms for every postfix [n, nano] X~ [m, metre, meter, metres, meters]

    #units
    has %.defn-by-name;         #name => defn Str of known names incl. postfix (values may be dupes)
    has %.syns-by-name;         #name => list of synonyms (excl. user defined, incl. plurals)
    has %.unit-by-name;         #name => Unit object cache (when instantiated)
    method get-syns(:$name) {       # type as Name?
        %!syns-by-name{$name}
    }



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


######## Subroutines (Exported) ########
#units
sub ListSyns is export {       # FIXME make Unit class method (revert to $!dx)
    my $dx := Directory.instance;

    $dx.syns-by-name;
    #	return sort keys $dx.defn-by-name;
}
sub ListDefns is export {       # FIXME make Unit class method (revert to $!dx)
    my $dx := Directory.instance;

    $dx.defn-by-name;
#	return sort keys $dx.defn-by-name;
}
sub ListUnits is export {       # FIXME make Unit class method (revert to $!dx)
    my $dx := Directory.instance;

    return sort keys $dx.defn-by-name;
}

#types
sub GetBase(Type $type ) is export {     # FIXME make Unit class method (revert to $!dx)
    my $dx := Directory.instance;

    if my $pt = $dx.type-to-basetype{$type} {
        return $pt;
    } else {
        for $dx.type-to-basename -> %p {
            return Unit.find(%p.value) if %p.key eq $type;
        }
    }
}

sub ListTypeNames is export {       # FIXME make Unit class method (revert to $!dx)
    my $dx := Directory.instance;

    $dx.type-to-basename
#    return sort keys $dx.type-to-basename;
}

sub ListPrototypes is export {       # FIXME make Unit class method (revert to $!dx)
    my $dx := Directory.instance;

    $dx.type-to-basetype    #ie type-to-base-symbol #iamerejh
#    return sort keys $dx.type-to-basetype;
}



#prefix
sub GetPrefixToFactor is export {
    my $dx := Directory.instance;

    return $dx.prefix.to-factor;
}
#postfix
sub GetPostfixByName is export {
    my $dx := Directory.instance;

    return $dx.postfix-by-name;
}
sub GetPostfixSynsByName is export {
    my $dx := Directory.instance;

    return $dx.postsyns-by-name;
}

#| DEPRECATED - rm with Measure ver 2
sub GetUnit( $u ) is export {
    Unit.find: $u
}

######## Subroutines (Internal) #######

sub type-hint( @t ) {
	#type hints help when multiple types are found
	for %type-hints.kv -> $k,$v {
		return $k if @t.sort eq $v.sort
	}
}

sub naive-plural( $n ) {
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
