unit module Physics::Unit:ver<1.1.26>:auth<Steve Roe (librasteve@furnival.net)>; #viz. https://en.wikipedia.org/wiki/International_System_of_Units

use Data::Dump::Tree;
use Physics::Unit::Maths;

our $db = 0;       #debug

#some units have the same dimensions but are different types - type hints steer type inference
our %type-hints = %(
    Area           => <Area FuelConsumption>,
    Energy         => <Energy Torque>,
    Momentum       => <Momentum Impulse>,
    Frequency      => <Frequency Radioactivity>,
    SpecificEnergy => <SpecificEnergy Dose>,
);

##### Constants and Data Maps ######

my $locale = 'imp';        # locale for 'us' or 'imp' gallons, pints, mpg etc.
$locale = ($_ ~~ /en_US/) ?? 'us' !! 'imp'   with %*ENV<LANG>;
$locale = ($_ ~~ /en_US/) ?? 'us' !! $locale with %*ENV<RAKULANG>; 

constant \preload = 0;		#Preload All Units ie. for debug (precomp load 1.6s otherwise ~60s)
constant \NumBases = 8;

#Power synonyms
my %pwr-preword   = ( square  => 2, sq => 2, cubic => 3, cu => 3 );
my %pwr-postword  = ( squared => 2, cubed => 3, );

#Power superscripts eg. x¹ x² x³ x⁴ x⁻¹ x⁻² x⁻³ x⁻⁴
my %pwr-superscript = (
     '¹' =>  1,  '²' =>  2,  '³' =>  3,  '⁴' =>  4,
    '⁻¹' => -1, '⁻²' => -2, '⁻³' => -3, '⁻⁴' => -4,
);

#-------------------------- NEW SHIT

# todo
# externailze all but Unit
# appenders
# more accessors -> submethods?
# FIXME s

class Unit { ... }
class Dictionary { ... }



class Unit does Physics::Unit::Maths[Unit] is export {
    has $.dictionary = Dictionary.instance;

    has Real    $!factor = 1;
    has Real    $!offset = 0;
    has Str()   $!defn   = '';
    has Str     $!type;
    has Str     @!names  = [];
    has Int     @.dims = 0 xx NumBases;
    has MixHash $.dmix is rw = ∅.MixHash;

    ### accessor methods ###
    # i.e. use 'self.attr: 42' not 'self.attr = 42'
    multi method factor($f) { $!factor = $f }
    multi method factor     { $!factor }

    multi method offset($o) { $!offset = $o }
    multi method offset     { $!offset }

    multi method defn($d)   { $!defn = $d }
    multi method defn       { $!defn }

    multi method type($t)   { $!type = $t }
    multi method type {

        #1 type has been set
        #eg. on Prefix.load or explicitly to avoid ambiguous state
        return when $!type;

        #2 check if name (if set) is a prototype
        with $.name {
            for $.dictionary.type-to-protoname.kv -> $k, $v {
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
                for $.dictionary.type-to-dims.kv -> $key, $value {
                    take $key if $value eqv self.dims
                }
            }
        )

    }

    #iamerejh -> symbol?
    method name             { @!names.first || '' }




    ### new & clone methods ###

    method load-names( @new-names ) {

        #iamerejh refactor get-syns
        if @new-names.so {
            if $.dictionary.get-syns(name => @new-names[0]) -> @syns {
                #predefined Unit, assign synonyms
                @!names = @syns;

            } else {
                #user defined Unit, assign names provided
                @!names = @new-names;
            }
        } else {
            #lookup defn in the postfix synonyms
            for $.dictionary.postsyns-by-name.kv -> $k, $v {
                if $v.grep($!defn) {
                    @!names = @$v;
                }
            }
            #otherwise, just assign defn
            @!names = [$!defn] unless @!names;
        }

        @!names.map( { $.dictionary.defn-by-name{$_} = $!defn } );
        @!names.map( { $.dictionary.unit-by-name{$_} =   self } );

        say "load-names: {@!names}" if $db;
    }

    #new by partial named arguments
    multi method new( :$defn!, :@names ) {
        my $n = CreateUnit( $defn );
        $n.load-names: @names;
        return $n
    }

    #new by deep cloning an existing Unit
    method clone {
        nextwith :names([]), :dims(@!dims.clone), :dmix($!dmix.clone);

    }
    multi method new( Unit:D $u: @names ) {
        my $n = $u.clone;
        $n.load-names: @names;
        return $n
    }

    #| role Maths uses cloned Units to avoid grammar
    #| we need to clear all but dims and dmix
    submethod clear {
        $!defn = Nil;
        $!type = Nil;
        @!names = [];
    }

    ### behavioural methods ###

    #| Manually attach NewType when no preset type, eg. m-1
    #| FIXME - put in Type class (reverse args)
    method NewType( Str $type-name ) {
        for @!names -> $name {
            $.dictionary.type-to-protoname{$type-name} = $name;
        }
        $.dictionary.type-to-prototype{$type-name} = self;
        $.dictionary.type-to-dims{$type-name} = self.dims;
    }

    ### output methods ###
    method Str  { self.name }
    method gist { self.Str }

    method canonical {
        #reset to SI base names
        my ( $ds, @dim-str );
        for 0 ..^ NumBases -> $i {
            given @.dims[$i] {
                when 0  { $ds = '' }
                when 1  { $ds = "{$.dictionary.basenames[$i]}" }
                default { $ds = "{$.dictionary.basenames[$i]}$_" }
            }
            @dim-str.push: $ds if $ds;
        }
        return @dim-str.join('.')
    }
    method pretty {
        #following SI recommendation
        my %pwr-sup-rev = %pwr-superscript.kv.reverse;
        my ( $ds, @dim-str );
        for 0 ..^ NumBases -> $i {
            given @.dims[$i] {
                when 0  { $ds = '' }
                when 1  { $ds = "{$.dictionary.basenames[$i]}" }
                default { $ds = "{$.dictionary.basenames[$i]}%pwr-sup-rev{$_}" }
            }
            @dim-str.push: $ds if $ds;
        }
        return @dim-str.join('⋅')
    }
    method raku {
        return qq:to/END/;
          Unit.new( factor => $!factor, offset => $!offset, defn => '$!defn', type => {$.type},
          dims => [{@!dims.join(',')}], dmix => {$!dmix.raku}, names => [{@!names.map( ->$n {"'$n'"}).join(',')}] );
        END
  }

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

        if not preload {
            #load data map hashes only - instantiate Unit objects on demand

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
                    $.dictionary.defn-by-name{$name} = $defn;
                    $.dictionary.syns-by-name{$name} = @synonyms;
                }
            }
        } else {
            #instantiate all Units right away (slow)   #FIXME - wrong anyway

            for @a -> %h {
                my ($defn, $names) = %h<defn>, %h<names>;

                Unit.new(defn => $defn, names => [|$names]);
            }
        }
    }

    #iamerejh
    method get-prototype( $t ) {
        GetPrototype( $t );
    }

    method get-unit( $d ) {
        GetUnit( $d );
    }
}

class Unit::Base {
    has $.dictionary = Dictionary.instance;

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
            @synonyms.map({ $.dictionary.syns-by-name{$_} = |@synonyms });

            my $u = Unit.new;
            $u.load-names: @synonyms;
            $u.defn: $u.name;

            #dimension vector has zeros in all but one place
            $u.dims[$i++] = 1;
            $u.dmix{$u.name} = 1;
            $u.type: $type;

            $.dictionary.type-to-protoname{$type} = $u.name;
            $.dictionary.type-to-prototype{$type} = $u;

            $.dictionary.basenames.push: $u.name;
            
            $.dictionary.postfix-by-name{$u.name} = @synonyms[1];    #extended name as value
            $.dictionary.postsyns-by-name{$u.name} = @synonyms;       #all synonyms as value

            $.dictionary.postfix-by-name;
            say "Initialized Base $names[0]" if $db;
        }
    }
}

class Unit::Type {
    has $.dictionary = Dictionary.instance;

    method load( @a ) {
        for @a -> %h {
            $.dictionary.type-to-protoname{%h.keys} = %h.values;
        }
    }
}

class Unit::Dims {
    has $.dictionary = Dictionary.instance;

    method load( @a ) {
        for @a -> %h {
            $.dictionary.type-to-dims{%h.keys} = %h.values;
        }
    }
}

class Unit::Derived is Unit {

    method load( %config ) {
        my @a = |%config<Derived>;

        for @a -> %h {
            my ($defn, $names) = %h<defn>, %h<names>;

            my @synonyms = |$names;

            $.dictionary.postfix-by-name{@synonyms[0]} = @synonyms[1];
            $.dictionary.postsyns-by-name{@synonyms[0]} = @synonyms;
        }

        callsame
    }
}

class Unit::Prefix is Unit {

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

            $.dictionary.prefix-by-name{$name} = $u;
            $.dictionary.prefix-by-code{$code} = $u;
            $.dictionary.prefix-to-factor{$name} = %h<defn>;

            say "Initialized Prefix $name" if $db;
        }
    }
}

class Unit::Postfix {
    has $.dictionary = Dictionary.instance;

    #Load SI Prefix code / Unit combos to data map hashes for postfix operators
    method load {
        # so far %postfix-by-name has been initialized with base and derived unit names
        # redo THIS!!! ^^^

        # replace kg with g
        $.dictionary.postfix-by-name<kg>:delete;
        $.dictionary.postsyns-by-name<kg>:delete;
        $.dictionary.postfix-by-name<g> = 'gram';
        $.dictionary.postsyns-by-name<g> = <g gram grams gramme grammes>;

        # delete non-declining singletons from %postfix-by-name so that they do not generate unwanted postfixes
        # leave them in %postfix-syns-by-name as we will want the syns for the singletons in do-postfix
        #$.dictionary.postfix-by-name<°>:delete;   (Angle does not make it to %postfix-by-name)
        $.dictionary.postfix-by-name<°C>:delete;
        $.dictionary.postfix-by-name<radian>:delete;
        $.dictionary.postfix-by-name<steradian>:delete;

        # Angle does not make it to %postfix-syns-by-name ?!
        $.dictionary.postsyns-by-name<°> = <° degree degrees deg degs º>;

        # pour in 'l' ie. ml, cl, etc quite common
        $.dictionary.postfix-by-name<l> = 'litre';
        $.dictionary.postsyns-by-name<l> = <l L litre litres liter liters>;

        # now %postfix-by-name has the right simple-names
        # so now can copy these across and us them to spin up all the combos
        my %simple-names = $.dictionary.postfix-by-name;

        for %simple-names.keys -> $n {
            for $.dictionary.prefix-by-code.kv -> $c, $p {

                # combine short keys and values, then extend both codes & names to decongest namespace
                my $combo = $c ~ $n;                                                #eg. 'ml' (used by custom Postfix op)
                $.dictionary.postfix-by-name{$combo} = $.dictionary.prefix-by-code{$c} ~ %simple-names{$n};   #eg. 'millilitres' (used by Grammar)

                # set up synonym list for population of Unit object name
                my $syns = $.dictionary.postsyns-by-name{$n};
                $syns = [ $p X~ @$syns ];   # using @$ to prevent ~ from stringifying the whole array
                $syns.shift;                # drop eg. 'millil'
                $syns.unshift: $combo;      # insert eg. 'ml'

                $.dictionary.postsyns-by-name{$combo} = $syns;
            }
        }
    }
}

class Dictionary {
    ### Singleton Behaviour ###
    my Dictionary $instance;

    method new {!!!}

    method instance {
        unless $instance {
            $instance = Dictionary.bless;
            $instance.load;
        }
        $instance;
    }

    ### Attributes ###
    has @.basenames;
    
    has %.prefix-by-name;       #name => Prefix object
    has %.prefix-by-code;       #code => Prefix name
    has %.prefix-to-factor;     #name => Prefix factor

    has %.defn-by-name;         #name => defn Str of known names incl. postfix (values may be dupes)
    has %.syns-by-name;         #name => list of synonyms (excl. user defined, incl. plurals)
    has %.unit-by-name;         #name => Unit object cache (when instantiated)

    has %.type-to-protoname;    #type => prototype name
    has %.type-to-prototype;    #type => prototype Unit object (when instantiated)
    has Array[Int]() %.type-to-dims;		    #type => dims vector

    has %.postfix-by-name;      #name => extended postfix defn (eg. cm => 'centimetre') to decongest Grammar namespace
    has %.postsyns-by-name;     #name => list of synonyms for every postfix [n, nano] X~ [m, metre, meter, metres, meters]

    submethod load {
        # FIXME - load general config & inject to loader

        require Physics::Unit::Definitions::en_SI;
        my $load = Physics::Unit::Definitions::en_SI.new;

        # core type info
        Unit::Base.new.load:    $load.config<base>;
        Unit::Type.new.load:    $load.config<types>;
        Unit::Dims.new.load:    $load.config<dims>;
        
        # unit children
        Unit::Derived.new.load: $load.config<derived>;
        Unit::Prefix.new.load:  $load.config<prefix>;
        
        # prep for postfix exports
        Unit::Postfix.new.load;

        # load dictionary for non-core units
        Unit.new.load:          $load.config<units>;

        if $db {
            say "+++++++++++++++++++";
            say "INITIALIZATION DONE";
            say "+++++++++++++++++++";
            say "";
        }
    }

    method get-prefix(:$name) {       # type as Name?
        %!prefix-by-name{$name}
    }

    method all-prefixes {
        %!prefix-by-name.keys.join('|')
    }

    method get-syns(:$name) {       # type as Name?
        %!syns-by-name{$name}
    }

    # FIXME need methods for
    # type-to-prototype
    # type-to-protoname

    #    method defn(Name $n --> Defn) {      #getter
    #        %!defn-by-name{$n}
    #    }
}


######## Subroutines (Exported) ########
sub ListSyns is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    $dictionary.syns-by-name;
    #	return sort keys $dictionary.defn-by-name;
}
sub ListDefns is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    $dictionary.defn-by-name;
#	return sort keys $dictionary.defn-by-name;
}
sub ListUnits is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    return sort keys $dictionary.defn-by-name;
}
sub ListTypeNames is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    $dictionary.type-to-protoname
#    return sort keys $dictionary.type-to-protoname;
}
sub ListPrototypes is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    $dictionary.type-to-prototype
#    return sort keys $dictionary.type-to-prototype;
}
sub ListBases is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    return $dictionary.basenames;
}
sub GetPrefixToFactor is export {
    my $dictionary := Dictionary.instance;

    return $dictionary.prefix-to-factor;
}
sub GetPostfixByName is export {
    my $dictionary := Dictionary.instance;

    return $dictionary.postfix-by-name;
}
sub GetPostfixSynsByName is export {
    my $dictionary := Dictionary.instance;

    return $dictionary.postsyns-by-name;
}
sub GetPrototype( Str $type ) is export {     # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    if my $pt = $dictionary.type-to-prototype{$type} {
		return $pt;
	} else {
		for $dictionary.type-to-protoname -> %p {
			return GetUnit(%p.value) if %p.key eq $type;
		}
	}
}
sub GetUnit( $u ) is export {     # FIXME make Unit class method (revert to $!dictionary)
  my $dictionary := Dictionary.instance;

  #1 if Unit, eg. from Measure.new( ... unit => $u ), just return it
  say "GU1 from $u" if $db;
  if $u ~~ Unit {
    return $u
  }

  #2 if name or prefix already instantiated
  say "GU2 from $u" if $db;

  return $dictionary.unit-by-name{$u} if $dictionary.unit-by-name{$u}.defined;
  return $dictionary.get-prefix(:name($u)) if $dictionary.get-prefix(:name($u)).defined;

  #3 if name in our defns, instantiate it
  say "GU3 from $u" if $db;

  for $dictionary.defn-by-name -> %p {
    if %p.key.grep($u) {
      my $nuo = Unit.new( defn => %p.value, names => [%p.key] );
      return $nuo;
    }
  }

  #4 if no match, instantiate new Unit object from definition
  say "GU4 from $u" if $db;

  my $nuo = Unit.new( defn => $u );
  return subst-shortest( $nuo ) // $nuo;
}

######## Subroutines (Internal) ########

sub subst-shortest( Unit $u ) {
    my $dictionary = Dictionary.instance;

    # substitutes shortest name if >1 unit name has same dimensions
    # ... so that eg. 'J' beats 'kg m^2 / s^2'
    # ... requires eg. 'J' to be instantiated first

    my @same-dims;
    for $dictionary.unit-by-name.kv -> $k,$v {
        @same-dims.push($k) if $v.same-dims($u)
    }
    if @same-dims {
        my @sort-by-size = @same-dims.sort({$^a.chars cmp $^b.chars});
        return $dictionary.unit-by-name{@sort-by-size[0]}  #shortest
    }
}

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

######## Grammars ########

sub CreateUnit( $defn is copy ) {       # FIXME make Unit::Definition.parse class method
	#6.d faster regexes with Strings {<$str>} & slower with Arrays {<@arr>}
    my $dictionary := Dictionary.instance;

    $defn .= trim;
    $defn .= subst('%LOCALE%', $locale);

	#| preprocess postfix units to extended defn - eg. cm to centimetre
	$defn = $dictionary.postfix-by-name{$defn} // $defn;

    #| rm compound names from element unit-name match candidates (to force regen of dmix)
    my $unit-names       = $dictionary.defn-by-name.keys.grep({! /<[\s*^./]>/}).join('|');

    my $prefix-names     = $dictionary.all-prefixes;

    my $pwr-prewords     = %pwr-preword.keys.join('|');
    my $pwr-postwords    = %pwr-postword.keys.join('|');
    my $pwr-superscripts = %pwr-superscript.keys.join('|');

    #escape quote non alphanum| charactersi...
    $unit-names       ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;
    $pwr-superscripts ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;

#    use Grammar::Tracer;
    grammar UnitGrammar {
      token TOP         { ^  \s* <numerator=.compound>
                            [\s* <divider> \s* <denominator=.compound>]?
                            [\s* '+' \s* <offset>  ]? \s* $	             }  #offset '+' hardwired
      token divider     { '/' || 'per' }
      token compound    { <element>+ % <sep> }
      token sep         { [ '*' || '.' || ' *' || ' .' || ' ' || '⋅' ] }
      token element     { <factor> || <pnp-before> || <pnp-after> }

      token factor      { <number> }
      token offset      { <number> }
      token number      { \S+ <?{ defined +"$/" }> } #get chars, assert coerce to Real via +

      token pnp-before  { <pwr-before>  \s+? <prefix-name> } #pnp==prefix-name-power
      token pnp-after   { <prefix-name> \s*? <pwr-after>?  }

      token prefix-name { <prefix>? \s*? <name> }
      token prefix      { <$prefix-names> }
      token name        { <$unit-names>   }

      token pwr-before  { <$pwr-prewords> }
      token pwr-after   { <pwr-postwd> || <pwr-supers> || <pwr-normal> }
      token pwr-postwd  { <$pwr-postwords>    }
      token pwr-supers  { <$pwr-superscripts> }

      token pwr-normal  { <pwr-symbol>? \s*? <pwr-digits> }
      token pwr-symbol  { '**' || '^' }
      token pwr-digits  { <[-+]>? <[1..4]> }
    }

    class UnitActions   {
		##say "in xxx...", $/.made;  #<== handy debug line, paste just after make

      #| assemble result with math operations from numerator and denominator (&offset)
      method TOP($/)			{
        my $nu = $<numerator>.made;
        my $de = $<denominator>.made;
        my $os = $<offset>.made;
        $nu.share($de) if $de;
        $nu.offset: +$os if $os;
        make $nu;
      }

      #| accumulates element Units using times
      method compound($/)		{
        my $acc = Unit.new;
        for $<element>>>.made -> $x {
          $acc.times($x);
        }
        make $acc;
      }

      #| makes a list of element units (either factor or prefix-name-power)
      method element($/)		{
        my ( $unit, $defn, $pwr );

        if $unit = $<factor>.made {
          make $unit;
        } else {
          $unit = $<pnp-before><prefix-name>.made<unit> ||
              $<pnp-after><prefix-name>.made<unit>;
          $defn = $<pnp-before><prefix-name>.made<defn> ||
              $<pnp-after><prefix-name>.made<defn>;
          $pwr  = $<pnp-before><pwr-before>.made ||
              $<pnp-after>.made || 1;
          make $unit.raise($pwr, $defn);
        }
      }

      #| handle factor and offset matches
      method factor($/)		{
        make Unit.new.times($/.Real);
      }
      method offset($/) {
        make $<number>;
      }

      #| make both unit and defn from prefix-name matches
      method prefix-name($/) {
        my $unit = $<name>.made<unit>;
        my $defn = $<name>.made<defn>;
        my $pfix = $<prefix>.made<unit>;
        $unit.times($pfix) if $pfix;
        make %( defn => $defn, unit => $unit );
      }
      method prefix($/)	{
        make %( unit => GetUnit($/.Str).clone );
      }
      method name($/) {
        my $defn=$/.Str;
        my $unit=GetUnit($defn).clone;
        $unit.dmix=∅.MixHash;
        make %( defn => $defn, unit => $unit );
      }

      #| extract (signed) power digits from various grammar options
      method pwr-before($/)	{
        make %pwr-preword{$/.Str};
      }
      method pnp-after($/)	{
        make $<pwr-after><pwr-postwd>.made ||
             $<pwr-after><pwr-supers>.made ||
             $<pwr-after><pwr-normal>.made;
      }
      method pwr-postwd($/)	{
        make %pwr-postword{$/.Str}.Int;
      }
      method pwr-supers($/)	{
        make %pwr-superscript{$/.Str}.Int;
      }
      method pwr-normal($/)	{
        make $<pwr-digits>.Int;
      }
    }

    my $match = UnitGrammar.parse( $defn, :actions(UnitActions) );

    if $match.so {
      say "Made: $match\t= ", $match.made if $db;

      my $made-unit = $match.made;
      $made-unit.defn: $defn;
      return $made-unit;
    } else {
      die "Couldn't parse defn Str $defn";
    }
}

#EOF
