unit module Physics::Unit:ver<1.1.26>:auth<Steve Roe (librasteve@furnival.net)>; #viz. https://en.wikipedia.org/wiki/International_System_of_Units

use Data::Dump::Tree;

my $db = 0;               #debug

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
#my Str @BaseNames;			#SI Base Unit names

#Power synonyms
my %pwr-preword   = ( square  => 2, sq => 2, cubic => 3, cu => 3 );
my %pwr-postword  = ( squared => 2, cubed => 3, );

#Power superscripts eg. x¹ x² x³ x⁴ x⁻¹ x⁻² x⁻³ x⁻⁴
my %pwr-superscript = (
     '¹' =>  1,  '²' =>  2,  '³' =>  3,  '⁴' =>  4,
    '⁻¹' => -1, '⁻²' => -2, '⁻³' => -3, '⁻⁴' => -4,
);

#my %defn-by-name;         #name => defn Str of known names incl. affix (values may be dupes)
#my %syns-by-name; 	       #name => list of synonyms (excl. user defined, incl. plurals)
#my %unit-by-name;         #name => Unit object cache (when instantiated)

my %affix-by-name;        #name => extended affix defn (eg. cm => 'centimetre') to decongest Grammar namespace
my %asyns-by-name;        #name => list of synonyms for every affix [n, nano] X~ [m, metre, meter, metres, meters]

#my %type-to-protoname;    #type => prototype name
#my %type-to-prototype;    #type => prototype Unit object (when instantiated)
my %type-to-dims;		  #type => dims vector

my %odd-type-by-name;     #mop up a few exceptional types   # FIXME

#-------------------------- NEW SHIT

# todo
# 1 interpose Dictionary service
#   -prefix base derived (affix) types dims (odd) units
# externailze all but Unit
# drop ##s
# appenders
# FIXME s

class Unit { ... }
class Dictionary { ... }

######## Classes & Roles ########

role Maths {
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
        my $u = GetUnit($d).clone;
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
        my $t = $x.type( :just1 );		    #occasionally can be > one type
        my $p = GetPrototype( $t );
        return( $t, $p )
    }
    method divide( Unit $r ) {
        my $l = self.clone;
        my $x = $l.share( $r );
        my $t = $x.type( :just1 );		    #occasionally can be > one type
        my $p = GetPrototype( $t );
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

        my $t = $l.type( :just1 );		    #occasionally can be > one type
        my $p = GetPrototype( $t );
        return( $t, $p )
    }
}

class Unit does Maths is export {
    has $.dictionary = Dictionary.instance;

    has Real $!factor = 1;
    has Real $!offset = 0;				#ie. for K <=> °C
    has Str  $!defn   = '';
    has Str  $!type;
    has Str  @.names is rw = [];
    has Int  @.dims = 0 xx NumBases;
    has MixHash $.dmix is rw = ∅.MixHash;

    ### accessor methods ###		    #use 'self.attr: 42' not 'self.attr = 42'
    multi method factor($f) { self.CheckChange; $!factor = $f }
    multi method factor     { $!factor }

    multi method offset($o) { self.CheckChange; $!offset = $o }
    multi method offset     { $!offset }

    multi method defn($d)   { $!defn = $d.Str }
    multi method defn       { $!defn }

    multi method type($t)   { $!type = $t.Str }
    multi method type(:$just1) {

        #1 type has been explicitly set ... eg. prefix or to avoid ambiguous state
        return $!type if $!type;

        #2 by looking up dims
        my @d;
        for %type-to-dims.keys -> $k {
            push @d, $k if self.dims cmp %type-to-dims{$k} ~~ Same;
        }
        if @d == 0 { return '' }
        if @d == 1 { return @d[0] }
        if $just1  { return type-hint(@d) // die 'Cannot resolve to just1 type, please set one in %type-hint' }
        if @d > 1  { return @d.sort }
    }

    ### new & clone methods ###

    #new by named arguments
    multi method new( :$defn!, :@names ) {
        my $n = CreateUnit( $defn );
        $n.SetNames: @names;
        $n.SetType();
        return $n
    }

    #new by deep cloning an existing Unit
    method clone {
        nextwith :names([]), :type(''), :dims(@!dims.clone), :dmix($!dmix.clone);
    }
    multi method new( Unit:D $u: @names ) {
        my $n = $u.clone;
        $n.SetNames: @names;
        $n.SetType();
        return $n
    }

    #| Manually make NewType when no preset type, eg. m-1
    method NewType( Str $type-name ) {
        for @.names -> $name {
            $!dictionary.type-to-protoname{$type-name} = $name;
        }
        $!dictionary.type-to-prototype{$type-name} = self;
        %type-to-dims{$type-name} = self.dims;
    }

    ### output methods ###
    method Str  { self.name }
    method gist { self.Str }

    multi method name()         { @!names[0] || '' }
    multi method name( Str $n ) { self.SetNames([$n]) }

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
        my $t-str = self.type;
        return qq:to/END/;
      Unit.new( factor => $.factor, offset => $.offset, defn => '$.defn', type => $t-str,
      dims => [{@.dims.join(',')}], dmix => {$.dmix.raku}, names => [{@.names.map( ->$n {"'$n'"}).join(',')}] );
    END
  }

    # FIXME meld these with accessors
    # FIXME write dictionary somehow
    ### behavioural methods ###
    method SetNames( @new-names ) {
        if @new-names.so {
            #      if %syns-by-name{@new-names[0]} -> @syns {
            if $!dictionary.get-syns(name => @new-names[0]) -> @syns {
                #predefined Unit, assign synonyms
                @.names = @syns;
            } else {
                #user defined Unit, assign names provided
                @.names = @new-names;
            }
        } else {
            #lookup defn in the affix synonyms
            for %asyns-by-name.kv -> $k, $v {
                if $v.grep($.defn) {
                    @.names = @$v;
                }
            }
            #otherwise, just assign defn
            @.names = [$.defn] unless @.names;
        }
        @.names.map( { $.dictionary.defn-by-name{$_} = self.defn } );
        @.names.map( { $.dictionary.unit-by-name{$_} = self } );

        say "SetNames: {@.names}" if $db;
    }
    method SetType( $t? ) {
        for @.names -> $n {
            #set up this Unit as a prototype
            for $.dictionary.type-to-protoname -> %p {
                if %p.value eq $n {
                    $.type: %p.key;
                    $.dictionary.type-to-prototype{$!type} = self;
                }
            }
            #mop up any odd types
            for %odd-type-by-name -> %p {
                if %p.key eq $n {
                    $.type: %p.value;
                }
            }
        }

        say "SetType: $.type" if $db;
    }
    method CheckChange {
        warn "You're not allowed to change named units!" if self.name;
    }

    multi method load( @a ) {

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
                    say $name;
                    $.dictionary.defn-by-name{$name} = $defn;
                    $.dictionary.syns-by-name{$name} = @synonyms;
                }
#                if $derived {          #FIXME - up to Unit::Derived
#                    %affix-by-name{@synonyms[0]} = @synonyms[1];
#                    %asyns-by-name{@synonyms[0]} = @synonyms;
#                }
            }
        } else {
            #instantiate all Units right away (slow)   #FIXME - wrong anyway

            for @a -> %h {
                my ($defn, $names) = %h<defn>, %h<names>;

                Unit.new(defn => $defn, names => [|$names]);
            }
        }
    }
}


class Unit::Prefix is Unit {
    method load( @a ) {

        for @a -> %h {
            my ( $code, $name ) = %h<names>;

            my $u = Unit.new;
            $u.factor:     %h<defn>;            # FIXME - go 'is built'  ?
            $u.defn:       %h<defn>;
            $u.names.push: $name;
            $u.type:       'prefix';

            $.dictionary.prefix-by-name{$name} = $u;
            $.dictionary.prefix-by-code{$code} = $u;
            $.dictionary.prefix-to-factor{$name} = %h<defn>;

            say "Initialized Prefix $name" if $db;
        }
    }
}

class Unit::Base is Unit {
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
            $u.SetNames: @synonyms;
            $u.defn: $u.name;

            #dimension vector has zeros in all but one place
            $u.dims[$i++] = 1;
            $u.dmix{$u.name} = 1;
            $u.type: $type;

            $.dictionary.type-to-protoname{$type} = $u.name;
            $.dictionary.type-to-prototype{$type} = $u;

            $.dictionary.basenames.push: $u.name;
            
#            %affix-by-name{$u.name} = @synonyms[1];    #extended name as value
#            %asyns-by-name{$u.name} = @synonyms;       #all synonyms as value

            say "Initialized Base $names[0]" if $db;
            #iamerejh
        }
    }
}

class Unit::Derived is Unit {
    method load( @a ) {
        callsame;
    }
}

class Unit::Types {

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
    ###

    has @.basenames;
    
    has %.prefix-by-name;       #name => Prefix object
    has %.prefix-by-code;       #code => Prefix name
    has %.prefix-to-factor;     #name => Prefix factor

    has %.defn-by-name;   #name => defn Str of known names incl. affix (values may be dupes)
    has %.syns-by-name;   #name => list of synonyms (excl. user defined, incl. plurals)
    has %.unit-by-name;   #name => Unit object cache (when instantiated)
    #
    #    has %.affix-by-name;        #name => extended affix defn (eg. cm => 'centimetre') to decongest Grammar namespace
    #    has %.asyns-by-name;        #name => list of synonyms for every affix [n, nano] X~ [m, metre, meter, metres, meters]
    #
    has %.type-to-protoname;    #type => prototype name
    has %.type-to-prototype;    #type => prototype Unit object (when instantiated)
    #    has %.type-to-dims;		  #type => dims vector
    #
    #    has %.odd-type-by-name;     #mop up a few exceptional types

    submethod load {
        # FIXME - load general config & inject to loader

        require Physics::Unit::Definitions::en_SI;
        my $load = Physics::Unit::Definitions::en_SI.new;

        Unit::Prefix.new.load:  $load.config<prefix>;
        Unit::Base.new.load:    $load.config<base>;

        say $load.config<derived>;
        Unit::Derived.new.load: $load.config<derived>;
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

    $dictionary.unit-by-name;
    #	return sort keys $dictionary.defn-by-name;
}
sub ListTypes is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    $dictionary.type-to-protoname
#    return sort keys $dictionary.type-to-protoname;
}
sub ListBases is export {       # FIXME make Unit class method (revert to $!dictionary)
    my $dictionary := Dictionary.instance;

    return $dictionary.basenames;
}
sub GetPrefixToFactor is export {
    my $dictionary := Dictionary.instance;

    return $dictionary.prefix-to-factor;
}
sub GetAffixByName is export {
	return %affix-by-name;
}
sub GetAffixSynsByName is export {
  return %asyns-by-name;
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

sub CreateUnit( $defn is copy ) {       # FIXME make Unit class method
	#6.d faster regexes with Strings {<$str>} & slower with Arrays {<@arr>}
    my $dictionary := Dictionary.instance;

    $defn .= trim;

	#| preprocess affix units to extended defn - eg. cm to centimetre
	$defn = %affix-by-name{$defn} // $defn;

    #| rm compound names from element unit-name match candidates (to force regen of dmix)
    my $unit-names       = $dictionary.defn-by-name.keys.grep({! /<[\s*^./]>/}).join('|');

    my $prefix-names     = $dictionary.all-prefixes;

    my $pwr-prewords     = %pwr-preword.keys.join('|');
    my $pwr-postwords    = %pwr-postword.keys.join('|');
    my $pwr-superscripts = %pwr-superscript.keys.join('|');

    #escape quote non alphanum| charactersi...
    $unit-names       ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;
    $pwr-superscripts ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;

    ##use Grammar::Tracer;
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
      method prefix-name($/)	{
        my $unit = $<name>.made<unit>;
        my $defn = $<name>.made<defn>;
        my $pfix = $<prefix>.made<unit>;
        $unit.times($pfix) if $pfix;
        make %( defn => $defn, unit => $unit );
      }
      method prefix($/)		{
        make %( unit => GetUnit($/.Str).clone );
      }
      method name($/)			{
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

######## Initialization ########

#sub InitDerivedUnit( @_ ) {
#    InitUnit( @_, :derived )
#}

sub InitTypes( @_ )  {
    my $dictionary = Dictionary.instance;

    for @_ -> %p {
        $dictionary.type-to-protoname{%p.key} = %p.value;
    }
}

sub InitTypeDims( @_ ) {
    for @_ -> %p {
        %type-to-dims{%p.key} = %p.value;
    }
}

sub InitAffixUnit {
    my $dictionary := Dictionary.instance;

    # so far %affix-by-name has been initialized with base and derived unit names
    # redo THIS!!! ^^^

	# replace kg with g
	%affix-by-name<kg>:delete;
    %asyns-by-name<kg>:delete;
	%affix-by-name<g> = 'gram';
    %asyns-by-name<g> = <g gram grams gramme grammes>;

	# delete non-declining singletons from %affix-by-name so that they do not generate unwanted postfixes
    # leave them in %affix-syns-by-name as we will want the syns for the singletons in do-postfix
	#%affix-by-name<°>:delete;   (Angle does not make it to %affix-by-name)
	%affix-by-name<°C>:delete;
	%affix-by-name<radian>:delete;
	%affix-by-name<steradian>:delete;

    # Angle does not make it to %affix-syns-by-name ?!
    %asyns-by-name<°> = <° degree degrees deg degs º>;

	# pour in 'l' ie. ml, cl, etc quite common
	%affix-by-name<l> = 'litre';
	%asyns-by-name<l> = <l L litre litres liter liters>;

    # now %affix-by-name has the right simple-names
    # so now can copy these across and us them to spin up all the combos
	my %simple-names = %affix-by-name;

	for %simple-names.keys -> $n {
		for $dictionary.prefix-by-code.kv -> $c, $p {

			# combine short keys and values, then extend both codes & names to decongest namespace
			my $combo = $c ~ $n;                                                #eg. 'ml' (used by custom Postfix op)
			%affix-by-name{$combo} = $dictionary.prefix-by-code{$c} ~ %simple-names{$n};   #eg. 'millilitres' (used by Grammar)

            # set up synonym list for population of Unit object name
            my $syns = %asyns-by-name{$n};
            $syns = [ $p X~ @$syns ];   # using @$ to prevent ~ from stringifying the whole array
            $syns.shift;                # drop eg. 'millil'
            $syns.unshift: $combo;      # insert eg. 'ml'

            %asyns-by-name{$combo} = $syns;
		}
	}
}

sub InitUnit( @_ , :$derived ) is export {
    my $dictionary = Dictionary.instance;

	#eg. ['N',  'newton'],           'kg m / s^2',
	#      |     ^^^^^^ synonyms      ^^^^^^^^^^ defn
	#	   |
	#	   > canonical name

	if not preload {
		#load data map hashes only - instantiate Unit objects on demand

		#| iterate over each unit line
		for @_ -> $names, $defn {
			my @synonyms = |$names;

			#| for each name (ie. synonym)
			for |$names -> $singular {
				if naive-plural( $singular ) -> $plural {
					@synonyms.push: $plural;
				}
			}
			for @synonyms -> $name {
				$dictionary.defn-by-name{$name} = $defn;
				$dictionary.syns-by-name{$name} = @synonyms;
			}
			if $derived {
				%affix-by-name{@synonyms[0]} = @synonyms[1];
                %asyns-by-name{@synonyms[0]} = @synonyms;
			}
		}
	} else {
		#instantiate all Units right away (slow)   #FIXME - wrong anyway
		for @_ -> $names, $defn {
            Unit.new( defn => $defn, names => [|$names] );
		}
	}
}

######## Unit Data ########

#InitDerivedUnit (
#	#SI Derived Units with special names & symbols
#	['sr', 'steradian'],                    'radian^2',
#	['Hz', 'hertz'],                        '1 / s',
#	['N',  'newton'],                       'kg m / s^2',
#	['Pa', 'pascal'],                       'N / m^2',
#	['J',  'joule'],                        'kg m^2 / s^2',
#	['W',  'watt'],                         'kg m^2 / s^3',
#	['C',  'coulomb'],                      'A s',
#	['V',  'volt'],                         'kg m^2 / A s^3',
#	['F',  'farad'],                        'A^2 s^4 / kg m^2',
#	['Ω',  'ohm'],                          'kg m^2 / A^2 s^3',
#	['S',  'siemens'],                      'A^2 s^3 / kg m^2',
#	['Wb', 'weber'],                        'kg m^2 / A s^2',
#	['T',  'tesla'],                        'kg / A s^2',
#	['H',  'henry'],                        'kg m^2 / A^2 s^2',
#	['°C', 'celsius', 'Centigrade'],        'K + 273.15',
#	['lm', 'lumen'],                        'cd sr',
#	['lx', 'lux'],                          'm^-2 cd',
#	['Bq', 'becquerel'],                    '1 Hz',
#	['Gy', 'gray'],                         'J / kg',
#	['Sv', 'sievert'],                      'J / kg',
#	['kat','katal'],                        'mol s^-1',
#);

InitTypes (
	#sets name of prototype unit
    'Dimensionless'      => 'unity',
    'Angle'              => 'radian',
    'AngularSpeed'		 => 'radians per second',
    'SolidAngle'         => 'steradian',
    'Frequency'          => 'hertz',
    'Area'               => 'm^2',
    'Volume'             => 'm^3',
    'Speed'              => 'm/s',
    'Acceleration'       => 'm/s^2',
    'Momentum'           => 'kg m/s',
    'Force'              => 'newton',
    'Torque'             => 'Nm',
    'Impulse'            => 'Ns',
    'MomentOfInertia'    => 'kg m^2',
    'AngularMomentum'    => 'kg m^2/s',
    'Pressure'           => 'pascal',
    'Density'			 => 'kg/m^3',
    'Energy'             => 'joule',
    'Power'              => 'watt',
    'Charge'             => 'coulomb',
    'Potential'			 => 'volt',
    'Resistance'         => 'ohm',
    'Conductance'        => 'siemens',
    'Capacitance'        => 'farad',
    'Inductance'         => 'henry',
    'MagneticField'      => 'tesla',
    'MagneticFlux'       => 'weber',
    'LuminousFlux'       => 'lumen',
    'Illuminance'        => 'lux',
    'Radioactivity'      => 'becquerel',
    'Dose'               => 'gray',
    'CatalyticActivity'  => 'kat',
    'FuelConsumption'    => 'm^3/m',
    'FuelEfficiency'     => 'm/m^3',
	'Flow'               => 'm^3/s',
	'SpecificEnergy'     => 'J/kg',
    'Irradiance'         => 'W/m^2',
    'Insolation'         => 'kWh/m^2',
    'ThermalResistance'  => 'Km^2/W',
    'ThermalConductance' => 'W/m^2K'
);

InitTypeDims (
	#viz https://en.wikipedia.org/wiki/Dimensional_analysis#Definition
	#                          (L,M,T,I,Θ,N,J,A)  [A=Angle]
	'Dimensionless'         => (0,0,0,0,0,0,0,0),
	'Length'			    => (1,0,0,0,0,0,0,0),
	'Mass'		            => (0,1,0,0,0,0,0,0),
	'Time'				    => (0,0,1,0,0,0,0,0),
	'Current'			    => (0,0,0,1,0,0,0,0),
	'Temperature'		    => (0,0,0,0,1,0,0,0),
	'Substance'			    => (0,0,0,0,0,1,0,0),
	'Luminosity'		    => (0,0,0,0,0,0,1,0),
	'Angle'			        => (0,0,0,0,0,0,0,1),
	'AngularSpeed'		    => (0,0,-1,0,0,0,0,1),  #FIXME maybe A=0 (see also disambiguate)
	'SolidAngle'		    => (0,0,0,0,0,0,0,2),
	'Frequency'			    => (0,0,-1,0,0,0,0,0),
	'Area'				    => (2,0,0,0,0,0,0,0),
	'Volume'			    => (3,0,0,0,0,0,0,0),
	'Speed'				    => (1,0,-1,0,0,0,0,0),
	'Acceleration'		    => (1,0,-2,0,0,0,0,0),
	'Momentum'			    => (1,1,-1,0,0,0,0,0),
	'Force'				    => (1,1,-2,0,0,0,0,0),
	'Torque'			    => (2,1,-2,0,0,0,0,0),
	'Impulse'			    => (1,1,-1,0,0,0,0,0),
	'MomentOfInertia'	    => (2,1,0,0,0,0,0,0),
	'AngularMomentum'	    => (2,1,-1,0,0,0,0,0),
	'Pressure'			    => (-1,1,-2,0,0,0,0,0),
	'Density'			    => (-3,1,0,0,0,0,0,0),
	'Energy'			    => (2,1,-2,0,0,0,0,0),
	'Power'				    => (2,1,-3,0,0,0,0,0),
	'Charge'			    => (0,0,1,1,0,0,0,0),
	'Potential'			    => (2,1,-3,-1,0,0,0,0),
	'Resistance'		    => (2,1,-3,-2,0,0,0,0),
	'Conductance'		    => (-2,-1,3,2,0,0,0,0),
	'Capacitance'		    => (-2,-1,4,2,0,0,0,0),
	'Inductance'		    => (2,1,-2,-2,0,0,0,0),
	'MagneticField'	        => (0,1,-2,-1,0,0,0,0),
	'MagneticFlux'		    => (2,1,-2,-1,0,0,0,0),
	'LuminousFlux'		    => (0,0,0,0,0,0,1,2),
	'Illuminance'		    => (-2,0,0,0,0,0,1,0),
	'Radioactivity'		    => (0,0,-1,0,0,0,0,0),
	'Dose'				    => (2,0,-2,0,0,0,0,0),
	'CatalyticActivity'     => (0,0,-1,0,0,1,0,0),
	'FuelConsumption'       => (2,0,0,0,0,0,0,0),
	'FuelEfficiency'        => (-2,0,0,0,0,0,0,0),
	'Flow'                  => (3,0,-1,0,0,0,0,0),
	'SpecificEnergy'        => (2,0,-2,0,0,0,0,0),
	'Irradiance'            => (0,1,-3,0,0,0,0,0),
	'Insolation'            => (0,1,-2,0,0,0,0,0),
	'ThermalResistance'     => (0,-1,3,0,1,0,0,0),
	'ThermalConductance'    => (0,1,-3,0,-1,0,0,0),
);

InitAffixUnit;
#Load SI Prefix code / Unit combos to data map hashes for postfix operators

#InitUnit (
#	# Dimensionless
#	['①','(1)','one','unity'],                  '1',  #U-2460 CIRCLED DIGIT ONE
#	['semi','demi','hemi'],                     '1/2',   # FIXME prefix?
#	['%','percent'],                            '1/100',
#	['ABV'],                                    '1',
#	['pi'],                                     '3.1415926535897932385',
#
#	# Angle
#	['°', 'degree', 'deg', 'º'],                'pi radians / 180',
#	['ᵍ', 'gon', 'grad'],                       'pi radians / 200',
#
#	# Solid Angle
#	['deg²'],                                   'deg^2',
#	['sp','spat'],                              '4 pi steradian',
#
#	# Time
#	['min', 'minute'],                          '60 s',
#	['hr', 'hour'],                             '60 min',
#	['day'],                                    '24 hr',
#	['week'],                                   '7 day',
#	['fortnight'],                              '2 week',
#	['yr', 'year'],                             '365.25 day',
#	['month'],                                  'year / 12',    # an average month
#	['century', 'centuries'],                   '100 yr',
#	['millenium', 'millenia'],                  '1000 yr',
#
#	# Frequency
#	['cycle'],                                  '1 Hz',
#	['revolution'],                             '1',
#	['rpm'],                                    'revolution per minute',
#
#	# Length
#	['km'],				                        'kilometre',
#	['fm'],				                        'femtometre',   #for 'MeV.fm'
#	['μ', 'micron'],                            '1e-6 m',
#	['å', 'angstrom'],                          '1e-10 m',
#	['au', 'astronomical-unit'],                '1.49598e11 m',
#	['ly', 'light-year'],                       '9.46e15 m',
#	['parsec'],                                 '3.083e16 m',
#	['ft', 'foot', 'feet'],                     '0.3048 m',
#	['in', 'inch'],                             'ft/12',
#	['yard'],                                   '3 ft',
#	['fathom'],                                 '2 yard',
#	['rod', 'pole', 'perch'],                   '5.5 yard',
#	['furlong'],                                '40 rod',
#	['mile'],                                   '5280 ft',
#	['nmile', 'nautical-mile'],                 '1852 m',
#	['ca', 'cable'],		                    '185.2 m',
#	['pica'],                                   'in/6',	#chosen defn not unique
#	['point'],                                  'pica/12',
#
#	# Area
#	['m^2', 'm2', 'm²'],                        'm^2',
#	['are'],                                    '100 square metre',
#	['hectare'],                                '100 are',
#	['barn'],                                   '1e-28 square metre',
#	['acre'],                                   '43560 square feet',
#
#	# Volume
#	['m^3', 'm3', 'm³'],                        'm^3',
#	['l', 'L', 'litre', 'liter'],               'm^3/1000',
#	['cc'],		                                'cubic centimetre',
#	['bottle'],                                 '750 millilitre',
#	['fluidram'],                               '3.5516 millilitre',
#	['minim'],                                  '0.059194 millilitre',
#	['alcohol-unit'],                           '10 millilitre',            # of pure alcohol
#	# setting Imperial (imp-) or US (us-) from $locale
#	['us-gallon'],                              '3.785411784 litre',
#	['imp-gallon'],                             '4.54609 litre',
#	['gallon'],									"1 {$locale}-gallon",
#	['firkin'],							        '9 gallon',
#	['barrel'],							        '36 gallon',
#	['quart'],                                  'gallon/4',
#	['peck'],                                   '8 quart',
#	['bushel'],                                 '4 peck',
#	['fifth'],                                  'us-gallon/5',
#	['us-pint'],                                'us-gallon/8',
#	['imp-pint'],                               'imp-gallon/8',
#	['pint'],									"1 {$locale}-pint",
#	['cup'],                                    'us-pint/2',
#	['floz', 'fluid-ounce'],                    'cup/8',
#	['gill'],                                   '4 fluid-ounce',
#	['tablespoon', 'tbsp'],                     'fluid-ounce / 2',
#	['teaspoon', 'tsp'],                        'tablespoon / 3',
#	['cord'],                                   '128 cubic feet',
#	['stere'],                                  '1 cubic metre',
#
#	# Speed
#	['m/s'],		                            'm/s',
#	['mph'],                                    'mile per hour',
#	['kph'],                                    'kilometre per hour',
#	['kps'],                                    'kilometre per second',
#	['fps'],                                    'feet per second',
#	['knot'],                                   'nmile per hour',
#
#	# AngularSpeed
#	['radians per second'],			            'Hz',  #the SI unit (radians=1)
#	['revs', 'revolutions per second'],         '2 pi * Hz',
#	['rpm'],							        '60 revs',
#
#	# Acceleration
#	['m/s^2'],                                  'm/s^2',
#	['g0', 'earth-gravity'],                    '9.80665 m/s^2',
#
#	# Mass
#	['g', 'gram', 'gm', 'gramme'],              'kg / 1000',
#	['u', 'atomic-mass-unit', 'Da', 'Dalton'],  '1.6605402e-27 kg',
#	['metric-ton', 'tonne'],                    '1000 kg',
#	['grain'],                                  '0.0648 gm',
#	['lbm', 'pounds-mass'],                     '0.45359237 kg',
#	['oz', 'ounce'],                            'lbm/16',
#	['stone'],                                  '14 lbm',
#	['hundredweight'],                          '100 lbm',
#	['ton', 'short-ton'],                       '2000 lbm',
#	['long-ton'],                               '2240 lbm',
#	['slug'],                                   'lbm g0 s^2/ft',
#	['dram'],                                   'ounce / 16',
#	['troy-pound'],                             '0.373 kg',
#	['troy-ounce'],                             '31.103 gm',
#	['pennyweight'],                            '1.555 gm',
#	['scruple'],                                '1.296 gm',
#	['carat', 'karat'],                         '200 milligram',
#	['j-point'],                                '2 carat',
#
#	# MomentOfInertia
#	['kg m^2'],                                 'kg m^2',
#
#	# Momentum
#	['kg m/s'],                                 'kg m/s',
#	['slug ft/s'],                              'slug feet/s',
#
#	# AngularMomentum
#	['kg m^2/s'],                               'kg m^2 / s',
#
#	# Force
#	['lb', 'lbs', 'pound', 'pound-force'],      'slug foot / s^2',
#	['ounce-force'],                            'pound-force / 16',
#	['dyne'],                                   'gm centimetre / s^2',
#	['gram-force'],                             'gm g0',
#	['kgf'],                                    'kilo gram-force',
#
#	# Torque
#	['Nm', 'newton-metre'],                     'N m',
#	['ft-lb', 'footpound'],                     'foot pound-force',
#
#	# Impulse
#	['Ns'],                                     'N * s',
#	['pound-second'],                           'pound * s',
#
#	# Pressure
#	['bar'],                                    '1e5 pascal',
#	['torr'],                                   '133.322368 pascal',  #(101325 / 760)
#	['psi'],                                    'pound per inch^2',
#	['atm', 'atmosphere'],                      '101325 pascal',
#	['mmHg'],                                   '133.322 pascal',
#
#	# Density
#	['kg/m^3'],                                 'kg / m^3',
#	['°proof'],                                 '923 kg / m^3',
#	#UK metric https://en.wikipedia.org/wiki/Alcohol_proof (US version is just 2x ABV)
#
#	# Energy
#	['eV', 'electron-volt'],                    '1.60217733e-19 joule',
#	['MeV'],                                    'mega electron-volt',
#	['GeV'],                                    'giga electron-volt',
#	['TeV'],                                    'tera electron-volt',
#	['cal', 'calorie'],                         '4.184 joule',
#	['kcal'],                                   'kilocalorie',
#	['btu', 'british-thermal-unit'],            '1055.056 joule',
#	['therm'],                                  '1.0e5 btu',
#	['erg'],                                    '1.0e-7 joule',
#	['kWh'],                                    'kilowatt hour',
#	['Eₕ', 'E_h', 'Ha'],                        '4.3597447222071e-18 joule',    #Hartree energy
#
#	# Power
#	['us-horsepower', 'us-hp'],                 '550 foot pound-force / s',
#	['PS', 'horsepower', 'hp'],                 '75 kg * g0 * m / s',
#
#	# Current (Base Unit)
#
#	# Potential (Derived Unit)
#
#	# Conductance
#	['mho'],                                    '1 / ohm',
#
#	# Capacitance (Derived Unit)
#
#	# Inductance (Derived Unit)
#
#	# Magnetic_flux
#	['Mx', 'maxwell'],                          '1e-8 weber',
#
#	# Magnetic_field
#	['gauss'],                                  '1e-4 tesla',
#
#	# Temperature
#	['°R', 'Rankine'],                          '5/9 * K',
#	['°F', 'Fahrenheit'],                       '5/9 * K + 459.67',
#
#	# Dose
#	['rad'],									'gray / 100',
#	['rem'],									'sievert / 100',
#
#	# FuelConsumption
#	['m^3/m'],                                  'm^3 / metre',
#	['l/100km'],                                '0.00001 litre / metre',
#
#	# FuelEfficiency
#	['m/m^3'],                                  'metre / m^3',
#	['mpg'],                                    'mile / gallon',
#
#	# Flow
#	['m^3/s'],                                  'm^3 / second',
#	['gpd'],                                    'gallon / day',
#
#	# SpecificEnergy
#	['J/kg'],                                   'joule / kg',
#	['MJ/kg'],                                  'mega joule / kg',
#
#	# Irradiance
#	['W/m^2'],                                  'W / m^2',
#
#	# Insolation
#	['kWh/m^2'],                                'kWh / m^2',
#	['Langley'],                                'cal / 10000 m^2',
#
#	# ThermalResistance
#	['Km^2/W'],                                 'K m^2 / W',
#	['tog'],                                    '0.1 K m^2 / W',
#
#	# ThermalConductance
#	['W/m^2K'],                                 'W / m^2 K',
#);
#
#sub InitOddTypes( @_ ) {
#    for @_ -> %p {
#        %odd-type-by-name{%p.key} = %p.value;
#    }
#}
#
## FIXME - avoid exceptions - after yamls
#InitOddTypes (
#    #mop up a few exceptional types
#    'eV'      => 'Energy',
#    'MeV'     => 'Energy',
#    'GeV'     => 'Energy',
#    'TeV'     => 'Energy',
#    'cal'     => 'Energy',
#    'kcal'    => 'Energy',
#    'btu'     => 'Energy',
#    'erg'     => 'Energy',
#    'kWh'     => 'Energy',
#    'ft-lb'   => 'Torque',
#);

if $db {
say "+++++++++++++++++++";
say "INITIALIZATION DONE";
say "+++++++++++++++++++";
say "";
}

#EOF
