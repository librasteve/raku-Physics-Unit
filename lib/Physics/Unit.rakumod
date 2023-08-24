unit module Physics::Unit:ver<1.1.19>:auth<Steve Roe (librasteve@furnival.net)>; #viz. https://en.wikipedia.org/wiki/International_System_of_Units

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
my Str @BaseNames;			#SI Base Unit names

my %prefix-by-name;       #name => Prefix object
my %prefix-by-code;       #code => Prefix name
my %prefix-to-factor;     #name => Prefix factor
my %defn-by-name;         #name => defn Str of known names incl. affix (values may be dupes)
my %syns-by-name; 	      #name => list of synonyms (excl. user defined, incl. plurals)
my %unit-by-name;         #name => Unit object cache (when instantiated)
my %affix-by-name;        #name => extended affix defn (eg. cm => 'centimetre') to decongest Grammar namespace
my %affix-syns-by-name;   #name => list of synonyms for every affix [n, nano] X~ [m, metre, meter, metres, meters]
my %type-to-protoname;    #type => prototype name
my %type-to-prototype;    #type => prototype Unit object (when instantiated)
my %type-to-dims;		  #type => dims vector
my %odd-type-by-name;     #mop up a few exceptional types

#Power synonyms
my %pwr-preword   = ( square  => 2, sq => 2, cubic => 3, cu => 3 );
my %pwr-postword  = ( squared => 2, cubed => 3, );

#Power superscripts eg. x¹ x² x³ x⁴ x⁻¹ x⁻² x⁻³ x⁻⁴
my %pwr-superscript = (
     '¹' =>  1,  '²' =>  2,  '³' =>  3,  '⁴' =>  4,
    '⁻¹' => -1, '⁻²' => -2, '⁻³' => -3, '⁻⁴' => -4,
);

######## Classes & Roles ########

class Unit is export {
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

    #1 type has been explicitly set ... rarely used eg. to avoid ambiguous state
    return $!type   if $!type;

    #2 we are a prefix
    return 'prefix' if %prefix-by-name{self.name};

    #3 by looking up dims
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
			%type-to-protoname{$type-name} = $name;
		}
		%type-to-prototype{$type-name} = self;
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
        when 1  { $ds = "@BaseNames[$i]" }
        default { $ds = "@BaseNames[$i]$_" }
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
          when 1  { $ds = "@BaseNames[$i]" }
          default { $ds = "@BaseNames[$i]%pwr-sup-rev{$_}" }
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

  ### behavioural methods ###
  method SetNames( @new-names ) {
    if @new-names.so {
      if %syns-by-name{@new-names[0]} -> @syns {
        #predefined Unit, assign synonyms
        @.names = @syns;
      } else {
        #user defined Unit, assign names provided
        @.names = @new-names;
      }
    } else {
      #lookup defn in the affix synonyms
      for %affix-syns-by-name.kv -> $k, $v {
        if $v.grep($.defn) {
          @.names = @$v;
        }
      }
      #otherwise, just assign defn
      @.names = [$.defn] unless @.names;
    }
    @.names.map( { %defn-by-name{$_} = self.defn } );
    @.names.map( { %unit-by-name{$_} = self } );

    say "SetNames: {@.names}" if $db;
  }
  method SetType( $t? ) {
    for @.names -> $n {
      #set up this Unit as a prototype
      for %type-to-protoname -> %p {
        if %p.value eq $n {
          $!type = %p.key;
          %type-to-prototype{$!type} = self;
        }
      }
      #mop up any odd types
      for %odd-type-by-name -> %p {
        if %p.key eq $n {
          $!type = %p.value;
        }
      }
    }

    say "SetType: $.type" if $db;
  }
  method CheckChange {
      warn "You're not allowed to change named units!" if self.name;
  }

  ### mathematical mutating Module methods ###
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

    my $e-can = %syns-by-name{$e}[0];		#lookup the canonical name
    self.dmix{$e-can} = $d;
    return self
  }

  #### convert & compare methods ####
  method same-dims( Unit $u ) {
    return 0 unless $u.dmix eqv self.dmix;
    return 0 unless $u.factor == self.factor;
    return 1
  }
  method same-unit( Unit $u ) {
    return 0 unless $u.dims eqv self.dims;
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
  method root-extract( Int $n where 1 <= $n <= 4 ) {
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

######## Subroutines (Exported) ########

sub ListUnits is export {
	return %defn-by-name.keys;
}
sub ListTypes is export {
    return sort keys %type-to-protoname;
}
sub ListBases is export {
    return @BaseNames;
}
sub GetPrefixToFactor is export {
	return %prefix-to-factor;
}
sub GetSynsByName is export {
  return %syns-by-name;
}
sub GetAffixByName is export {
	return %affix-by-name;
}
sub GetAffixSynsByName is export {
  return %affix-syns-by-name;
}
sub GetPrototype( Str $type ) is export {
	if my $pt = %type-to-prototype{$type} {
		return $pt;
	} else {
		for %type-to-protoname -> %p {
			return GetUnit(%p.value) if %p.key eq $type;
		}
	}
}
sub GetUnit( $u ) is export {

  #1 if Unit, eg. from Measure.new( ... unit => $u ), just return it
  say "GU1 from $u" if $db;
  if $u ~~ Unit {
    return $u
  }

  #2 if name or prefix already instantiated
  say "GU2 from $u" if $db;

  return %unit-by-name{$u}   if %unit-by-name{$u}.defined;
  return %prefix-by-name{$u} if %prefix-by-name{$u}.defined;

  #3 if name in our defns, instantiate it
  say "GU3 from $u" if $db;

  for %defn-by-name -> %p {
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
  # substitutes shortest name if >1 unit name has same dimensions
  # ... so that eg. 'J' beats 'kg m^2 / s^2'
  # ... requires eg. 'J' to be instantiated first

  my @same-dims;
  for %unit-by-name.kv -> $k,$v {
    @same-dims.push($k) if $v.same-dims($u)
  }
  if @same-dims {
    my @sort-by-size = @same-dims.sort({$^a.chars cmp $^b.chars});
    return %unit-by-name{@sort-by-size[0]}  #shortest
  }
}

sub type-hint(@t ) {
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

sub CreateUnit( $defn is copy ) {
	#6.d faster regexes with Strings {<$str>} & slower with Arrays {<@arr>}

	$defn .= trim;

	#| preprocess affix units to extended defn - eg. cm to centimetre
	$defn = %affix-by-name{$defn} // $defn;

    #| rm compound names from element unit-name match candidates (to force regen of dmix)
    my $unit-names       = %defn-by-name.keys.grep({! /<[\s*^./]>/}).join('|');

    my $prefix-names     = %prefix-by-name.keys.join('|');
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
                            [\s* '+' \s* <offset>  ]? \s* $	}		#offset '+' hardwired
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
      token pwr-digits  { <[-+]>? <[1..4]> }
      token pwr-symbol  { '**' || '^' }
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

sub InitPrefix( @_ ) {
  for @_ -> $name, $factor {
    my $u = Unit.new;
    $u.factor:     $factor;
    $u.defn:       $factor;
    $u.names.push: $name;
    $u.type:       'prefix';

    %prefix-by-name{$name} = $u;
		%prefix-to-factor{$name} = $factor;

    say "Initialized Prefix $name" if $db;
  }
}
sub InitPrefixCode( @_ ) {
	@_.map( {%prefix-by-code{.key}=.value} );
}
sub InitBaseUnit( @_ ) {
  my $i = 0;

  for @_ -> %h {
    my ($type, $names) = %h.key, %h.value;

    my @synonyms = |$names;

    #| for each name (ie. synonym)
    for |$names -> $singular {
      if naive-plural( $singular ) -> $plural {
        @synonyms.push: $plural;
      }
    }
    @synonyms.map( { %syns-by-name{$_} = |@synonyms } );

    my $u = Unit.new;
    $u.SetNames: @synonyms;
    $u.defn: $u.name;

    #dimension vector has zeros in all but one place
    $u.dims[$i++] = 1;
    $u.dmix{$u.name} = 1;

    $u.type: $type;
    %type-to-protoname{$type} = $u.name;
    %type-to-prototype{$type} = $u;

    @BaseNames.push: $u.name;
    %affix-by-name{$u.name} = @synonyms[1];			#extended name as value
    %affix-syns-by-name{$u.name} = @synonyms;   #all synonyms as value

    say "Initialized Base Unit $names[0]" if $db;
  }
}
sub InitDerivedUnit( @_ ) {
	InitUnit( @_, :derived )
}
sub InitAffixUnit {
	# so far %affix-by-name has been initialized with base and derived unit names

	# replace kg with g
	%affix-by-name<kg>:delete;
  %affix-syns-by-name<kg>:delete;
	%affix-by-name<g> = 'gram';
  %affix-syns-by-name<g> = <g gram grams gramme grammes>;

	# delete non-declining singletons from %affix-by-name so that they do not generate unwanted postfixes
  # leave them in %affix-syns-by-name as we will want the syns for the singletons in do-postfix
	#%affix-by-name<°>:delete;   (Angle does not make it to %affix-by-name)
	%affix-by-name<°C>:delete;
	%affix-by-name<radian>:delete;
	%affix-by-name<steradian>:delete;

  # Angle does not make it to %affix-syns-by-name ?!
  %affix-syns-by-name<°> = <° degree degrees deg degs º>;

	# pour in 'l' ie. ml, cl, etc quite common
	%affix-by-name<l> = 'litre';
	%affix-syns-by-name<l> = <l L litre litres liter liters>;

  # now %affix-by-name has the right simple-names
  # so now can copy these across and us them to spin up all the combos
	my %simple-names = %affix-by-name;

	for %simple-names.keys -> $n {
		for %prefix-by-code.kv -> $c, $p {

			# combine short keys and values, then extend both codes & names to decongest namespace
			my $combo = $c ~ $n;                                                #eg. 'ml' (used by custom Postfix op)
			%affix-by-name{$combo} = %prefix-by-code{$c} ~ %simple-names{$n};   #eg. 'millilitres' (used by Grammar)

      # set up synonym list for population of Unit object name
      my $syns = %affix-syns-by-name{$n};
      $syns = [ $p X~ @$syns ];   # using @$ to prevent ~ from stringifying the whole array
      $syns.shift;                # drop eg. 'millil'
      $syns.unshift: $combo;      # insert eg. 'ml'

      %affix-syns-by-name{$combo} = $syns;
		}
	}
}
sub InitTypes( @_ )  {
  for @_ -> %p {
    %type-to-protoname{%p.key} = %p.value;
  }
}
sub InitTypeDims( @_ ) {
  for @_ -> %p {
		%type-to-dims{%p.key} = %p.value;
	}
}
sub InitOddTypes( @_ ) {
  for @_ -> %p {
    %odd-type-by-name{%p.key} = %p.value;
  }
}
sub InitUnit( @_ , :$derived ) is export {
	#eg. ['N',  'newton'],           'kg m / s^2',
	#      |     ^^^^^^ synonyms      ^^^^^^^^^^ defn
	#	     |
	#	     > canonical name

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
				%defn-by-name{$name} = $defn;
				%syns-by-name{$name} = @synonyms;
			}
			if $derived {
				%affix-by-name{@synonyms[0]} = @synonyms[1];
        %affix-syns-by-name{@synonyms[0]} = @synonyms;
			}
		}
	} else {
		#instantiate all Units right away (slow)
		for @_ -> $names, $defn {
            Unit.new( defn => $defn, names => [|$names] );
		}
	}

}

######## Unit Data ########

InitPrefix (
  #avoid 1e2 format to encourage Rats
  #SI Prefixes
  'deka',    10,
  'deca',    10,
  'hecto',   100,
  'kilo',    1000,
  'mega',    1000000,
  'giga',    1000000000,
  'tera',    1000000000000,
  'peta',    1000000000000000,
  'exa',     1000000000000000000,
  'zetta',   1000000000000000000000,
  'yotta',   1000000000000000000000000,
  'deci',    0.1,
  'centi',   0.01,
  'milli',   0.001,
  'micro',   0.000001,
  'nano',    0.000000001,
  'pico',    0.000000000001,
  'femto',   0.000000000000001,
  'atto',    0.000000000000000001,
  'zepto',   1e-21,
  'yocto',   1e-24,
  #others
  'million', 1_000_000,
  'billion', 1_000_000_000,
  'trillion',1_000_000_000_000,
  'mn',      1_000_000,
  'bn',      1_000_000_000,
  'tn',      1_000_000_000_000,
);
InitPrefixCode (
    #SI Prefix code
    da => 'deka',
    #'deca', ignore this spelling alterative
    h => 'hecto',
    k => 'kilo',
    M => 'mega',
    G => 'giga',
    T => 'tera',
    P => 'peta',
    E => 'exa',
    Z => 'zetta',
    Y => 'yotta',
    d => 'deci',
    c => 'centi',
    m => 'milli',
    μ => 'micro',
    n => 'nano',
    p => 'pico',
    f => 'femto',
    a => 'atto',
    z => 'zepto',
    y => 'yocto',
);
InitBaseUnit (
    #SI Base Units
    #viz https://en.wikipedia.org/wiki/Dimensional_analysis#Definition
    ##FIXME revert to algorithmic plurals
    'Length'      => ['m', 'metre', 'meter',],
    'Mass'        => ['kg', 'kilogram',],
    'Time'        => ['s', 'sec', 'second',],
    'Current'     => ['A', 'amp', 'ampere', 'ampère',],
    'Temperature' => ['K', 'kelvin',],
    'Substance'   => ['mol', 'mole',],
    'Luminosity'  => ['cd', 'candela', 'candle',],
    'Angle'		  => ['radian',],
);
InitDerivedUnit (
	#SI Derived Units with special names & symbols
	['sr', 'steradian'],                    'radian^2',
	['Hz', 'hertz'],                        '1 / s',
	['N',  'newton'],                       'kg m / s^2',
	['Pa', 'pascal'],                       'N / m^2',
	['J',  'joule'],                        'kg m^2 / s^2',
	['W',  'watt'],                         'kg m^2 / s^3',
	['C',  'coulomb'],                      'A s',
	['V',  'volt'],                         'kg m^2 / A s^3',
	['F',  'farad'],                        'A^2 s^4 / kg m^2',
	['Ω',  'ohm'],                          'kg m^2 / A^2 s^3',
	['S',  'siemens'],                      'A^2 s^3 / kg m^2',
	['Wb', 'weber'],                        'kg m^2 / A s^2',
	['T',  'tesla'],                        'kg / A s^2',
	['H',  'henry'],                        'kg m^2 / A^2 s^2',
	['°C', 'celsius', 'Centigrade'],        'K + 273.15',
	['lm', 'lumen'],                        'cd sr',
	['lx', 'lux'],                          'm^-2 cd',
	['Bq', 'becquerel'],                    '1 Hz',
	['Gy', 'gray'],                         'J / kg',
	['Sv', 'sievert'],                      'J / kg',
	['kat','katal'],                        'mol s^-1',
);
InitAffixUnit;
	#Load SI Prefix code / Unit combos to data map hashes for postfix operators
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
	'ConsumptionRate'    => 'm^3/s',
	'SpecificEnergy'     => 'J/kg',
    'Irradiance'         => 'W/m^2',
    'Insolation'         => 'kWh/m^2',
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
	'ConsumptionRate'       => (2,0,0,0,0,0,0,0),
	'SpecificEnergy'        => (3,0,-1,0,0,0,0,0),
	'Irradiance'            => (0,1,-3,0,0,0,0,0),
	'Insolation'            => (0,1,-2,0,0,0,0,0),
);
InitOddTypes (
    #mop up a few exceptional types
    'eV'      => 'Energy',
    'MeV'     => 'Energy',
    'GeV'     => 'Energy',
    'TeV'     => 'Energy',
    'cal'     => 'Energy',
    'kcal'    => 'Energy',
    'btu'     => 'Energy',
    'erg'     => 'Energy',
    'kWh'     => 'Energy',
    'ft-lb'   => 'Torque',
);
InitUnit (
	# Dimensionless
	['①','(1)','one','unity'],                  '1',  #U-2460 CIRCLED DIGIT ONE
	['semi','demi','hemi'],                     '1/2',
	['%','percent'],                            '1/100',
	['ABV'],                                    '1',
	['pi'],                                     '3.1415926535897932385',

	# Angle
	['°', 'degree', 'deg', 'º'],                'pi radians / 180',
	['ᵍ', 'gon', 'grad'],                       'pi radians / 200',

	# Solid Angle
	['deg²'],                                   'deg^2',
	['sp','spat'],                              '4 pi steradians',

	# Time
	['min', 'minute'],                          '60 s',
	['hr', 'hour'],                             '60 min',
	['day'],                                    '24 hr',
	['week'],                                   '7 days',
	['fortnight'],                              '2 week',
	['yr', 'year'],                             '365.25 days',
	['month'],                                  'year / 12',    # an average month
	['century', 'centuries'],                   '100 yr',
	['millenium', 'millenia'],                  '1000 yr',

	# Frequency
	['cycle'],                                  '1 Hz',
	['revolution'],                             '1',
	['rpm'],                                    'revolutions per minute',

	# Length
	['km'],				                        'kilometre',
	['fm'],				                        'femtometre',   #for 'MeV.fm'
	['μ', 'micron'],                            '1e-6 m',
	['å', 'angstrom'],                          '1e-10 m',
	['au', 'astronomical-unit'],                '1.49598e11 m',
	['ly', 'light-year'],                       '9.46e15 m',
	['parsec'],                                 '3.083e16 m',
	['ft', 'foot', 'feet'],                     '0.3048 m',
	['in', 'inch'],                             'ft/12',
	['yard'],                                   '3 ft',
	['fathom'],                                 '2 yards',
	['rod', 'pole', 'perch'],                   '5.5 yards',
	['furlong'],                                '40 rods',
	['mile'],                                   '5280 ft',
	['nmile', 'nautical-mile'],                 '1852 m',
	['ca', 'cable'],		                    '185.2 m',
	['pica'],                                   'in/6',	#chosen defn not unique
	['point'],                                  'pica/12',

	# Area
	['m^2', 'm2', 'm²'],                        'm^2',
	['are'],                                    '100 square metres',
	['hectare'],                                '100 ares',
	['barn'],                                   '1e-28 square metres',
	['acre'],                                   '43560 square feet',

	# Volume
	['m^3', 'm3', 'm³'],                        'm^3',
	['l', 'L', 'litre', 'liter'],               'm^3/1000',
	['cc'],		                                'cubic centimetre',
	['bottle'],                                 '750 millilitre',
	['fluidram'],                               '3.5516 millilitre',
	['minim'],                                  '0.059194 millilitre',
	['alcohol-unit'],                           '10 millilitre',            # of pure alcohol
	# setting Imperial (imp-) or US (us-) from $locale
	['us-gallon'],                              '3.785411784 litre',
	['imp-gallon'],                             '4.54609 litre',
	['gallon'],									"1 {$locale}-gallon",
	['firkin'],							        '9 gallons',
	['barrel'],							        '36 gallons',
	['quart'],                                  'gallon/4',
	['peck'],                                   '8 quarts',
	['bushel'],                                 '4 pecks',
	['fifth'],                                  'us-gallon/5',
	['us-pint'],                                'us-gallon/8',
	['imp-pint'],                               'imp-gallon/8',
	['pint'],									"1 {$locale}-pint",
	['cup'],                                    'us-pint/2',
	['floz', 'fluid-ounce'],                    'cup/8',
	['gill'],                                   '4 fluid-ounces',
	['tablespoon', 'tbsp'],                     'fluid-ounce / 2',
	['teaspoon', 'tsp'],                        'tablespoon / 3',

	# Speed
	['m/s'],		                            'm/s',
	['mph'],                                    'miles per hour',
	['kph'],                                    'kilometre per hour',
	['kps'],                                    'kilometre per second',
	['fps'],                                    'feet per second',
	['knot'],                                   'nmile per hour',

	# AngularSpeed
	['radians per second'],			            'Hz',  #the SI unit (radians=1)
	['revs', 'revolutions per second'],         '2 pi * Hz',
	['rpm'],							        '60 revs',

	# Acceleration
	['m/s^2'],                                  'm/s^2',
	['g0', 'earth-gravity'],                    '9.80665 m/s^2',

	# Mass
	['g', 'gram', 'gm', 'gramme'],              'kg / 1000',
	['u', 'atomic-mass-unit'],                  '1.6605402e-27 kg',
	['metric-ton', 'tonne'],                    '1000 kg',
	['grain'],                                  '0.0648 gm',
	['lbm', 'pounds-mass'],                     '0.45359237 kg',
	['oz', 'ounce'],                            'lbm/16',
	['stone'],                                  '14 lbm',
	['hundredweight'],                          '100 lbm',
	['ton', 'short-ton'],                       '2000 lbm',
	['long-ton'],                               '2240 lbm',
	['slug'],                                   'lbm g0 s^2/ft',
	['dram'],                                   'ounce / 16',
	['troy-pound'],                             '0.373 kg',
	['troy-ounce'],                             '31.103 gm',
	['pennyweight'],                            '1.555 gm',
	['scruple'],                                '1.296 gm',
	['carat', 'karat'],                         '200 milligram',
	['j-point'],                                '2 carat',

	# MomentOfInertia
	['kg m^2'],                                 'kg m^2',

	# Momentum
	['kg m/s'],                                 'kg m/s',
	['slug ft/s'],                              'slug feet/s',

	# AngularMomentum
	['kg m^2/s'],                               'kg m^2 / s',

	# Force
	['lb', 'lbs', 'pound', 'pound-force'],      'slug foot / s^2',
	['ounce-force'],                            'pound-force / 16',
	['dyne'],                                   'gm centimetre / s^2',
	['gram-force'],                             'gm g0',
	['kgf'],                                    'kilo gram-force',

	# Torque
	['Nm', 'newton-metre'],                     'N m',
	['ft-lb', 'footpound'],                     'foot pound-force',

	# Impulse
	['Ns'],                                     'N * s',
	['pound-second'],                           'pound * s',

	# Pressure
	['bar'],                                    '1e5 pascal',
	['torr'],                                   '133.322368 pascal',  #(101325 / 760)
	['psi'],                                    'pounds per inch^2',
	['atm', 'atmosphere'],                      '101325 pascal',
	['mmHg'],                                   '133.322 pascal',

	# Density
	['kg/m^3'],                                 'kg / m^3',
	['°proof'],                                 '923 kg / m^3',
	#UK metric https://en.wikipedia.org/wiki/Alcohol_proof (US version is just 2x ABV)

	# Energy
	['eV', 'electron-volt'],                    '1.60217733e-19 joule',
	['MeV'],                                    'mega electron-volt',
	['GeV'],                                    'giga electron-volt',
	['TeV'],                                    'tera electron-volt',
	['cal', 'calorie'],                         '4.184 joules',
	['kcal'],                                   'kilocalories',
	['btu', 'british-thermal-unit'],            '1055.056 joule',
	['therm'],                                  '1.0e5 btu',
	['erg'],                                    '1.0e-7 joule',
	['kWh'],                                    'kilowatt hour',
	['Eₕ', 'E_h', 'Ha'],                        '4.3597447222071e-18 joule',    #Hartree energy

	# Power
	['us-horsepower', 'us-hp'],                 '550 foot pound-force / s',
	['PS', 'horsepower', 'hp'],                 '75 kg * g0 * m / s',

	# Current (Base Unit)

	# Potential (Derived Unit)

	# Conductance
	['mho'],                                    '1 / ohm',

	# Capacitance (Derived Unit)

	# Inductance (Derived Unit)

	# Magnetic_flux
	['Mx', 'maxwell'],                          '1e-8 weber',

	# Magnetic_field
	['gauss'],                                  '1e-4 tesla',

	# Temperature
	['°R', 'Rankine'],                          '5/9 * K',
	['°F', 'Fahrenheit'],                       '5/9 * K + 459.67',

	# Dose
	['rad'],									'gray / 100',
	['rem'],									'sievert / 100',

	# FuelConsumption
	['m^3/m'],                                  'm^3 / metre',
	['l/100km'],                                '0.00001 litres / metre',

	# FuelEfficiency
	['m/m^3'],                                  'metres / m^3',
	['mpg'],                                    'miles / gallon',

	# ConsumptionRate
	['m^3/s'],                                  'm^3 / second',
	['gpd'],                                    'gallons / day',

	# SpecificEnergy 
	['J/kg'],                                   'joules / kg',
	['MJ/kg'],                                  'mega joules / kg',

	# Irradiance 
	['W/m^2'],                                  'W / m^2',

	# Insolation 
	['kWh/m^2'],                                'kWh / m^2',
	['Langley'],                                'calorie / cm^2',
);



if $db {
say "+++++++++++++++++++";
say "INITIALIZATION DONE";
say "+++++++++++++++++++";
say "";
}

#EOF
