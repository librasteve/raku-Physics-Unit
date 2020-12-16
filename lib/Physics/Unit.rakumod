unit module Physics::Unit:ver<0.0.4>:auth<Steve Roe (p6steve@furnival.net)>; 
#viz. https://en.wikipedia.org/wiki/International_System_of_Units

#snagging
#-odd type mop up
#-rereview data map names (ie protoname should be type-to-protoname & type-to-protounit
#-list-of-names should be a key only hash to avoid dupes
#-anyway defn-to-names has same info (except where externally defined in which case GU2)
#-remove protoype / SetType logic?
#-new test 02...
#-clean up UnitActions code
#`[ to Measure
495     'Luminous-Flux'      => 'lumen',
496     'Illuminance'        => 'lux',
497     'Radioactivity'      => 'becquerel',
4_99     'Catalytic-Activity' => 'kat',
##check - not _
'Magnetic-Flux'      => 'weber',
'Magnetic-Field'     => 'tesla',
#]


my $db = 0;           #debug 

##### Constants and Data Maps ######
constant \preload  = 0;     #Preload Derived Units (ie. debug / slow)
constant \locale   = "imp"; #Imperial="imp"; US="us' FIXME v2 make tag
constant \NumBases = 8; 
my Str   @BaseNames;

my @list-of-names;          #all known Unit object names
my %defn-to-names;          #map defn => [names] of stock Units
my %unit-by-name;           #map name => Unit objects (when instantiated)
my %prefix-by-name;         #map name => Prefix objects
my %protoname-to-type;      #map name of prototype Unit => Type
my %type-to-prototype;      #map Type name to a Unit object that exemplifies the type 
my %type-to-dims;			#map Type name to dims vector
my %odd-type-by-name;       #mop up remaining odd ambiguous types

#Power synonyms
my %pwr-preword   = ( square  => 2, sq => 2, cubic => 3, );   
my %pwr-postword  = ( squared => 2,          cubed => 3, );   

#Power superscripts ie. x¹ x² x³ x⁴ x⁻¹ x⁻² x⁻³ x⁻⁴  
my %pwr-superscript = (  
     '¹' =>  1,  '²' =>  2,  '³' =>  3,  '⁴' =>  4,   
    '⁻¹' => -1, '⁻²' => -2, '⁻³' => -3, '⁻⁴' => -4,  
);

######## Classes & Roles ########
class Unit is export {
    has Real $!factor = 1;
    has Real $!offset = 0;				#i.e. for K <=> °C
    has Str  $!defn   = '';
    has Str  $!type;
    has Str  @.names  is rw = [];
    has Int  @.dims   = 0 xx NumBases;
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
		#1 type has been explicitly set ... rarely used to avoid ambiguous state
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
		if $just1  { return disambiguate(@d) }
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
        nextwith :names([]), :type(''), :dims(@.dims.clone), :dmix($.dmix.clone)
    }
    multi method new( Unit:D $u: @names ) {
        my $n = $u.clone;
        $n.SetNames: @names;
        $n.SetType();
        return $n
    }

    ### output methods ###
    method Str { self.name }

    multi method name()         { @!names[0] || '' }
    multi method name( Str $n ) { self.SetNames([$n]) }

    method canonical {
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
    method pretty {               #following SI recommendation
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
    method SetNames( @_ is copy, :$noplural ) {
        @.names = [];			#clears then sets
        for @_ -> $n {
            push @.names, $n;
            %unit-by-name{$n} = self;

            #naive plurals - append 's' unless...
            if my $ns = naive-plural( $n ) { 
                @.names.push: $ns;
                %unit-by-name{$ns} = self;
            }   
        }
        @.names.push: $.defn unless @.names;
        @list-of-names.push: |@.names;

        say "SetNames: {@.names}" if $db;
    }
    method SetType( $t? ) { 
		for @.names -> $n {                 #set up this unit as a prototype
			if my $p = %protoname-to-type{$n} {
				$!type = $p;	
				%type-to-prototype{$!type} = self;
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
		self.dmix{$e} = $d;
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

    ### mathematical Measure methods ###
    method multiply( Unit $r --> Unit ) {
		my $l = self.clone;
		my $x = $l.times( $r );
		my $t = $x.type( just1 => 1 );		#occasionally can be > one type
		return %type-to-prototype{$t};
    }
    method divide( Unit $r ) {
		my $l = self.clone;
		my $x = $l.share( $r );
		my $t = $x.type( just1 => 1 );		#occasionally can be > one type
		return %type-to-prototype{$t}
    }
	method root-extract( Int $n where 1 <= $n <= 4 ) {
        #only when all dims divisible by root
		my $l = self.clone;
		die "rebase failed" unless $l.factor == 1;
		$l.defn: '';
		$l.type: '';
		$l.dims = $l.dims.map({($_/$n).Int});
		for $l.dmix.kv -> $k,$v { $l.dmix{$k} = $v/$n }
		return %type-to-prototype{$l.type}
	}
}

######## Subroutines ########
sub ListUnits is export {		
	return @list-of-names
}
sub ListTypes is export {
    return sort keys %type-to-prototype
}
sub ListBases is export {
    return @BaseNames
}
sub GetPrototype( Str $type ) is export {
#FIXME needs testing -- Measure calls for rebase (ie convert to base==proto type)
	if my $pt = %type-to-prototype{$type} {
		return $pt;
	} else {
		for %protoname-to-type -> %p {
			return GetUnit(%p.key) if %p.value eq $type;
		}
	}	
}

sub GetUnit( $u ) is export {
    #1 if Unit, eg. from Measure.new( ... unit => $u ), just return it
    if $u ~~ Unit {
		say "GU1 from $u" if $db;
        return $u
    }   

    #2 if name or prefix already instantiated
		say "GU2 from $u" if $db;

    for %unit-by-name.kv   -> $k,$v { 
		return $v if $k eq $u 
	} 
    for %prefix-by-name.kv -> $k,$v { 
		return $v if $k eq $u 
	}

    #3 if name in our defns, instantiate it 
		say "GU3 from $u" if $db;

	for %defn-to-names -> %p {
		if %p.value.grep($u) {
			my $nuo = Unit.new( defn => %p.key, names => %p.value );  
			return $nuo;
		}   
	}   

    #4 if no match, instantiate from definition 
		say "GU4 from $u" if $db;

    my $nuo = Unit.new( defn => $u );
    return subst-shortest( $nuo ) // $nuo;
}
sub subst-shortest( Unit $u ) { 
    #subtitutes shortest name if >1 unit name has same dimensions 
    # ... so that eg. 'J' beats 'kg m^2 / s^2'

    my @same-by-name;
    for %unit-by-name.kv -> $k,$v { 
        @same-by-name.push($k) if $v.same-dims($u) 
    }   
    if @same-by-name {   
        my @same-by-size = @same-by-name.sort({$^a.chars cmp $^b.chars});
        return %unit-by-name{@same-by-size[0]}  #shortest
    }   
}
sub disambiguate( @t ) {
	#bias rules to help when multiple types are found
	my %dh = %(
		<Energy>      => <Energy Torque>,
		<Frequency>   => <Angular-Speed Frequency>,
	);
	for %dh.kv -> $k,$v { return $k if @t.sort eq $v.sort } 
}
sub naive-plural( $n ) { 
    #naive plurals - append 's' unless...
    if     $n.chars > 2                 #...too short
        && $n.comb.first(:end) ne 's'   #...already ends with 's' 
        && $n.comb.first(:end) ne 'z'   #...already ends with 'z' 
        && $n !~~ /<[\d\/^*]>/          #...contains a digit or a symbol
    {   
        return $n ~ 's';
    }   
}

######## Grammars ########
sub CreateUnit( $defn is copy ) {

	#6.d faster regexes with Strings {<$str>} & slower with Arrays {<@arr>}
    #erase compound names from element name match candidates (to force generation of dmix)
    my $unit-names       = @list-of-names.grep({! /<[\s*^./]>/}).join('|');
    my $prefix-names     = %prefix-by-name.keys.join('|');
    my $pwr-prewords     = %pwr-preword.keys.join('|');
    my $pwr-postwords    = %pwr-postword.keys.join('|');
    my $pwr-superscripts = %pwr-superscript.keys.join('|');

    #escape quote non alphanum| charactersi...
    $unit-names       ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;
    $pwr-superscripts ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;

	$defn .= trim;

    ##use Grammar::Tracer;
    grammar UnitGrammar {
        token TOP         { ^  \s* <numerator=.compound>
                              [\s* <divider> \s* <denominator=.compound>]?
                              [\s*    '+'    \s* <offset>  ]? \s* $ } #'+' is hardwired
        token divider     { '/' || 'per' }
        token compound    { <element>+ % <sep> }
        token sep         { [ '*' || '.' || ' *' || ' .' || ' ' ] }
        token element     { <factor> || <pnp-before> || <pnp-after> }

        token factor      { <number> }
        token offset      { <number> }
        token number      { \S+ <?{ defined +"$/" }> } #get chars, assert coerce to Real via +

        token pnp-before  { <pwr-before>  \s+? <prefix-name> } #pnp=="prefix-name-power" combo
        token pnp-after   { <prefix-name> \s*? <pwr-after>?  }

        token prefix-name { <prefix>? \s*? <name> }
        token prefix      { <$prefix-names> }
        token name        { <$unit-names>   }

        token pwr-before  { <$pwr-prewords> }
        token pwr-after   { <pwr-postwd> || <pwr-supers> || <pwr-normal> }
        token pwr-postwd  { <$pwr-postwords>    }
        token pwr-supers  { <$pwr-superscripts> }

        token pwr-normal  { <pwr-symbol> \s*? <pwr-digits> }
        token pwr-digits  { <[-+]>? <[1..4]> }
        token pwr-symbol  { '**' || '^' }
    }

    class UnitActions   {
		#| from key value pair example https://docs.raku.org/language/grammars#Action_objects
		method TOP($/)			{ 
			my $nu = $<numerator>.made;
			my $de = $<denominator>.made;
			$nu.share($de) if $de;
			make $nu;
#say "in top...", $/.made if $db;
		}

		#| accumulates element Units using times
		method compound($/)		{ 
			my $acc = Unit.new();
			for $<element>>>.made -> $x {
				$acc.times($x);
			}
			make $acc;
#say "in comp, acc made..."; say $/.made;
		}

		#| makes a list of element units from factor, offset, prefix and name
		method element($/)		{ 
			my $nn = $<pnp-before><prefix-name>.made<unit> || 
					 $<pnp-after><prefix-name>.made<unit>;
			my $ee = $<pnp-before><prefix-name>.made<defn> || 
					 $<pnp-after><prefix-name>.made<defn>;
			my $dd = $<pnp-before><pwr-before>.made || 
					 $<pnp-after><pwr-after>.made   // 1;
			if $nn { 
				$nn.raise($dd, $ee);
				make $nn;
			}
			my $fu = $<factor>.made;
			if $fu {
				make $fu;
			}
			my $ou = $<factor>.made;
			if $ou {
				make $ou;
			}
##say "in elem..."; say $/.made;
		}

		#| make Unit from factor
        method factor($/)		{ 
			my $fu = Unit.new();
			$fu.times($/.Real);
			make $fu;
##say "in factor..."; say $/.made;
		}

		#| make Unit from offset 
        method offset($/)		{ 
			my $ou = Unit.new();
			$ou.offset($/.Real);
			make $ou;
##say "in offset..."; say $/.made;
		}

        method prefix-name($/)	{ 
			my $en = $<name>.made<defn>;
			my $nn = $<name>.made<unit>;
			my $pn = $<prefix>.made<unit>;
			$nn.times($pn) if $pn;
			make %( defn => $en, unit => $nn );
##say "in prefix-name..."; say $/.made;
		}

        method prefix($/)		{ 
			my $pf=GetUnit($/.Str).clone;
			make %( unit => $pf );
##say "in prefix..."; say $/.made;
		}

        method name($/)			{ 
			my $en=$/.Str; 
			my $nn=GetUnit($en).clone; 
			$nn.dmix=∅.MixHash;
			make %( defn => $en, unit => $nn );
##say "in name..."; say $/.made;
		}

        method pwr-before($/)	{
			my $dp=%pwr-preword{$/.Str};
			make $dp;
##say "in pnp-before..."; say $/.made;
		}

		method pnp-after($/)	{
			my $nn = $<pwr-postwd>.made || 
					 $<pwr-supers>.made ||
					 $<pwr-normal>.made;
			make $nn;
##say "in pnp-after..."; say $/.made;
		}
        method pwr-postwd($/)	{
			my $dp=%pwr-postword{$/.Str};
			make $dp;
		}
        method pwr-supers($/)	{ 
			my $dp=%pwr-superscript{$/.Str}; 
			make $dp;
		}
        method pwr-normal($/)	{ 
			my $dp=$<pwr-digits>.Int;
			make $dp;
		}
    }

    my $match = UnitGrammar.parse( $defn, :actions(UnitActions) );
    if $match.so {
		say "Made: $match\t= ", $match.made if $db;
		my $mu = $match.made;
        $mu.defn: $defn;
        return $mu;
    } else {
        die "Couldn't parse defn Str $defn.";
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

        say "Initialized Prefix $name" if $db;
    }
}
sub InitBaseUnit( @_ ) {
    my $i = 0;
    for @_ -> %h {
        my ($type, $names) = %h.key, %h.value;

        my $u = Unit.new;
        $u.SetNames: $names;    #auto decont to list
        $u.defn: $u.name;
        @BaseNames.push: $u.name;

        #dimension vector has zeros in all but one place
        $u.dims[$i++] = 1;
		$u.dmix{$u.name} = 1;

        $u.type: $type;
		%protoname-to-type{$u.name} = $type;
		%type-to-prototype{$type} = $u;

        say "Initialized Base Unit $names[0]" if $db;
    }
}
sub InitDerivedUnit( @_ ) { 
    if preload {
        for @_ -> $names, $defn {
            Unit.new( defn => $defn, names => [|$names] );
        }   
    } else {
        InitUnit( @_ )
    }   
}
sub InitTypes( @_ )  {
    for @_ -> %p {
        %protoname-to-type{%p.value} = %p.key; #ie. reversed
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
sub InitUnit( @_ ) is export {
	if preload {
		for @_ -> $names, $defn {
            Unit.new( defn => $defn, names => [|$names] );
		}
	} else {
		#eg. ['N',  'newton'],           'kg m / s^2',
		#      ^ ,   ^^^^^^ names         ^^^^^^^^^^ defn

		for @_ -> $names, $defn {
			my @newbies;
			for |$names -> $n {
				@newbies.push: $n; 
				if naive-plural( $n ) -> $p {
					@newbies.push: $p; 
				}   
			}   
			@list-of-names.push: |@newbies;
			%defn-to-names{$defn} = [@newbies];
		}   
	}
}
######## Unit Data ########

InitPrefix (
    #SI Prefixes
    #avoid 1e2 format to encourage Rats
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
    'zepto',   0.000000000000000000001,
    'yocto',   0.000000000000000000000001,
);
InitBaseUnit (
    #SI Base Units 
	#viz https://en.wikipedia.org/wiki/Dimensional_analysis#Definition
    'Length'      => ['m', 'metre', 'meter'],
    'Mass'        => ['kg', 'kilogram'],
    'Time'        => ['s', 'second', 'sec'],
    'Current'     => ['A', 'amp', 'ampere', 'ampère'],  
    'Temperature' => ['K', 'kelvin'],   
    'Substance'   => ['mol', 'mole'],
    'Luminosity'  => ['cd', 'candela', 'candle'],
	'Angle'		  => ['radian'],		
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
	['kat', 'katal'],                       'mol s^-1',
	#SI coherent Derived Units in terms of base units - TBD [see url]
	#SI coherent Derived Units that include units with special names - TBD [see url]
);
InitTypes (
	#sets name of prototype unit
    'Dimensionless'      => 'unity',
    'Angle'              => 'radian',
    'Angular-Speed'		 => 'radians per second',
    'Solid-Angle'        => 'steradian',
    'Frequency'          => 'hertz',
    'Area'               => 'm^2',
    'Volume'             => 'm^3',
    'Speed'              => 'm/s',
    'Acceleration'       => 'm/s^2',
    'Momentum'           => 'kg m/s',
    'Force'              => 'newton',
    'Torque'             => 'Nm',
    'Impulse'            => 'Ns',
    'Moment-of-Inertia'  => 'kg m^2',
    'Angular-Momentum'   => 'kg m^2/s',
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
    'Magnetic-Flux'      => 'weber',
    'Magnetic-Field'     => 'tesla',
    'Luminous-Flux'      => 'lumen',
    'Illuminance'        => 'lux',
    'Radioactivity'      => 'becquerel',
    'Dose'               => 'gray',
    'Catalytic-Activity' => 'kat',
);
InitTypeDims (
	#viz https://en.wikipedia.org/wiki/Dimensional_analysis#Definition
	#                      (L,M,T,I,Θ,N,J,A)  [A=Angle]

	'Acceleration'		=> (1,0,-2,0,0,0,0,0),
	'Angle'				=> (0,0,0,0,0,0,0,1),
	'Angular-Momentum'	=> (2,1,-1,0,0,0,0,0),
	'Angular-Speed'		=> (0,0,-1,0,0,0,0,0),
	'Area'				=> (2,0,0,0,0,0,0,0),
	'Capacitance'		=> (-2,-1,4,2,0,0,0,0),
	'Catalytic-Activity'=> (0,0,-1,0,0,1,0,0),
	'Charge'			=> (0,0,1,1,0,0,0,0),
	'Conductance'		=> (-2,-1,3,2,0,0,0,0),
	'Density'			=> (-3,1,0,0,0,0,0,0),
	'Dimensionless'		=> (0,0,0,0,0,0,0,0),
	'Dose'				=> (2,0,-2,0,0,0,0,0),
	'Energy'			=> (2,1,-2,0,0,0,0,0),
	'Force'				=> (1,1,-2,0,0,0,0,0),
	'Frequency'			=> (0,0,-1,0,0,0,0,0),
	'Illuminance'		=> (-2,0,0,0,0,0,1,0),
	'Impulse'			=> (1,1,-1,0,0,0,0,0),
	'Inductance'		=> (2,1,-2,-2,0,0,0,0),
	'Luminous-Flux'		=> (0,0,0,0,0,0,1,2),
	'Magnetic-Field'	=> (0,1,-2,-1,0,0,0,0),
	'Magnetic-Flux'		=> (2,1,-2,-1,0,0,0,0),
	'Moment-of-Inertia'	=> (2,1,0,0,0,0,0,0),
	'Momentum'			=> (1,1,-1,0,0,0,0,0),
	'Potential'			=> (2,1,-3,-1,0,0,0,0),
	'Power'				=> (2,1,-3,0,0,0,0,0),
	'Pressure'			=> (-1,1,-2,0,0,0,0,0),
	'Radioactivity'		=> (0,0,-1,0,0,0,0,0),
	'Resistance'		=> (2,1,-3,-2,0,0,0,0),
	'Solid-Angle'		=> (0,0,0,0,0,0,0,2),
	'Speed'				=> (1,0,-1,0,0,0,0,0),
	'Torque'			=> (2,1,-1,0,0,0,0,0),
	'Volume'			=> (3,0,0,0,0,0,0,0),
);
InitOddTypes (
    #mop up any odd ambiguous types
    'eV'    => 'Energy',
    'MeV'   => 'Energy',
    'GeV'   => 'Energy',
    'TeV'   => 'Energy',
    'cal'   => 'Energy',
    'kcal'  => 'Energy',
    'btu'   => 'Energy',
    'erg'   => 'Energy',
    'kWh'   => 'Energy',
    'ft-lb' => 'Torque',
);
#`[[888
#mop up remaining ambiguous types
GetUnit('eV').type:    'Energy';
GetUnit('MeV').type:   'Energy';
GetUnit('GeV').type:   'Energy';
GetUnit('TeV').type:   'Energy';
GetUnit('cal').type:   'Energy';
GetUnit('kcal').type:  'Energy';
GetUnit('btu').type:   'Energy';
GetUnit('erg').type:   'Energy';
GetUnit('kWh').type:   'Energy';
GetUnit('ft-lb').type: 'Torque';
#]]
InitUnit (
	# Dimensionless
	['one', 'unity'],							'1',
	['semi','demi','hemi'],						'1/2',
	['%','percent'],							'1/100',
	['ABV'],									'1',   
	#FIXME v2 try ['pi'], 'π', #extend Number regex
	['pi'],										'3.1415926535897932385', 

	# Angle
	['°', 'degree', 'deg', 'º'],                'pi radians / 180',
	['ᵍ', 'gon'],                               'pi radians / 200',

	# Solid Angle
	['deg²'],									'deg^2',
	['sp','spat'],								'4 pi steradians',

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
	# setting Imperial (imp-) or US (us-) from \locale
	['us-gallon'],                              '3.785411784 litre',
	['imp-gallon'],                             '4.54609 litre',
	['gallon'],									"1 {locale}-gallon",
	['firkin'],							        '9 gallons',
	['barrel'],							        '36 gallons',
	['quart'],                                  'gallon/4',
	['peck'],                                   '8 quarts',
	['bushel'],                                 '4 pecks',
	['fifth'],                                  'us-gallon/5',
	['us-pint'],                                'us-gallon/8',
	['imp-pint'],                               'imp-gallon/8',
	['pint'],									"1 {locale}-pint",
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

	# Angular-Speed
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

	# Moment-of-Inertia
	['kg m^2'],                                 'kg m^2',

	# Momentum
	['kg m/s'],                                 'kg m/s',
	['slug ft/s'],                              'slug feet/s',

	# Angular-Momentum
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
	['rad'],           'gray / 100',
	['rem'],           'sievert / 100',
);

if $db {
say "+++++++++++++++++++";
say "INITIALIZATION DONE";
say "+++++++++++++++++++";
say "";
}

#EOF
