unit module Physics::Unit:ver<0.0.4>:auth<Steve Roe (p6steve@furnival.net)>; 
#viz. https://en.wikipedia.org/wiki/International_System_of_Units

#snagging
#NewType to SetType
#-preload
#- odd type mop up
#-rereview data map names


my $db = 0;           #debug 

##### Constants and Data Maps ######
#888constant \preload  = 0;       #Preload Derived Units   FIXME v2 make tag
constant \locale = "imp";   #Imperial="imp"; US="us' FIXME v2 make tag
constant \NumBases = 8; 
my Str   @BaseNames;

my @list-of-names;          #all known Unit object names
my %defn-to-names;          #map defn => [names] of pre-defined Units
my %unit-by-name;           #map name => Unit objects (when instantiated)
my %prefix-by-name;         #map name => Prefix objects
my %protoname-to-type;      #map name of prototype Unit => Type
my %type-to-prototype;      #map Type name to a Unit object that exemplifies the type 
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
        return $!type   if $!type;		#rarely set ... just to disambiguate

        return 'prefix' if %prefix-by-name{self.name};

        my @t;
        for %type-to-prototype.keys -> $k { 
            push @t, $k if self.dims eqv %type-to-prototype{$k}.dims
        }    
        #return value depends on whether we got zero, one or multiple types
		if @t == 0 { return '' }
		if @t == 1 { return @t[0] }
		if $just1  { return disambiguate(@t) }
		if @t > 1  { return @t.sort }
    }    

    ### new & clone methods ###

    #new by deep cloning an existing Unit
    method clone {
        nextwith :names([]), :type(''), :dims(@.dims.clone)
    }
    multi method new( Unit:D $u: @names ) {
        my $n = $u.clone;
        $n.SetNames: @names;
        return $n
    }

    #new by named arguments 
    multi method new( :$defn!, :@names ) {
        my $n = CreateUnit( $defn );
        $n.SetNames: @names;
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

    ### behavioural methods ###
    method SetNames( @_ is copy, :$noplural ) {
        @.names = [];			#clears then sets
        for @_ -> $n {
            push @.names, $n;
            %unit-by-name{$n} = self;

            #naive plurals - append 's' unless...
            if     $n.chars > 2                 #...too short
                && $n.comb.first(:end) ne 's'   #...already ends with 's'
                && $n !~~ /<[\d\/^*]>/          #...contains a digit or a symbol
                && ! $noplural                  #...instructed not to
            {
                my $ns = $n ~ 's';
                push @.names, $ns;
                %unit-by-name{$ns} = self;
            }
        }
    }
#`[[888 mv to SetType
    method NewType( $t ) {
        $!type = $t;
        %type-to-prototype{$t} = self;
    }
#]]
    method CheckChange {
        die "You're not allowed to change named units!" if self.name;
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
    return sort keys %unit-by-name
}
sub ListTypes is export {
    return sort keys %type-to-prototype
}
sub ListBases is export {
    return @BaseNames
}
sub GetPrototype( Str $t ) is export {
	return %type-to-prototype{$t}
}
sub GetUnit( $u ) is export {
    return $u if $u ~~ Unit;

    for %unit-by-name.kv -> $k,$v { return $v if $k eq $u }
    for %prefix-by-name.kv -> $k,$v { return $v if $k eq $u }

	my $c = CreateUnit($u);				#finally, try as a definition

 	#look for same dims (shortest name wins if >1)
	my @same-by-name;
    for %unit-by-name.kv -> $k,$v { 
		@same-by-name.push($k) if $v.same-dims($c) 
	}
    if @same-by-name {	
		my @same-by-size = @same-by-name.sort({$^a.chars cmp $^b.chars});
		return %unit-by-name{@same-by-size[0]}	#shortest
	}
    return $c
}
sub disambiguate( @t ) {
	#bias rules to help when multiple types are found
	my %dh = %(
		<Energy>      => <Energy Torque>,
		<Frequency>   => <Angular-Speed Frequency>,
	);
	for %dh.kv -> $k,$v { return $k if @t.sort eq $v.sort } 
}

######## Grammars ########
sub CreateUnit( $defn is copy ) {
    #6.d faster regexes with Strings {<$str>} & slower with Arrays {<@arr>}
	#strip compound names to force dmix generation
    my $unit-names       = %unit-by-name.keys.grep({! /<[\s*^./]>/}).join('|');
    my $prefix-names     = %prefix-by-name.keys.join('|');
    my $pwr-prewords     = %pwr-preword.keys.join('|');
    my $pwr-postwords    = %pwr-postword.keys.join('|');
    my $pwr-superscripts = %pwr-superscript.keys.join('|');

    #escape quote non alphanum| charactersi...
    $unit-names       ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;
    $pwr-superscripts ~~ s:g/ ( <-[a..z A..Z 0..9 \|]> ) / '$0' /;

    my $u = Unit.new();         #stub unit we are creating
	$defn .= trim;

    ##use Grammar::Tracer;
    grammar UnitGrammar {
        token TOP         { ^  \s* <compound>
                              [\s* <divider> \s* <compound>]?
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

    my $ad = 0;         #after the divider?
    my Int  $d = 1;	    #digits from power
	my Str  $e = '';	#element name
    my Unit ($n,$p);    #new units for each element name & prefix
    class UnitActions   {
        method divider($/)     { $ad = 1 }
        method factor($/)      { !$ad ?? $u.times($/.Real) !! $u.share($/.Real) }
        method offset($/)      { $u.offset($/.Real) }

        method name($/)        { $e=$/.Str; $n=GetUnit($e).clone; $n.dmix=∅.MixHash }
        method prefix($/)      { $p=GetUnit($/.Str).clone; $p.dmix=∅.MixHash }
        method prefix-name($/) { $n.times($p) if $p       }

        method pwr-before($/)  { $d=%pwr-preword{$/.Str}     }
        method pwr-postwd($/)  { $d=%pwr-postword{$/.Str}    }
        method pwr-supers($/)  { $d=%pwr-superscript{$/.Str} }
        method pwr-normal($/)  { $d=$<pwr-digits>.Int        }

		method element($/)     { if $n { $n.raise($d, $e);
									     !$ad ?? $u.times($n) !! $u.share($n) }
								 $d=1; $e=''; $n=$p=Nil;     }
    }

    my $match = UnitGrammar.parse( $defn, :actions(UnitActions) );
    if $match.so {
        $u.defn: $defn;
        $u.SetNames: [$defn], :noplural;
        return $u
    } else {
        die "Couldn't parse defn Str $defn.";
    }
}

######## Initialization ########
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

        $u.NewType: $type;

        say "Initialized Base Unit $names[0]" if $db;
    }
}
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
sub InitUnit( @_ ) is export {
    for @_ -> $names, $defn {
        my $u = CreateUnit( $defn );
        $u.SetNames: $names;    #decont from scalar to list

        say "Initialized Unit $names[0]"  if $db;
        say "================ >>>"        if $db;
        say "UG.parsed: ≪$defn≫"          if $db;
        say "UG.result: "                 if $db;
        say $u.gist                       if $db;
    }
}
sub InitTypes( @_ )  {
    for @_ -> %p {
        my ($t, $u) = %p.key, %p.value; #888 del
		GetUnit($u).NewType($t);		#888 del
		%protoname-to-type{%p.value} = %p.key; #ie. reversed
    }
}
sub InitOddTypes( @_ ) { 
    for @_ -> %p {
        %odd-type-by-name{%p.key} = %p.value;
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
    'Length'      => ['m', 'metre', 'meter'],
    'Mass'        => ['kg', 'kilogram'],
    'Time'        => ['s', 'second', 'sec'],
    'Current'     => ['A', 'amp', 'ampere', 'ampère'],  
    'Temperature' => ['K', 'kelvin'],   
    'Substance'   => ['mol', 'mole'],
    'Luminosity'  => ['cd', 'candela', 'candle'],
	'Angle'		  => ['radian'],		
);

InitUnit (

	# Dimensionless
	['pi'],          '3.1415926535897932385',
#FIXME try ['pi'], 'π', #or similar

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

	# Dimensionless
	['one', 'unity'],							'1',
	['semi','demi','hemi'],						'1/2',
	['%','percent'],							'1/100',
	['ABV'],									'1',   

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
    'Magnetic_Flux'      => 'weber',
    'Magnetic_Field'     => 'tesla',
    'Dose'               => 'gray',
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

if $db {
say "+++++++++++++++++++";
say "INITIALIZATION DONE";
say "+++++++++++++++++++";
say "";
}

#EOF
