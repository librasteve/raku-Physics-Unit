[![Build Status](https://travis-ci.com/p6steve/raku-Physics-Unit.svg?branch=master)](https://travis-ci.com/p6steve/raku-Physics-Unit)

# Physics::Unit
A set of stock SI, Imperial and US Unit objects for raku that are employed by Physics::Measure objects.

#viz. https://en.wikipedia.org/wiki/International_System_of_Units

See also Physics::UnitAffix for support of SI Prefixes.

# Instructions
zef install --verbose https://github.com/p6steve/raku-Physics-Unit.git

and, conversely, zef uninstall Physics::Unit

For more control, the following also work:

zef --verbose install https://github.com/p6steve/raku-Physics-Unit.git@v1.0.0

zef --verbose install https://github.com/p6steve/raku-Physics-Unit.git@latest

# Synopsis - Physics::Unit

```perl6
#!/usr/bin/env raku
use Physics::Unit;

#Unit objects can be selected or created with GetUnit:
my Unit   $u  = GetUnit( 'm' );
    
# Define your own unit named "ff" (named args)
my $ff = Unit.new( defn => 'furlong / fortnight', names => ['ff'] );

say "$ff";              #'ff'
say $ff.type;           #'Speed' (this is automatically induced from the dimensions of the defn expression)
say $ff.canonical;      #'m.s-1' (unit expressed in terms of SI base units)
say $ff.pretty;         #'m⋅s⁻¹' (unicode variant per the SI recommendation)

say $ff.raku;
	#Unit.new( factor => 0.00016631, offset => 0, defn => 'furlong / fortnight', type => '',
	#dims => [1,0,-1,0,0,0,0,0], dmix => ("fortnight"=>-1,"furlong"=>1).MixHash, 
	#names => ['ff'] );

# New Unit by reference to an existing one
my $fh = $ff.new( <fh fi> );     #optional argument: list of names

# New Unit (positional args)
my $fg = Unit.new( defn => 'furlong / fortnight', names => ['fg'] );

# More intricate unit expression (using the newly defined unit 'ff'):
my $gonzo = Unit.new( defn => "13 square millimeters per ff", names => ['gonzo'] );

# Parsing of input
my $u1 = GetUnit( 'kg m^2 / s^2' );
my $u2 = GetUnit( 'kg m^2/s^2' );

# Comparison by dimensions
say "compare $u1, $u2... " ~ $u2.same-dims($u1);    #'compare J, J... 1'

# SI recommended string representation
say "{$u1.factor} {$u1.pretty}";                    #'1 m²⋅kg⋅s⁻²'

# SI derived unit representation
say "{$u1.factor} {$u1.name}";                      #'1 J'

dd ListUnits();                                     #List of Units

```
#`[[
("\%", "13 square millimeters per ff", "A", "ABV", "ABVs", "Bq", "C", "Centigrade", "Centigrades", "F", "Fahrenheit", "Fahrenheits", "GeV", "GeVs", "Gy", "H", "Hz", "J", "K", "L", "MeV", "MeVs", "Mx", "N", "Nm", "Ns", "PS", "Pa", "Rankine", "Rankines", "S", "Sv", "T", "TeV", "TeVs", "V", "W", "Wb", "acre", "acres", "alcohol-unit", "alcohol-units", "amp", "ampere", "amperes", "amps", "ampère", "ampères", "angstrom", "angstroms", "are", "ares", "astronomical-unit", "astronomical-units", "atm", "atmosphere", "atmospheres", "atms", "atomic-mass-unit", "atomic-mass-units", "au", "bar", "barn", "barns", "barrel", "barrels", "bars", "becquerel", "becquerels", "bottle", "bottles", "british-thermal-unit", "british-thermal-units", "btu", "btus", "bushel", "bushels", "ca", "cable", "cables", "cal", "calorie", "calories", "cals", "candela", "candelas", "candle", "candles", "carat", "carats", "cc", "cd", "celsius", "centuries", "century", "centurys", "coulomb", "coulombs", "cup", "cups", "cycle", "cycles", "day", "days", "deg", "degree", "degrees", "degs", "deg²", "deg²s", "demi", "demis", "dram", "drams", "dyne", "dynes", "eV", "earth-gravity", "earth-gravitys", "electron-volt", "electron-volts", "erg", "ergs", "farad", "farads", "fathom", "fathoms", "feet", "feets", "ff", "fg", "fh", "fi", "fifth", "fifths", "firkin", "firkins", "floz", "flozs", "fluid-ounce", "fluid-ounces", "fluidram", "fluidrams", "foot", "footpound", "footpounds", "foots", "fortnight", "fortnights", "fps", "ft", "ft-lb", "ft-lbs", "furlong", "furlong / fortnight", "furlongs", "g", "g0", "gallon", "gallons", "gauss", "gill", "gills", "gm", "gon", "gons", "gonzo", "gonzos", "grain", "grains", "gram", "gram-force", "gram-forces", "gramme", "grammes", "grams", "gray", "grays", "hectare", "hectares", "hemi", "hemis", "henry", "henrys", "hertz", "hertzs", "horsepower", "horsepowers", "hour", "hours", "hp", "hr", "hundredweight", "hundredweights", "imp-gallon", "imp-gallons", "imp-pint", "imp-pints", "in", "inch", "inchs", "j-point", "j-points", "joule", "joules", "kWh", "kWhs", "karat", "karats", "kat", "katal", "katals", "kats", "kcal", "kcals", "kelvin", "kelvins", "kg", "kg m/s", "kg m^2", "kg m^2 / s^2", "kg m^2/s", "kg m^2/s^2", "kg/m^3", "kgf", "kgfs", "kilogram", "kilograms", "km", "knot", "knots", "kph", "kphs", "kps", "l", "lb", "lbm", "lbms", "lbs", "light-year", "light-years", "liter", "liters", "litre", "litres", "lm", "long-ton", "long-tons", "lumen", "lumens", "lux", "luxs", "lx", "ly", "m", "m/s", "m/s^2", "m2", "m3", "m^2", "m^3", "maxwell", "maxwells", "meter", "meters", "metre", "metres", "metric-ton", "metric-tons", "mho", "mhos", "micron", "microns", "mile", "miles", "millenia", "millenias", "millenium", "milleniums", "min", "minim", "minims", "mins", "minute", "minutes", "mol", "mole", "moles", "mols", "month", "months", "mph", "mphs", "m²", "m³", "nautical-mile", "nautical-miles", "newton", "newton-metre", "newton-metres", "newtons", "nmile", "nmiles", "ohm", "ohms", "one", "ones", "ounce", "ounce-force", "ounce-forces", "ounces", "oz", "parsec", "parsecs", "pascal", "pascals", "peck", "pecks", "pennyweight", "pennyweights", "percent", "percents", "perch", "perchs", "pi", "pica", "picas", "pint", "pints", "point", "points", "pole", "poles", "pound", "pound-force", "pound-forces", "pound-second", "pound-seconds", "pounds", "pounds-mass", "psi", "psis", "quart", "quarts", "rad", "radian", "radians", "radians per second", "radians per seconds", "rads", "rem", "rems", "revolution", "revolutions", "revolutions per second", "revolutions per seconds", "revs", "rod", "rods", "rpm", "rpms", "s", "scruple", "scruples", "sec", "second", "seconds", "secs", "semi", "semis", "short-ton", "short-tons", "siemens", "sievert", "sieverts", "slug", "slug ft/s", "slugs", "sp", "spat", "spats", "sr", "steradian", "steradians", "stone", "stones", "tablespoon", "tablespoons", "tbsp", "tbsps", "teaspoon", "teaspoons", "tesla", "teslas", "therm", "therms", "ton", "tonne", "tonnes", "tons", "torr", "torrs", "troy-ounce", "troy-ounces", "troy-pound", "troy-pounds", "tsp", "tsps", "u", "unity", "unitys", "us-gallon", "us-gallons", "us-horsepower", "us-horsepowers", "us-hp", "us-hps", "us-pint", "us-pints", "volt", "volts", "watt", "watts", "weber", "webers", "week", "weeks", "yard", "yards", "year", "years", "yr", "°", "°C", "°F", "°R", "°proof", "°proofs", "º", "å", "Ω", "μ", "ᵍ").Seq
#]]

