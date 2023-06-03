[![License: Artistic-2.0](https://img.shields.io/badge/License-Artistic%202.0-0298c3.svg)](https://opensource.org/licenses/Artistic-2.0)
[![raku-physics-unit -> DH](https://github.com/librasteve/raku-Physics-Unit/actions/workflows/unit-weekly.yaml/badge.svg)](https://github.com/librasteve/raku-Physics-Unit/actions/workflows/unit-weekly.yaml)

# Physics::Unit
A set of stock SI, Imperial and US Unit objects for raku that are employed by [Physics::Measure](https://github.com/librasteve/raku-Physics-Measure) objects.

#viz. https://en.wikipedia.org/wiki/International_System_of_Units

# Instructions
```zef install Physics::Unit``` and, conversely, ```zef uninstall Physics::Unit```

_for latest, go ```zef install https://github.com/librasteve/raku-Physics-Unit.git```_

# Synopsis - Physics::Unit

```perl6
#!/usr/bin/env raku 
use Physics::Unit;

#SYNOPSIS

# Define your own unit named "ff" 
my $ff = Unit.new( defn => 'furlong / fortnight', names => ['ff'] );
say "$ff";                      # 'ff' ... string context gives unit name
say $ff.type;                   # Speed inferred from defn
say $ff.canonical;              # 'm.s-1' SI derived unit representation
say $ff.pretty;                 # 'm⋅s⁻¹' SI recommended string representation
say $ff.raku;                   # or say $ff; or dd $ff; for object details

#Unit.new( factor => 0.00016631, offset => 0, defn => 'furlong / fortnight', type => Speed,
#             dims => [1,0,-1,0,0,0,0,0], dmix => ("fortnight"=>-1,"furlong"=>1).MixHash, names => ['ff'] );

# New Unit by renaming an existing one 
my $fh = $ff.new( <fh fi> );

# Flexible unit expression (here using the newly defined Unit 'ff'):
my $gonzo = Unit.new( defn => "13 square millimeters per ff", names => ['gonzo'] );

# Parsing of input  
my $u1 = GetUnit( 'J' );
my $u2 = GetUnit( 'kg m^2 / s^2' );
my $u3 = GetUnit( 'kg m^2/s^2' );  
say ~$u3;                                             # 'J'
say "compare $u1, $u2... " ~ $u2.same-dims($u1);      # 1 (ie. same)

put ListUnits().sort;                # shows predefined and user defined Units
```

The ① symbol is used to denote Dimensionless units.

% (1) A ABV ABVs Bq C Centigrade Centigrades F Fahrenheit Fahrenheits GeV GeVs Gy H Hz J K L MeV MeVs Mx N Nm Ns PS Pa Rankine Rankines S Sv T TeV TeVs V W Wb acre acres alcohol-unit alcohol-units amp ampere amperes amps ampère ampères angstrom angstroms are ares astronomical-unit astronomical-units atm atmosphere atmospheres atms atomic-mass-unit atomic-mass-units au bar barn barns barrel barrels bars becquerel becquerels bottle bottles british-thermal-unit british-thermal-units btu btus bushel bushels ca cable cables cal calorie calories cals candela candelas candle candles carat carats cc cd celsius centuries century centurys coulomb coulombs cup cups cycle cycles day days deg degree degrees degs deg² deg²s demi demis dram drams dyne dynes eV earth-gravity earth-gravitys electron-volt electron-volts erg ergs farad farads fathom fathoms feet feets ff fh fi fifth fifths firkin firkins floz fluid-ounce fluid-ounces fluidram fluidrams foot footpound footpounds foots fortnight fortnights fps ft ft-lb ft-lbs furlong furlongs g g0 gallon gallons gauss gill gills gm gon gons gonzo grain grains gram gram-force gram-forces gramme grammes grams gray grays hectare hectares hemi hemis henry henrys hertz horsepower horsepowers hour hours hp hr hundredweight hundredweights imp-gallon imp-gallons imp-pint imp-pints in inch inchs j-point j-points joule joules kWh kWhs karat karats kat katal katals kats kcal kcals kelvin kelvins kg kg m/s kg m^2 kg m^2 / s^2 kg m^2/s kg m^2/s^2 kg/m^3 kgf kgfs kilogram kilograms km knot knots kph kphs kps l lb lbm lbms lbs light-year light-years liter liters litre litres lm long-ton long-tons lumen lumens lux luxs lx ly m m/s m/s^2 m2 m3 m^2 m^3 maxwell maxwells meter meters metre metres metric-ton metric-tons mho mhos micron microns mile miles millenia millenias millenium milleniums min minim minims mins minute minutes mol mole moles mols month months mph mphs m² m³ nautical-mile nautical-miles newton newton-metre newton-metres newtons nmile nmiles ohm ohms one ones ounce ounce-force ounce-forces ounces oz parsec parsecs pascal pascals peck pecks pennyweight pennyweights percent percents perch perchs pi pica picas pint pints point points pole poles pound pound-force pound-forces pound-second pound-seconds pounds pounds-mass psi psis quart quarts rad radian radians radians per second radians per seconds rads rem rems revolution revolutions revolutions per second revolutions per seconds revs rod rods rpm rpms s scruple scruples sec second seconds secs semi semis short-ton short-tons siemens sievert sieverts slug slug ft/s slugs sp spat spats sr steradian steradians stone stones tablespoon tablespoons tbsp tbsps teaspoon teaspoons tesla teslas therm therms ton tonne tonnes tons torr torrs troy-ounce troy-ounces troy-pound troy-pounds tsp tsps u unity unitys us-gallon us-gallons us-horsepower us-horsepowers us-hp us-hps us-pint us-pints volt volts watt watts weber webers week weeks yard yards year years yr ° °C °F °R °proof °proofs º å Ω μ ᵍ ①

Copyright (c) Henley CLoud Consulting Ltd. 2021-2023
