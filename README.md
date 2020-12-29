[![Build Status](https://travis-ci.com/p6steve/raku-Physics-Unit.svg?branch=master)](https://travis-ci.com/p6steve/raku-Physics-Unit)

# Physics::Unit
A set of stock SI, Imperial and US Unit objects for raku that are employed by Physics::Measure objects.

#viz. https://en.wikipedia.org/wiki/International_System_of_Units

See also Physics::UnitAffix for support of SI Prefixes.

# Instructions
zef install --verbose https://github.com/p6steve/raku-Physics-Unit.git

and, conversely, zef uninstall Physics::Unit

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
say $u3;
say "compare $u1, $u2... " ~ $u2.same-dims($u1);

put ListUnits();                # shows predefined and user defined Units
```
#`[[
m metre metres meter meters m kg kilogram kilograms kg s sec secs second seconds s A amp amps ampere amperes ampère ampères A K kelvin kelvins K mol mols mole moles mol cd candela candelas candle candles cd radian radians radian sr steradian steradians Hz hertz N newton newtons Pa pascal pascals J joule joules W watt watts C coulomb coulombs V volt volts F farad farads Ω ohm ohms S siemens Wb weber webers T tesla teslas H henry henrys °C celsius Centigrade Centigrades lm lumen lumens lx lux luxs Bq becquerel becquerels Gy gray grays Sv sievert sieverts kat katal kats katals one unity ones unitys semi demi hemi semis demis hemis % percent percents ABV ABVs pi ° degree deg º degrees degs ᵍ gon gons deg² deg²s sp spat spats min minute mins minutes hr hour hours day days week weeks fortnight fortnights yr year years month months century centuries centurys millenium millenia milleniums millenias cycle cycles revolution revolutions rpm rpms km μ micron microns å angstrom angstroms au astronomical-unit astronomical-units ly light-year light-years parsec parsecs ft foot feet foots feets in inch inchs yard yards fathom fathoms rod pole perch rods poles perchs furlong furlongs mile miles nmile nautical-mile nmiles nautical-miles ca cable cables pica picas point points m^2 m2 m² are ares hectare hectares barn barns acre acres m^3 m3 m³ l L litre liter litres liters cc bottle bottles fluidram fluidrams minim minims alcohol-unit alcohol-units us-gallon us-gallons imp-gallon imp-gallons gallon gallons firkin firkins barrel barrels quart quarts peck pecks bushel bushels fifth fifths us-pint us-pints imp-pint imp-pints pint pints cup cups floz fluid-ounce fluid-ounces gill gills tablespoon tbsp tablespoons tbsps teaspoon tsp teaspoons tsps m/s mph mphs kph kphs kps fps knot knots radians per second radians per seconds revs revolutions per second revolutions per seconds rpm rpms m/s^2 g0 earth-gravity earth-gravitys g gram gm gramme grams grammes u atomic-mass-unit atomic-mass-units metric-ton tonne metric-tons tonnes grain grains lbm pounds-mass lbms oz ounce ounces stone stones hundredweight hundredweights ton short-ton tons short-tons long-ton long-tons slug slugs dram drams troy-pound troy-pounds troy-ounce troy-ounces pennyweight pennyweights scruple scruples carat karat carats karats j-point j-points kg m^2 kg m/s slug ft/s kg m^2/s lb lbs pound pound-force pounds pound-forces ounce-force ounce-forces dyne dynes gram-force gram-forces kgf kgfs Nm newton-metre newton-metres ft-lb footpound ft-lbs footpounds Ns pound-second pound-seconds bar bars torr torrs psi psis atm atmosphere atms atmospheres kg/m^3 °proof °proofs eV electron-volt electron-volts MeV MeVs GeV GeVs TeV TeVs cal calorie cals calories kcal kcals btu british-thermal-unit btus british-thermal-units therm therms erg ergs kWh kWhs us-horsepower us-hp us-horsepowers us-hps PS horsepower hp horsepowers mho mhos Mx maxwell maxwells gauss °R Rankine Rankines °F Fahrenheit Fahrenheits rad rads rem rems ft foot feet foots feets yard yards rod pole perch rods poles perchs furlong furlongs min minute mins minutes hr hour hours day days week weeks fortnight fortnights ff fh fi gonzo J joule joules kg m^2 / s^2 kg m^2/s^2
#]]

