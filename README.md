[![License: Artistic-2.0](https://img.shields.io/badge/License-Artistic%202.0-0298c3.svg)](https://opensource.org/licenses/Artistic-2.0)
[![raku-physics-unit -> DH](https://github.com/librasteve/raku-Physics-Unit/actions/workflows/unit-weekly.yaml/badge.svg)](https://github.com/librasteve/raku-Physics-Unit/actions/workflows/unit-weekly.yaml)


# Version 2

This version of Physics::Unit is a breaking change and a major refactor of the configuration files, build and load process to enable pluggable definitions. All configuration data is now loaded from resource YAML files, Config.rakumod and en_SI.rakumod. _See below for long term goals._

Synthetic Units (e.g. .type => ```synthetic:m-1``) allows Measure calculations to produce intermediate results without dredefined units for every intermediate step.

CreateUnit is consolidated to Unit.new, GetUnit becomes Unit.find and all non method parts of API are replaced.

*You will need to upgrade Physics::Measure to v2+ also*

---

# Physics::Unit
A set of stock SI, Imperial and US Unit objects for raku that are employed by [Physics::Measure](https://github.com/librasteve/raku-Physics-Measure) objects.

#viz. https://en.wikipedia.org/wiki/International_System_of_Units

# Instructions
```zef install Physics::Unit```

The ```locale``` for units is set to 'us' or 'imp' (imperial) automatically depending on your ```%*ENV<LANG>``` system value.
You can override this with ```export RAKULAND='en_US'``` or ```export RAKULAND='anything else'```.
This affects vanilla gallons, pints, mpg, etc. so don't worry if you are from Europe.
You can always use ```imp-pint``` vs ```us-pint``` explicitly to override.

# Synopsis - Physics::Unit

```perl6
#!/usr/bin/env raku 
use Physics::Unit;

#SYNOPSIS

# Define your own Unit named "ff"
my $ff = Unit.new( defn => 'furlong / fortnight', names => ['ff'] );
say '==============';
dd $ff;
say "$ff";				# 'ff' ... string context gives unit name
say $ff.type;			# 'Speed' inferred from defn
say $ff.canonical;		# 'm.s-1' SI derived unit representation
say $ff.pretty;			# 'm⋅s⁻¹' SI recommended string representation
say $ff.raku;			#  or 'say $ff;' or 'dd $ff;' for details

#Unit.new( factor => 0.00016631, offset => 0, defn => 'furlong / fortnight', type => Speed,
#			 dims => [1,0,-1,0,0,0,0,0], dmix => ("fortnight"=>-1,"furlong"=>1).MixHash, names => ['ff'] );

# New Unit by renaming an existing one
my $fh = $ff.new( <fh fi> );

# Flexible unit expression (here using the newly defined Unit 'ff'):
my $gonzo = Unit.new( defn => "13 square millimeters per ff", names => ['gonzo'] );

# Parsing of input
my $u1 = Unit.find( 'J' );
my $u2 = Unit.find( 'kg m^2 / s^2' );
my $u3 = Unit.find( 'kg m^2/s^2' );
say "compare $u1, $u2... " ~ $u2.same-unit($u1);   #True

# Check directory information
use Physics::Unit::Directory;

my $dx := Directory.instance;
put $dx.type.names;
put $dx.unit.names;
```

The ① symbol is used to denote Dimensionless units.

% (1) A ABV ABVs Bq C Centigrade Centigrades F Fahrenheit Fahrenheits GeV GeVs Gy H Hz J K L MeV MeVs Mx N Nm Ns PS Pa Rankine Rankines S Sv T TeV TeVs V W Wb acre acres alcohol-unit alcohol-units amp ampere amperes amps ampère ampères angstrom angstroms are ares astronomical-unit astronomical-units atm atmosphere atmospheres atms atomic-mass-unit atomic-mass-units au bar barn barns barrel barrels bars becquerel becquerels bottle bottles british-thermal-unit british-thermal-units btu btus bushel bushels ca cable cables cal calorie calories cals candela candelas candle candles carat carats cc cd celsius centuries century centurys coulomb coulombs cup cups cycle cycles day days deg degree degrees degs deg² deg²s demi demis dram drams dyne dynes eV earth-gravity earth-gravitys electron-volt electron-volts erg ergs farad farads fathom fathoms feet feets ff fh fi fifth fifths firkin firkins floz fluid-ounce fluid-ounces fluidram fluidrams foot footpound footpounds foots fortnight fortnights fps ft ft-lb ft-lbs furlong furlongs g g0 gallon gallons gauss gill gills gm gon gons gonzo grain grains gram gram-force gram-forces gramme grammes grams gray grays hectare hectares hemi hemis henry henrys hertz horsepower horsepowers hour hours hp hr hundredweight hundredweights imp-gallon imp-gallons imp-pint imp-pints in inch inchs j-point j-points joule joules kWh kWhs karat karats kat katal katals kats kcal kcals kelvin kelvins kg kg m/s kg m^2 kg m^2 / s^2 kg m^2/s kg m^2/s^2 kg/m^3 kgf kgfs kilogram kilograms km knot knots kph kphs kps l lb lbm lbms lbs light-year light-years liter liters litre litres lm long-ton long-tons lumen lumens lux luxs lx ly m m/s m/s^2 m2 m3 m^2 m^3 maxwell maxwells meter meters metre metres metric-ton metric-tons mho mhos micron microns mile miles millenia millenias millenium milleniums min minim minims mins minute minutes mol mole moles mols month months mph mphs m² m³ nautical-mile nautical-miles newton newton-metre newton-metres newtons nmile nmiles ohm ohms one ones ounce ounce-force ounce-forces ounces oz parsec parsecs pascal pascals peck pecks pennyweight pennyweights percent percents perch perchs pi pica picas pint pints point points pole poles pound pound-force pound-forces pound-second pound-seconds pounds pounds-mass psi psis quart quarts rad radian radians radians per second radians per seconds rads rem rems revolution revolutions revolutions per second revolutions per seconds revs rod rods rpm rpms s scruple scruples sec second seconds secs semi semis short-ton short-tons siemens sievert sieverts slug slug ft/s slugs sp spat spats sr steradian steradians stone stones tablespoon tablespoons tbsp tbsps teaspoon teaspoons tesla teslas therm therms ton tonne tonnes tons torr torrs troy-ounce troy-ounces troy-pound troy-pounds tsp tsps u unity unitys us-gallon us-gallons us-horsepower us-horsepowers us-hp us-hps us-pint us-pints volt volts watt watts weber webers week weeks yard yards year years yr ° °C °F °R °proof °proofs º å Ω μ ᵍ ①

# Operation Principles & Edge Cases

## Operation Principles
1. Definition can be shared with several unit names - thus J and Nm can remain distinct
1. Unit.find first tries name match, then calls Unit.new to try definition match
1. Matching names is exact; definitions is loose
1. Defn matches dimension Mix (shallow) - thus 'kg m^2 / s^2' and 'N m' do remain distinct
1. Long strings (eg. 'kg m^2/s^2') auto reduce to SI derived units (eg. 'J') [if Unit already instaniated]
1. Override with Unit.new(defn=>'kg m^2/s^2', names=>['kg m^2/s^2']);

## Edge Cases
Unit.find('J');           #stock unit name=>'J'.., type=>Energy, defn=>'kg m^2 / s^2'
Unit.find('kg m^2/s^2');  #same  unit name=>'J'.., type=>Energy, defn=>'kg m^2 / s^2'
Unit.find('kg m^2 / s^2');#same  unit name=>'J'.., type=>Energy, defn=>'kg m^2 / s^2'
Unit.find('Nm');          #stock unit name=>'Nm',  type=>Torque, defn=>'N m'
Unit.find('N m');         #same  unit name=>'Nm',  type=>Torque, defn=>'N m'
Unit.new(defn=>'kg m^2/s^2', names=>['kg m^2/s^2']);
Unit.find('kg m^2/s^2');  #new unit name=>''kg m^2/s^2', type=>Energy,Torque, defn=>'kg m^2/s^2'
.type('Energy');    #This establishes the type=>Energy once and for all
Unit.find('kg m^2/s^2');  #same unit name=>'kg m^2/s^2', type=>Energy, defn='kg m^2/s^2'

# Long Term Goals

## Next

1. Split SI only from others
1. Currency
1. Information
1. Split en_US / en_GB
1. Log units (dB, etc)


## Pluggability

Leverage the raku zef module ecosystem to handle dependencies.

In pursuit of:
- Speed of core definitions
- More independence en_US vs en_GB
- New types of definitions (currency, information)
- Enabling 3rd party extensions
- Internationalization
- Multiple systems and domains (see pint _default_en.txt_)
  - en_SI
  - en_Liquid (ABV, volumes)
  - en_Kitchen (spoons, etc)
  - en_Time
  - en_CGS
  - en_KMS   (? needed)
  - en_Avoirdupois
  - en_Astronomical
  - en_Earth (knot, atm)
  - en_Green
  - en_Chemistry

## Internationalization

```perl6
en_SI Base Units
#viz https://en.wikipedia.org/wiki/Dimensional_analysis#Definition
'Length'      => ['m', 'metre', 'meter',],
'Mass'        => ['kg', 'kilogram',],
'Time'        => ['s', 'sec', 'second',],
'Current'     => ['A', 'amp', 'ampere', 'ampère',],
'Temperature' => ['K', 'kelvin',],
'Substance'   => ['mol', 'mole',],
'Luminosity'  => ['cd', 'candela', 'candle',],
'Angle'		  => ['radian',],

fr_SI / Base, Derived
# Unités de base du SI
# Voir https://fr.wikipedia.org/wiki/Analyse_dimensionnelle#Définition
##revenir aux pluriels algorithmiques
'Longueur'    => ['m', 'mètre',],
'Masse'       => ['kg', 'kilogramme',],
'Temps'       => ['s', 'sec', 'seconde',],
'Courant'     => ['A', 'amp', 'ampère',],
'Température' => ['K', 'kelvin',],
'Substance'   => ['mol', 'mole',],
'Luminosité'  => ['cd', 'candela',],
'Angle'       => ['radian',],


de_SI / Base, Derived
# SI-Basiseinheiten
# Siehe https://de.wikipedia.org/wiki/Dimensionale_Analyse#Definition
##zurück zu algorithmischen Pluralformen
'Länge'        => ['m', 'Meter',],
'Masse'        => ['kg', 'Kilogramm',],
'Zeit'         => ['s', 'sek', 'Sekunde',],
'Stromstärke'  => ['A', 'Amp', 'Ampere',],
'Temperatur'   => ['K', 'Kelvin',],
'Stoffmenge'   => ['mol', 'Mol',],
'Lichtstärke'  => ['cd', 'Candela',],
'Winkel'       => ['Radiant',],

class Longueur is Length {}

```perl6
$ cat Physics.rakumod
unit module Physics;

my class Length {
has Int $.value;
}

my package EXPORT::en {
OUR::<Length> := Length;
}

my package EXPORT::de {
OUR::<Länge> := Length;
}
$ raku -I. -e 'use Physics :en; say Length;'
(Length)
$ raku -I. -e 'use Physics :de; say Länge;'
(Length)
```

# Others

- Improved plural automation (internationalization)
- Slangify (ie Physics::Measure)



Copyright (c) Henley Cloud Consulting Ltd. 2021-2024
