[![License: Artistic-2.0](https://img.shields.io/badge/License-Artistic%202.0-0298c3.svg)](https://opensource.org/licenses/Artistic-2.0)
[![raku-physics-unit -> DH](https://github.com/librasteve/raku-Physics-Unit/actions/workflows/unit-weekly.yaml/badge.svg)](https://github.com/librasteve/raku-Physics-Unit/actions/workflows/unit-weekly.yaml)


# Version 2

## Issues / Opportunities
- Unit.find revert to Unit.new
- Measure Types to be parameterized? and international? - yep (breaks Nav)
- Measure[]
- consider 20 but <km> [again]
- Physics::Unit::Unit => Physics::Unit
- slangify
- synthetic units Type = u.WHICH  (valid identifier)  ??
  - my $u = Unit.find <ohm>; say $u.WHICH;   #Physics::Unit::Unit|4267563835920   #PU4267563835920
- https://raku.land/zef:lizmat/ObjectCache ??
- rework us vs imp

## Thoughts
- add synth units
  - need unique name/type pattern
- [x] add something like
  - Unit Factory
  - Unit Directory
  - Session class
- use lizmat role Cached
- turn data maps into Session / Directory services
- add currency dim
- add information dim
- package units
- [x] class Prefix, Base ... is Unit
- [ ] reinstate check-change & is-final / finalize (both .new multis / clone)  ??

## TODOS
- [ ] cc info to main (check preload = 1)
- [ ] general config (ie core definitions)


en_US - English (United States)
en_GB - English (United Kingdom)
fr_FR - French (France)
es_ES - Spanish (Spain)
de_DE - German (Germany)
it_IT - Italian (Italy)
pt_BR - Portuguese (Brazil)
zh_CN - Chinese (China)
ja_JP - Japanese (Japan)
ru_RU - Russian (Russia)
ko_KR - Korean (South Korea)
ar_SA - Arabic (Saudi Arabia)
hi_IN - Hindi (India)
tr_TR - Turkish (Turkey)
nl_NL - Dutch (Netherlands)
pl_PL - Polish (Poland)
sv_SE - Swedish (Sweden)
fi_FI - Finnish (Finland)
da_DK - Danish (Denmark)
no_NO - Norwegian (Norway)

en loads both UK and US variants

en_SI
en_Liquid (ABV, volumes)
en_Kitchen (spoons, etc)
en_Time
en_CGS
en_KMS   (? needed)
en_Avoirdupois
en_Astronomical
en_Earth (knot, atm)
en_Green
en_Chemistry

## Pint lessons
_default_en.txt_
- dimension meta formulae - discard (P::U auto induces this)
- dimension - base link -discard (P::U has this hardwired)
- add currency and information dimensions - then certain prefix KiB, MiB etc only applies to info
- consider "symbol" attr (later)
- defer any system stuff (cgs etc)
- defer log units (dB ...)
- underscore k_C stuff ignore
- lift semantics, but not syntax
- what about internationalization
  - ^^ LLMs
  - ^^ Type names

``` pint
# Energy
watt_hour = watt * hour = Wh = watthour
```

``` P::U
# Energy
['Wh', 'watt_hour', 'watthour'], 'watt * hour',
<Wh watt_hour watthour>, 'watt * hour',
```

```
Type => [
  [
    [names],     # .first is symbol
    'defn',
  ],
  [
    [names],     # .first is symbol
    'defn',
  ],
]
```

```
class Unit::Raw  {
  has Str $.type;
  has Str @.names;
  has Str $.defn;
}
```

these are the things that need to be pluggable...!
the inheritance / append method means that they are extensions to base

Core / Base, Derived, Prefix
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

fr_SI / Base, Derived
InitBaseUnit (
# Unités de base du SI
# Voir https://fr.wikipedia.org/wiki/Analyse_dimensionnelle#Définition
##FIXME revenir aux pluriels algorithmiques
'Longueur'    => ['m', 'mètre',],
'Masse'       => ['kg', 'kilogramme',],
'Temps'       => ['s', 'sec', 'seconde',],
'Courant'     => ['A', 'amp', 'ampère',],
'Température' => ['K', 'kelvin',],
'Substance'   => ['mol', 'mole',],
'Luminosité'  => ['cd', 'candela',],
'Angle'       => ['radian',],
);

de_SI / Base, Derived
InitBaseUnit (
# SI-Basiseinheiten
# Siehe https://de.wikipedia.org/wiki/Dimensionale_Analyse#Definition
##FIXME zurück zu algorithmischen Pluralformen
'Länge'        => ['m', 'Meter',],
'Masse'        => ['kg', 'Kilogramm',],
'Zeit'         => ['s', 'sek', 'Sekunde',],
'Stromstärke'  => ['A', 'Amp', 'Ampere',],
'Temperatur'   => ['K', 'Kelvin',],
'Stoffmenge'   => ['mol', 'Mol',],
'Lichtstärke'  => ['cd', 'Candela',],
'Winkel'       => ['Radiant',],
);

class Longueur is Length {}

Length.names.append: ['m', 'mètre',],



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
my $u1 = Unit.find( 'J' );
my $u2 = Unit.find( 'kg m^2 / s^2' );
my $u3 = Unit.find( 'kg m^2/s^2' );  
say ~$u3;                                             # 'J'
say "compare $u1, $u2... " ~ $u2.same-dims($u1);      # 1 (ie. same)

put ListUnits().sort;                # shows predefined and user defined Units
```

# FIXME bring in new synopsis 

The ① symbol is used to denote Dimensionless units.

% (1) A ABV ABVs Bq C Centigrade Centigrades F Fahrenheit Fahrenheits GeV GeVs Gy H Hz J K L MeV MeVs Mx N Nm Ns PS Pa Rankine Rankines S Sv T TeV TeVs V W Wb acre acres alcohol-unit alcohol-units amp ampere amperes amps ampère ampères angstrom angstroms are ares astronomical-unit astronomical-units atm atmosphere atmospheres atms atomic-mass-unit atomic-mass-units au bar barn barns barrel barrels bars becquerel becquerels bottle bottles british-thermal-unit british-thermal-units btu btus bushel bushels ca cable cables cal calorie calories cals candela candelas candle candles carat carats cc cd celsius centuries century centurys coulomb coulombs cup cups cycle cycles day days deg degree degrees degs deg² deg²s demi demis dram drams dyne dynes eV earth-gravity earth-gravitys electron-volt electron-volts erg ergs farad farads fathom fathoms feet feets ff fh fi fifth fifths firkin firkins floz fluid-ounce fluid-ounces fluidram fluidrams foot footpound footpounds foots fortnight fortnights fps ft ft-lb ft-lbs furlong furlongs g g0 gallon gallons gauss gill gills gm gon gons gonzo grain grains gram gram-force gram-forces gramme grammes grams gray grays hectare hectares hemi hemis henry henrys hertz horsepower horsepowers hour hours hp hr hundredweight hundredweights imp-gallon imp-gallons imp-pint imp-pints in inch inchs j-point j-points joule joules kWh kWhs karat karats kat katal katals kats kcal kcals kelvin kelvins kg kg m/s kg m^2 kg m^2 / s^2 kg m^2/s kg m^2/s^2 kg/m^3 kgf kgfs kilogram kilograms km knot knots kph kphs kps l lb lbm lbms lbs light-year light-years liter liters litre litres lm long-ton long-tons lumen lumens lux luxs lx ly m m/s m/s^2 m2 m3 m^2 m^3 maxwell maxwells meter meters metre metres metric-ton metric-tons mho mhos micron microns mile miles millenia millenias millenium milleniums min minim minims mins minute minutes mol mole moles mols month months mph mphs m² m³ nautical-mile nautical-miles newton newton-metre newton-metres newtons nmile nmiles ohm ohms one ones ounce ounce-force ounce-forces ounces oz parsec parsecs pascal pascals peck pecks pennyweight pennyweights percent percents perch perchs pi pica picas pint pints point points pole poles pound pound-force pound-forces pound-second pound-seconds pounds pounds-mass psi psis quart quarts rad radian radians radians per second radians per seconds rads rem rems revolution revolutions revolutions per second revolutions per seconds revs rod rods rpm rpms s scruple scruples sec second seconds secs semi semis short-ton short-tons siemens sievert sieverts slug slug ft/s slugs sp spat spats sr steradian steradians stone stones tablespoon tablespoons tbsp tbsps teaspoon teaspoons tesla teslas therm therms ton tonne tonnes tons torr torrs troy-ounce troy-ounces troy-pound troy-pounds tsp tsps u unity unitys us-gallon us-gallons us-horsepower us-horsepowers us-hp us-hps us-pint us-pints volt volts watt watts weber webers week weeks yard yards year years yr ° °C °F °R °proof °proofs º å Ω μ ᵍ ①

Copyright (c) Henley Cloud Consulting Ltd. 2021-2024
