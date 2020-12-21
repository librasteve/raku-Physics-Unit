#!/usr/bin/env raku
#t/01-tst.t 
#TESTALL$ prove6 ./t      [from root]
use lib '../lib';
use Test;
plan 21; 

use Physics::Unit;

#derived-unit-names
my @d-u-n;

sub InitDerivedUnit( @_ ) {
	for @_ -> $names, $defn {
		@d-u-n.push: $names[1];
	}
}
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
    ['°C', 'celsius', 'Centigrade'],        'K + 273.15',   #Temperature
    ['lm', 'lumen'],                        'cd sr',
    ['lx', 'lux'],                          'm^-2 cd',
    ['Bq', 'becquerel'],                    '1 Hz',
    ['Gy', 'gray'],                         'J / kg',
    ['Sv', 'sievert'],                      'J / kg',
    ['kat', 'katal'],                       'mol s^-1',
    #SI coherent Derived Units in terms of base units - TBD [see url]
    #SI coherent Derived Units that include units with special names - TBD [see url]
);

my @unit-types = <
Solid-Angle
Frequency
Force
Pressure
Energy
Power
Charge
Potential
Capacitance
Resistance
Conductance
Magnetic-Flux
Magnetic-Field
Inductance
Temperature
Luminous-Flux
Illuminance
Radioactivity
Dose
Dose
Catalytic-Activity
>;

for 0..^@d-u-n -> $i {
	is GetUnit(@d-u-n[$i]).type, @unit-types[$i], "{@d-u-n[$i]} => {@unit-types[$i]}";
}

#done-testing
