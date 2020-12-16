#!/usr/bin/env raku
#t/01-tst.t 
#TESTALL$ prove6 ./t      [from root]
use lib '../lib';
use Test;
£plan 12; 

use Physics::Unit;

#`[
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
    'Angular-Speed'      => 'radians per second',
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
    'Density'            => 'kg/m^3',
    'Energy'             => 'joule',
    'Power'              => 'watt',
    'Charge'             => 'coulomb',
    'Potential'          => 'volt',
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

#new Constructor
my Unit $u .=new(defn => 'm', names => ['m']);
is $u.name,'m',				'$u.name exists'; 
is ~$u.type,'Length',		'$u.units made'; 

#new Definition, Str output formats
my $ff = Unit.new( defn => 'furlong / fortnight', names => ['ff'] );
is ~$ff,'ff',				'$ff defined'; 
is ~$ff.type,'Speed',		'$ff.type ok';
is ~$ff.canonical,'m.s-1',	'$ff.canonical ok';
is ~$ff.pretty,'m⋅s⁻¹',		'$ff.pretty ok';

#new Unit by reference to an existing one 
my $fh = $ff.new( <fh fi> );
is ~$fh,'fh',				'new by ref ok'; 

#new Unit (named args)
my $fg = Unit.new( defn => 'furlong / fortnight', names => ['fg'] );
is ~$fg,'fg',				'new named ok'; 

#more intricate unit expression (using the newly defined unit 'ff'):
my $gonzo = Unit.new( defn => "13 square millimeters per ff", names => ['gonzo'] );
is ~$gonzo,'gonzo',			'new intricate ok'; 

#parsing of input  
my $u1 = GetUnit( 'kg m^2 / s^2' );
my $u2 = GetUnit( 'kg m^2/s^2' );  
ok $u2.same-dims($u1),		'parse & cmp ok';

#SI recommended string representation
is "{$u1.factor} {$u1.pretty}",'1 m²⋅kg⋅s⁻²', 'SI recommended ok';

#SI derived unit representation
is "{$u1.factor} {$u1.name}",'1 J', 'SI derived ok';
#]

done-testing
