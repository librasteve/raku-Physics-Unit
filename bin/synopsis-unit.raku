#!/usr/bin/env raku 
use lib '../lib';
use Physics::Unit;

#SYNOPSIS

my $a1 = Unit.new( defn => '5 radians per second', names => ['radians per second'] ); say "$a1";
my $a2 = Unit.new( defn => '2 ft', names => ['2f'] ); say ~$a2; say $a2.type;

# Define your own unit named "ff" (named args)
my $ff = Unit.new( defn => 'furlong / fortnight', names => ['ff'] );
say "$ff";
say $ff.type;
say $ff.canonical;
say $ff.pretty;
say $ff.raku;
die "woah";

# New Unit by reference to an existing one
my $fh = $ff.new( <fh fi> );

# New Unit (positional args)
my $fg = Unit.new( defn => 'furlong / fortnight', names => ['fg'] );

# More intricate unit expression (using the newly defined unit 'ff'):
my $gonzo = Unit.new( defn => "13 square millimeters per ff", names => ['gonzo'] );

# Parsing of input  
my $u1 = GetUnit( 'J' );
my $u2 = GetUnit( 'kg m^2 / s^2' );
my $u3 = GetUnit( 'kg m^2/s^2' ); 
say "compare $u1, $u2... " ~ $u2.same-dims($u1);

# SI recommended string representation
say "{$u2.factor} {$u2.pretty}";

# SI derived unit representation
say "{$u2.factor} {$u2.canonical}";

# SI derived unit representation
say "{$u2.factor} {$u2.name}";

#put ListUnits();

#`[[
##### Principles & Behaviours ######
Principles
a. Definition is often not a stock unit name - thus J and Nm can remain distinct
b. GetUnit first tries name match, then calls CreateUnit to try definition match
c. Matching names is exact; definitions is loose
d. Defn matches dimension Mix (shallow) - thus 'kg m^2 / s^2' and 'N m' do remain distinct
e. Long strings (eg. 'kg m^2/s^2') auto compress to SI base / derived units (eg. 'J') 
f. Override with Unit.new(defn=>'kg m^2/s^2', names=>['kg m^2/s^2']);
 
Behaviours
GetUnit('J');           #stock unit name=>'J'.., type=>Energy, defn=>'kg m^2 / s^2' 
GetUnit('kg m^2/s^2');  #same  unit name=>'J'.., type=>Energy, defn=>'kg m^2 / s^2' 
GetUnit('kg m^2 / s^2');#same  unit name=>'J'.., type=>Energy, defn=>'kg m^2 / s^2' 
GetUnit('Nm');          #stock unit name=>'Nm',  type=>Torque, defn=>'N m'
GetUnit('N m');         #same  unit name=>'Nm',  type=>Torque, defn=>'N m'
    Unit.new(defn=>'kg m^2/s^2', names=>['kg m^2/s^2']);
GetUnit('kg m^2/s^2');  #new unit name=>''kg m^2/s^2', type=>Energy,Torque, defn=>'kg m^2/s^2'
    .type('Energy');    #This establishes the type=>Energy once and for all  
GetUnit('kg m^2/s^2');  #same unit name=>'kg m^2/s^2', type=>Energy, defn='kg m^2/s^2'
#]]

#EOF
