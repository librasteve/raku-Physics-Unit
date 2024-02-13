#!/usr/bin/env raku
use v6.d;
use lib '../lib';

use Data::Dump::Tree;

use Physics::Unit;

#SYNOPSIS

#`[
  Unit.new( factor => 1, offset => 0, defn => 'm', type => Length, dims => [1,0,0,0,0,0,0,0],
  dmix => ("m"=>1).MixHash, names => ['m','metre','meter','metres','meters'] );
#]


my $u = GetUnit <ohm>;
dd $u;

#die;
#say $u.WHICH;   #Physics::Unit::Unit|4267563835920   #PU4267563835920

say '==============';


#dd %Physics::Unit::defn-by-name;




# Define your own Unit named "ff"
my $ff = Unit.new( defn => 'furlong / fortnight', names => ['ff'] );
dd $ff;
say "$ff";				# 'ff' ... string context gives unit name
say $ff.type;			# 'Speed' inferred from defn
say $ff.canonical;		# 'm.s-1' SI derived unit representation
die;
say $ff.pretty;			# 'm⋅s⁻¹' SI recommended string representation
say $ff.raku;			#  or 'say $ff;' or 'dd $ff;' for details

#Unit.new( factor => 0.00016631, offset => 0, defn => 'furlong / fortnight', type => Speed,
#			  dims => [1,0,-1,0,0,0,0,0], dmix => ("fortnight"=>-1,"furlong"=>1).MixHash, names => ['ff'] );

# New Unit by renaming an existing one
my $fh = $ff.new( <fh fi> );

# Flexible unit expression (here using the newly defined Unit 'ff'):
my $gonzo = Unit.new( defn => "13 square millimeters per ff", names => ['gonzo'] );

# Parsing of input
my $u1 = GetUnit( 'J' );
my $u2 = GetUnit( 'kg m^2 / s^2' );
my $u3 = GetUnit( 'kg m^2/s^2' );
say ~$u3;
say "compare $u1, $u2... " ~ $u2.same-dims($u1);

put ListUnits().sort;

#`[[
##### Principles & Behaviours ######
Principles
a. Definition can be shared with several unit names - thus J and Nm can remain distinct
b. GetUnit first tries name match, then calls CreateUnit to try definition match
c. Matching names is exact; definitions is loose
d. Defn matches dimension Mix (shallow) - thus 'kg m^2 / s^2' and 'N m' do remain distinct
e. Long strings (eg. 'kg m^2/s^2') auto reduce to SI derived units (eg. 'J') [if Unit already instaniated]
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
