#!/usr/bin/env raku
use v6.d;
use lib '../lib';

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

#EOF
