[![Build Status](https://travis-ci.com/p6steve/raku-Physics-Unit.svg?branch=master)](https://travis-ci.com/p6steve/raku-Physics-Unit)

# Physics::Unit
A set of stock SI, Imperial and US Unit objects for raku that are employed by Physics::Measure objects.

#viz. https://en.wikipedia.org/wiki/International_System_of_Units

# Instructions
zef install --verbose Physics::Unit
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
