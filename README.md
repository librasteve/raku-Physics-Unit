...put travis ci here...

#WORK IN PROGRESS - PLEASE IGNORE
# raku-Physics-Unit
A set of SI, Imperial and US Unit objects that can be consumed by Physics::Measure objects.

# Instructions
zef install --verbose https://github.com/p6steve/raku-Physics-Unit.git

and, conversely, zef uninstall Physics::Unit

# Synopsis - Physics::Unit

```perl6
#!/usr/bin/env raku
use Physics::Unit;

#SYNOPSIS

#Unit objects can be selected or created with GetUnit:
    my Unit   $u  = GetUnit( 'm' );
#Define your own unit named "ff" (named args)
    my $ff = Unit.new( defn => 'furlong / fortnight', names => ['ff'] );

#EOF
```
