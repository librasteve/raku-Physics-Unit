#!/usr/bin/env raku
#t/01-tst.t 
#TESTALL$ prove6 ./t      [from root]
use lib '../lib';
use Test;
plan 12; 

use Physics::Unit;

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

#new Unit (positional args)
my $fg = Unit.new( defn => 'furlong / fortnight', names => ['fg'] );
is ~$fg,'fg',				'new positional ok'; 

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

#done-testing
