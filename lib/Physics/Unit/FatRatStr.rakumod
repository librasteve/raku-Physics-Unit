use MONKEY-TYPING;

grammar DecimalExponent {
    token TOP      { ^ <number> $ }
    token number   { <sign>? <whole> [ '.' <decimal>+ ]? <[eE]> <exponent>? }

    token whole    { <[0..9]>+ }
    token decimal  { <[0..9]>+ }
    token exponent { <sign>? <[0..9]>+ }
    token sign     { <[+-]> }
}

class DecimalActions {
    method TOP($/) {
        make $<number>.made;
    }

    method number($/) {
        my $sign = 1;
        with $<sign> {
            if .Str eq '-' { $sign = -1 }
        }

        my $whole    = $<whole>.Str;
        my $decimal  = $<decimal>.Str;
        my $exponent = $<exponent>.Str;

        my $adjust   = $decimal.chars;
        my $shift    = $adjust - $exponent;

        my $numerator   = $sign * ( ( $whole * 10**$adjust ) + $decimal);
        my $denominator = 10**$shift;

        make FatRat.new($numerator, $denominator);
    }
}

class FatRatStr {
    has FatRat $!fatrat is built handles <nude FatRat>;
    has Str    $!str    is built handles <Str>;
}

use MONKEY-TYPING;

augment class NumStr {
    method FatRatStr(NumStr:D: --> FatRatStr:D) {
        my $m = DecimalExponent.parse(self.Str, :actions(DecimalActions));
        FatRatStr.new(fatrat => $m.made, str => self.Str);
    }
}

augment class Str {
    method FatRatStr(Str:D: --> FatRatStr:D) {
        my $m = DecimalExponent.parse(self, :actions(DecimalActions));
        FatRatStr.new(fatrat => $m.made, str => self);
    }
}
