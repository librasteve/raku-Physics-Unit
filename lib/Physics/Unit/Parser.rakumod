#| Definition Str Parser for Units

use Physics::Unit::Config;

role Parser[::Unit] {
    my $cg = Config.new;

    method parse($defn is copy, $dx) {
        # Generally defn parsing proceeds by finding elements which are themselves
        # Unit objects and then recursively getting them (which can invoice recursive
        # calls to this parser). The class method Unit.find is used to do this.

        #6.d faster regexes with Strings {<$str>} & slower with Arrays {<@arr>}
        $defn .= trim;
        $defn .= subst('%LOCALE%', $cg.locale);

        #| preprocess postfix units to extended defn - eg. cm to centimetre
        $defn = $dx.postfix.to-defn{$defn} // $defn;

        #| rm compound names from element unit-name match candidates (to force regen of dmix)
        my $unit-names = $dx.unit.to-defn.keys.grep({ !/<[\s*^./]>/ }).join('|');

        my $prefix-names = $dx.prefix.to-unit.keys.join('|');

        my $pwr-prewords = $cg.pwr-preword.keys.join('|');
        my $pwr-postwords = $cg.pwr-postword.keys.join('|');
        my $pwr-superscripts = $cg.pwr-superscript.keys.join('|');

        #escape quote non alphanum| characters...
        $unit-names ~~ s:g/ (<-[a..z A..Z 0..9 \|]>) / '$0' /;
        $pwr-superscripts ~~ s:g/ (<-[a..z A..Z 0..9 \|]>) / '$0' /;

#        use Grammar::Tracer;
        my grammar UnitGrammar {
            token TOP {
                ^  \s* <numerator=.compound>
                  [\s* <divider> \s* <denominator=.compound>]?
                  [\s* '+' \s* <offset>]? \s*                $
            }
            #offset '+' hardwired
            token divider { '/' || 'per' }
            token compound { <element>+ % <sep> }
            token sep { ['*' || '.' || ' *' || ' .' || ' ' || '⋅'] }
            token element { <factor> || <pnp-before> || <pnp-after> }

            token factor { <number> }
            token offset { <number> }
            token number {
                \S+ <?{ defined +"$/" }>
            }
            #get chars, assert coerce to Real via +

            token pnp-before {
                <pwr-before>  \s+? <prefix-name>
            }
            #pnp==prefix-name-power
            token pnp-after {
                <prefix-name> \s*? <pwr-after>?
            }

            token prefix-name {
                <prefix>? \s*? <name>
            }
            token prefix { <$prefix-names> }
            token name { <$unit-names> }

            token pwr-before { <$pwr-prewords> }
            token pwr-after { <pwr-postwd> || <pwr-supers> || <pwr-normal> }
            token pwr-postwd { <$pwr-postwords> }
            token pwr-supers { <$pwr-superscripts> }

            token pwr-normal {
                <pwr-symbol>? \s*? <pwr-digits>
            }
            token pwr-symbol { '**' || '^' }
            token pwr-digits {
                <[-+]>? <[1..4]>
            }
        }

        my class UnitActions {
            ##say "in xxx...", $/.made;  #<== handy debug line, paste just after make

            #| assemble result with math operations from numerator and denominator (&offset)
            method TOP($/) {
                my $nu = $<numerator>.made;
                my $de = $<denominator>.made;
                my $os = $<offset>.made;
                $nu.share($de) if $de;
                $nu.offset: +$os if $os;
                make $nu;
            }

            #| accumulates element Units using times
            method compound($/) {
                my $acc = Unit.new;
                for $<element>>>.made -> $x {
                    $acc.times($x);
                }
                make $acc;
            }

            #| makes a list of element units (either factor or prefix-name-power)
            method element($/) {
                my ($unit, $defn, $pwr);

                if $unit = $<factor>.made {
                    make $unit;
                } else {
                    $unit = $<pnp-before><prefix-name>.made<unit> ||
                        $<pnp-after><prefix-name>.made<unit>;
                    $defn = $<pnp-before><prefix-name>.made<defn> ||
                        $<pnp-after><prefix-name>.made<defn>;
                    $pwr = $<pnp-before><pwr-before>.made ||
                        $<pnp-after>.made || 1;
                    make $unit.raise($pwr, $defn);
                }
            }

            #| handle factor and offset matches
            method factor($/) {
                make Unit.new.times($/.Real);
            }
            method offset($/) {
                make $<number>;
            }

            #| make both unit and defn from prefix-name matches
            method prefix-name($/) {
                my $unit = $<name>.made<unit>;
                my $defn = $<name>.made<defn>;
                my $pfix = $<prefix>.made<unit>;
                $unit.times($pfix) if $pfix;
                make %(:$defn, :$unit);
            }
            method prefix($/) {
                make %( unit => Unit.find($/.Str).clone);

            }
            method name($/) {
                my $defn = $/.Str;
                my $unit = Unit.find($defn).clone;
                $unit.dmix = ∅.MixHash;
                make %(:$defn, :$unit);
            }

            #| extract (signed) power digits from various grammar options
            method pwr-before($/) {
                make $cg.pwr-preword{$/.Str};
            }
            method pnp-after($/) {
                make $<pwr-after><pwr-postwd>.made ||
                    $<pwr-after><pwr-supers>.made ||
                    $<pwr-after><pwr-normal>.made;
            }
            method pwr-postwd($/) {
                make $cg.pwr-postword{$/.Str}.Int;
            }
            method pwr-supers($/) {
                make $cg.pwr-superscript{$/.Str}.Int;
            }
            method pwr-normal($/) {
                make $<pwr-digits>.Int;
            }
        }

        my $match = UnitGrammar.parse($defn, :actions(UnitActions));

        if $match.so {
            my $made-unit = $match.made;
            $made-unit.defn: $defn;

            say "Made: $match => { $made-unit.raku }" if $cg.db;

            return $made-unit;
        } else {
            die "Couldn't parse defn Str $defn";
        }
    }
}

#EOF