class Config {
    #debug (also see Grammar::Tracer line in Parser)
    has $.db = 0;

    #Power synonyms
    has %.pwr-preword = (square => 2, sq => 2, cubic => 3, cu => 3);
    has %.pwr-postword = (squared => 2, cubed => 3,);

    #Power superscripts eg. x¹ x² x³ x⁴ x⁻¹ x⁻² x⁻³ x⁻⁴
    has %.pwr-superscript = (
    '¹' => 1, '²' => 2, '³' => 3, '⁴' => 4,
    '⁻¹' => -1, '⁻²' => -2, '⁻³' => -3, '⁻⁴' => -4,
    );

    # locale for 'us' or 'imp' gallons, pints, mpg etc.
    has $.locale = 'imp';

    submethod TWEAK{
        $!locale = ($_ ~~ /en_US/) ?? 'us' !! 'imp'   with %*ENV<LANG>;
        $!locale = ($_ ~~ /en_US/) ?? 'us' !! $!locale with %*ENV<RAKULANG>;
    }
}

