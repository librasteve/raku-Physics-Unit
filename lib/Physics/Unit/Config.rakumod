class Config {
    #debug (also see Grammar::Tracer line in Parser)
    has $.db = 0;

    #power synonyms
    has %.pwr-preword  = (square  => 2, sq => 2, cubic => 3, cu => 3,);
    has %.pwr-postword = (squared => 2,          cubed => 3,);

    #power superscripts eg. x¹ x² x³ x⁴ x⁻¹ x⁻² x⁻³ x⁻⁴
    has %.pwr-superscript = (
         '¹' =>  1,  '²' =>  2,  '³' =>  3,  '⁴' =>  4,
        '⁻¹' => -1, '⁻²' => -2, '⁻³' => -3, '⁻⁴' => -4,
    );

    #locale for 'us' or 'imp' gallons, pints, mpg etc.
    has $.locale = 'imp';

    #factor names
    has %.factor-names = (
        hundred         => 100,
        thousand        => 1000,
        million         => 1000000,
        billion         => 1000000000,
        trillion        => 1000000000000,
        quadrillion     => 1000000000000000,
        quintillion     => 1000000000000000000,
        sextillion      => 1000000000000000000000,
        septillion      => 1000000000000000000000000,
        octillion       => 1000000000000000000000000000,
        nonillion       => 1000000000000000000000000000000,
        decillion       => 1000000000000000000000000000000000,

        th => 1000,
        mn => 1000000,
        bn => 1000000000,
        tn => 1000000000000,

        hundredth       => <1/100>,
        thousandth      => <1/1000>,
        millionth       => <1/1000000>,
        billionth       => <1/1000000000>,
        trillionth      => <1/1000000000000>,
        quadrillionth   => <1/1000000000000000>,
        quintillionth   => <1/1000000000000000000>,
        sextillionth    => <1/1000000000000000000000>,
        septillionth    => <1/1000000000000000000000000>,
        octillionth     => <1/1000000000000000000000000000>,
        nonillionth     => <1/1000000000000000000000000000000>,
        decillionth     => <1/1000000000000000000000000000000000>,

        semi       => <1/2>,
        demi       => <1/2>,
        hemi       => <1/2>,
        sesqui     => <3/2>,   # one and a half
        bi         => 2,
        tri        => 3,
        quadri     => 4,
        whole      => 1,
        half       => <1/2>,
        quarter    => <1/4>,
        third      => <1/3>,
        fourth     => <1/4>,
        fifth      => <1/5>,
        sixth      => <1/6>,
        seventh    => <1/7>,
        eighth     => <1/8>,
        ninth      => <1/9>,
        tenth      => <1/10>,
    );

    submethod TWEAK{
        $!locale = ($_ ~~ /en_US/) ?? 'us' !! 'imp'    with %*ENV<LANG>;
        $!locale = ($_ ~~ /en_US/) ?? 'us' !! $!locale with %*ENV<RAKULANG>;
    }

    has %.type-hint = %(
        Area           => <Area FuelConsumption>,
        Energy         => <Energy Torque>,
        Momentum       => <Momentum Impulse>,
        Frequency      => <Frequency Radioactivity>,
        SpecificEnergy => <SpecificEnergy Dose>,
        Insolation     => <Insolation SpectralFluxDensity>,
    );

    # main Loader
    method load {
        require Physics::Unit::en_SI;

        #| also specified in Build.rakumod
        my $pul = Physics::Unit::en_SI.new(
            raph => '.raph-config',
            path => 'Unit/Definitions/en_SI',
            parts => <base types dims derived prefix general currency>,
        );

        $pul.load;
    }

    #some global type defs
    subset Defn   of Str is export;
    subset Type   of Str is export;
    subset Name   of Str is export;
    subset Symbol of Str is export;
    subset Names  of Array[Name] is export;
    subset Syns   of Array[Name] is export;
    subset Dims   of Array[Int]  is export;

}

