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
    );

    # main Loader
    method load {
        require Physics::Unit::en_SI;

        #| also specified in Build.rakumod
        my $pul = Physics::Unit::en_SI.new(
            raph => '.raph-config',
            path => 'Unit/Definitions/en_SI',
            parts => <base types dims derived prefix general>,
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

