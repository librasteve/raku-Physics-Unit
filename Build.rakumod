class Build {
	method build($dist-path) {
		#| also specified in en_SI.rakumod
		my $raph = '.raph-config';
		my $path = 'Unit/Definitions/en_SI';
		my @parts = <base types dims derived prefix general>;

		#| mkdir will use existing if present
		mkdir "$*HOME/$raph";
		mkdir "$*HOME/$raph/Unit";
		mkdir "$*HOME/$raph/Unit/Definitions";
		mkdir "$*HOME/$raph/Unit/Definitions/en_SI";

		for @parts -> $part {
				copy "resources/$path/$part.yaml", "$*HOME/$raph/$path/$part.yaml";
		}

		exit 0
	}
}
