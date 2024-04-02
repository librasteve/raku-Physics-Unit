class Build {
	method build($dist-path) {
		my $raph = '.raph-config';

		#| mkdir will use existing if present
		mkdir "$*HOME/$raph";
		mkdir "$*HOME/$raph/Unit";
		mkdir "$*HOME/$raph/Unit/Definitions";
		mkdir "$*HOME/$raph/Unit/Definitions/en_SI";

		my $path = 'Unit/Definitions/en_SI';
		my @parts = <base types dims derived prefix general>;

		for @parts -> $part {
				copy "resources/$path/$part.yaml", "$*HOME/$raph/$path/$part.yaml";
		}

		exit 0
	}
}
