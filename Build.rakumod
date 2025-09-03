class Build {
	method build($dist-path) {

        #| cp all yaml files from resources to local config dir
		#| also specified in en_SI.rakumod
		my $raph = '.raph-config';
		my $path = 'Unit/Definitions/en_SI';
		my @parts = <base types dims derived prefix general currency-template>;

		#| mkdir will use existing if present
		mkdir "$*HOME/$raph";
		mkdir "$*HOME/$raph/Unit";
		mkdir "$*HOME/$raph/Unit/Definitions";
		mkdir "$*HOME/$raph/Unit/Definitions/en_SI";

		for @parts -> $part {
			copy "resources/$path/$part.yaml", "$*HOME/$raph/$path/$part.yaml";
		}

        #| setup exchange rates
        note "Fetching currency exchange rates";
        note "***reinstall Physics::Unit for latest rates***";

        use HTTP::Tiny;
        use JSON::Fast;

        my $url = "https://latest.currency-api.pages.dev/v1/currencies/usd.json";

        my $response = HTTP::Tiny.new.get($url);
        warn "Failed to fetch $url" unless $response<success>;

        my $json-text = $response<content>.decode("utf-8");
        my $data = from-json($json-text);

        my %rates = $data<usd>.hash.map({ .key.uc => .value });

        #| parse currency-template, populate the rates
        #| generate currency.yaml and write back

        my @lines = "$*HOME/$raph/$path/currency-template.yaml".IO.lines;

        for @lines -> $line is rw {
            $line ~~ s/\'(\w+)\'/{ "{1 / %rates{$0}} USD" }/;
        }

        spurt "$*HOME/$raph/$path/currency.yaml".IO, @lines.join("\n");

        exit 0
    }
}
