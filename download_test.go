package main

import (
	"strings"
	"testing"
)

func TestParseSamples(t *testing.T) {
	html := `
	<div class="part">
	<section>
	<h3>入力例 1</h3><pre>10
	</pre>
	</section>
	</div>

	<div class="part">
	<section>
	<h3>出力例 1</h3><pre>40
	</pre>
	</section>
	</div>
	`
	r := strings.NewReader(html)
	samples, err := parseSamples(r)
	if err != nil {
		t.Fatal(err)
	}
	if len(samples) != 1 || samples[0].in != "10" || samples[0].out != "40" {
		t.Errorf("unexpected result: %+v", samples)
	}
}
