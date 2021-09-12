build:
	wasm-pack build --target web --out-name wasm --out-dir ./static

watch-dev:
	rg -t rust -t toml --files | entr -s "wasm-pack build --target web --out-name wasm --dev --out-dir ./static"

watch-rel:
	rg -t rust -t toml --files | entr -s "wasm-pack build --target web --out-name wasm --release --out-dir ./static"

watch-prof:
	rg -t rust -t toml --files | entr -s "wasm-pack build --target web --out-name wasm --profiling --out-dir ./static"

watch: watch-dev

fmt:
	rg --files | entr -s "cargo fmt"

serve: build
	# have a server process running before serving the frontend `cd ../;stack run server` 
	miniserve ./static/

