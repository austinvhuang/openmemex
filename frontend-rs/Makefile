build:
	wasm-pack build --target web --out-name wasm --out-dir ./static

watch:
	rg --files | entr -s "wasm-pack build --target web --out-name wasm --out-dir ./static"

fmt:
	rg --files | entr -s "cargo fmt"

serve: build
	# have a server process running before serving the frontend `cd ../;stack run server` 
	miniserve ./static/

