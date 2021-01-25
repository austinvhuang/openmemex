watch:
	rg --files | entr -s "wasm-pack build --target web --out-name wasm --out-dir ./static"

fmt:
	rg --files | entr -s "cargo fmt"

serve:
	miniserve ./static/
