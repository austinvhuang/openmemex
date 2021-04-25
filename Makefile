# Dependencies ##################################################

install-tools:
	stack install ormolu ghcid

install-dependencies:
	sudo apt install libtesseract-dev
	sudo apt install tesseract-ocr
	sudo apt-install imagemagick
	sudo apt install libva-dev
	sudo snap install chromium

clean-backups:
	rm note2self.db.backup.20*.db

download-libtorch-mac:
	rm -rf ./libtorch
	rm -f cpu-libtorch-macos-latest.zip
	wget https://github.com/hasktorch/libtorch-binary-for-ci/releases/download/1.8.0/cpu-libtorch-macos-latest.zip
	unzip cpu-libtorch-macos-latest.zip
	rm -f cpu-libtorch-macos-latest.zip

libtorch/lib/libtorch_cpu.so:
	rm -rf ./libtorch
	rm -f cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip
	wget https://github.com/hasktorch/libtorch-binary-for-ci/releases/download/1.8.0/cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip
	unzip cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip
	rm -f cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip

download-libtorch-linux: libtorch/lib/libtorch_cpu.so

# Builds ##################################################

cli-watcher:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:n2s" -Tmain

frontend-rs/static/wasm_bg.wasm:
	cd frontend-rs; make build

# run `source setenv` before invoking this
watch-server: frontend-rs/static/wasm_bg.wasm libtorch/lib/libtorch_cpu.so
	~/.local/bin/ghcid -c "stack ghci note2self:exe:server" -Tmain 
	
watch-crawler:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:crawler" -Tmain

watch-crawler-build:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:crawler"

thumbnails:
	crawler/Thumbnails.hs

roberta_traced.zip: 
	./get_roberta.sh
	python model/test_export.py

# Tests ##################################################

# model-test-linux: libtorch-linux
test-model-linux: libtorch/lib/libtorch_cpu.so roberta_traced.zip
	export LD_LIBRARY_PATH=`pwd`/libtorch/lib; stack run model

# TODO - is this still needed?
test-model-mac-build: download-libtorch-mac roberta_traced.zip
	# rm -rf ./.stack-work/install
	stack build model

	# this should fail
	stack run model || true

	# patch the rpath
	otool -l `stack exec -- which model` > otool.log.pre
	install_name_tool -add_rpath libtorch/lib `stack exec -- which model`
	otool -l `stack exec -- which model` > otool.log.post

	# see https://unix.stackexchange.com/questions/100786/why-does-diff-fail-when-invoked-from-a-makefile
	diff otool.log.pre otool.log.post; [ $$? -eq 1 ] 

	stack run model

test-screenshot:
	# chromium --headless --disable-gpu --screenshot=deleteme.png --window-size=600,800 --force-device-scale-factor=4.0 https://www.yahoo.com
	chromium --headless --disable-gpu --screenshot=deleteme.png --window-size=600,800 --hide-scrollbars https://www.yahoo.com
	xdg-open deleteme.png

test-post-note:
	curl -g --header "Content-Type: application/json" --request POST --data '{"pnContent":"https://monoskop.org/images/5/51/Wiener_Norbert_The_Human_Use_of_Human_Beings.pdf", "pnTags":["book"]}' --request POST http://localhost:3000/submit/note
