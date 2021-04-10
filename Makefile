install-tools:
	stack install ormolu ghcid

cli-watcher:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:n2s" -Tmain

server-watcher:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:server" -Tmain

crawler-run:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:crawler" -Tmain

crawler-build:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:crawler"

thumbnails:
	crawler/Thumbnails.hs

clean-backups:
	rm note2self.db.backup.20*.db

libtorch-mac:
	rm -rf ./libtorch
	rm -f cpu-libtorch-macos-latest.zip
	wget https://github.com/hasktorch/libtorch-binary-for-ci/releases/download/1.7.0/cpu-libtorch-macos-latest.zip
	unzip cpu-libtorch-macos-latest.zip
	rm -f cpu-libtorch-macos-latest.zip

libtorch-linux:
	rm -rf ./libtorch
	rm -f cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip
	wget https://github.com/hasktorch/libtorch-binary-for-ci/releases/download/1.7.0/cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip
	unzip cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip
	rm -f cpu-libtorch-cxx11-abi-shared-with-deps-latest.zip

model-linux: libtorch-linux
	export LD_LIBRARY_PATH=`pwd`/libtorch/lib; stack run model

model-mac-build: 
	python model/test_export.py
	rm -rf ./.stack-work/install
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

install-dependencies:
	sudo apt install tesseract-ocr
	sudo apt install libtesseract-dev
	sudo apt-install imagemagick
	sudo apt install libva-dev
	sudo snap install chromium

test-screenshot:
	# chromium --headless --disable-gpu --screenshot=deleteme.png --window-size=600,800 --force-device-scale-factor=4.0 https://www.yahoo.com
	chromium --headless --disable-gpu --screenshot=deleteme.png --window-size=600,800 --hide-scrollbars https://www.yahoo.com
	xdg-open deleteme.png

test-post:
	curl -g --header "Content-Type: application/json" --request POST --data '{"pnContent":"https://monoskop.org/images/5/51/Wiener_Norbert_The_Human_Use_of_Human_Beings.pdf", "pnTags":["book"]}' --request POST http://localhost:3000/submit/note
