install-tools:
	stack install ormolu ghcid

server-watcher:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:server" -Tmain

prettier-watcher:
	rg *.vue *.js *.html --files | entr -s "npx prettier --write *.vue"

crawler-run:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:crawler" -Tmain

crawler-build:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:crawler"

clean-backups:
	rm note2self.db.backup.20*.db

vue-server:
	cd frontend/n2s; npm run serve

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

model-mac: 
	source setenv; stack run model
