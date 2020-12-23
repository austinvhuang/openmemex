install-tools:
	stack install ormolu ghcid

server-watcher:
	~/.local/bin/ghcid -c "stack ghci note2self:exe:server" -Tmain
	
