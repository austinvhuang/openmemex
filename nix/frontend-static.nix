{
  npmlock2nix,
  version,
  commit,
}:
npmlock2nix.build {
  src = ../.;
  installPhase = "cp -r dist $out";
  preBuild = ''
    # substituteInPlace webpack.config.js \
    # --replace 'gitRevisionPlugin.version()' '"${version}"' \
    # --replace 'gitRevisionPlugin.commithash()' '"${commit}"'
  '';
  buildCommands = ["npm run build"];
}
