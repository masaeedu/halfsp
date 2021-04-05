let
  sources = import ./nix/sources.nix;
  compilerVersion = import ./compiler.nix;
  hnix = import sources.iohk-hnix {};
  pkgs = (import hnix.sources.nixpkgs) hnix.nixpkgsArgs;
  hls = import sources.nix-haskell-hls { ghcVersion = compilerVersion; hlsUnstable = true; };
  snapshot = import ./snapshot.nix;
in
(import ./.).shellFor {
  withHoogle = true;
  buildInputs = [
    hls.hls-renamed
    hls.hls-wrapper
    (pkgs.haskell-nix.tool compilerVersion "hpack"         { index-state = snapshot; version = "0.34.4"; })
    (pkgs.haskell-nix.tool compilerVersion "cabal-install" { index-state = snapshot; version = "3.4.0.0"; })
    (pkgs.haskell-nix.tool compilerVersion "ghcid"         { index-state = snapshot; version = "0.8.7";  })
  ];
}
