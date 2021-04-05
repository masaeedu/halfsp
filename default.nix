let
  sources = import ./nix/sources.nix;
  compilerVersion = import ./compiler.nix;
  hnix = import sources.iohk-hnix {};
  pkgs = (import hnix.sources.nixpkgs) hnix.nixpkgsArgs;
in
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  compiler-nix-name = compilerVersion;
}
