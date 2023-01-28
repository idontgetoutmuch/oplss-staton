let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

      backprop = super.haskell.lib.dontCheck (
        hself.callCabal2nix "backprop" (builtins.fetchGit {
          url = "file:////Users/dom/oplss-staton/backprop";
          rev = "96ba7d37a6f583a3e018b8dab50691bdfa482dc8";
        }) { });

      # backprop = hself.callCabal2nix "backprop" (builtins.fetchGit {
      #     url = "https://github.com/idontgetoutmuch/backprop";
      #     rev = "96ba7d37a6f583a3e018b8dab50691bdfa482dc8";
      #   }) { };

    };
  };
};

in

{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-22.05-darwin.tar.gz")
  {
    config.allowBroken = true;
    overlays = [ myHaskellPackageOverlay ];
  }
}:

let
  R-with-my-packages = nixpkgs.rWrapper.override{
    packages = with nixpkgs.rPackages; [
      ggplot2
      reshape2
      tidyverse
    ]; };

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    ad
    backprop
    base
    cassava
    hasql
    http-client
    http-client-tls
    http-conduit
    microlens
    monad-extras
    path
    path-io
    pipes
    postgresql-binary
    process
    unix-compat
    vinyl
    xlsx
    zip
    base brick containers foldl free histogram-fill ieee754 integration
    lens linear log-domain math-functions matrix monad-coroutine
    monad-extras mtl mwc-random pipes pretty-simple primitive random
    safe scientific statistics text transformers vector vty

    random-fu
    hspec

    Chart
    Chart-diagrams
    diagrams-svg
    diagrams-rasterific

    base bytestring cassava colour
    data-default-class

    # Not great :-(
    (pkgs.haskell.lib.dontCheck gamma)

    lazyio lens log-domain monad-extras
    mtl random statistics transformers vector
    uniplate

    storable-complex
    hmatrix
    one-liner-instances
    mnist-idx
  ];

in

pkgs.stdenv.mkDerivation {
  name = "rOnly";

  buildInputs = [
    pkgs.libintlOrEmpty
    R-with-my-packages
    pkgs.cabal2nix
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ];
}
