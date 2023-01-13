let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

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
      jqr
      RPostgres
      readxl
      openxlsx
    ]; };

  pkgs = nixpkgs;

  # monad-bayes = pkgs.haskell.packages.ghc902.developPackage {
  #   name = "monad-bayes";
  #   root = "/Users/dom/Dropbox/Tidy/monad-bayes-maria";

  #   # Remove this override when bumping nixpkgs
  #   source-overrides = {
  #     vty = pkgs.fetchzip {
  #       url = "mirror://hackage/vty-5.37/vty-5.37.tar.gz";
  #       sha256 = "sha256-OOrJBi/mSIyaibgObrp6NmUTWxRu9pxmjAL0EuPV9wY=";
  #     };

  #     text-zipper = pkgs.fetchzip {
  #       url = "mirror://hackage/text-zipper-0.12/text-zipper-0.12.tar.gz";
  #       sha256 = "sha256-P2/UHuG3UuSN7G31DyYvyUWSyIj2YXAOmjGkHtTaP8o=";
  #     };

  #     bimap = pkgs.fetchzip {
  #       url = "mirror://hackage/bimap-0.5.0/bimap-0.5.0.tar.gz";
  #       sha256 = "sha256-pbw+xg9Qz/c7YoXAJg8SR11RJGmgMw5hhnzKv+bGK9w=";
  #     };

  #     brick = pkgs.fetchzip {
  #       url = "mirror://hackage/brick-1.4/brick-1.4.tar.gz";
  #       sha256 = "sha256-KDa7RVQQPpinkJ0aKsYP0E50pn2auEIP38l6Uk7GmmE=";
  #     };
  #   };

  #   cabal2nixOptions = "--benchmark";
  # };

  haskellDeps = ps: with ps; [
    ad
    base
    cassava
    hasql
    http-client
    http-client-tls
    http-conduit
    microlens
    # monad-bayes
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

    # apecs apecs-gloss apecs-physics base bytestring cassava Chart
    # Chart-diagrams colour data-default-class debug gamma gloss lazyio
    # lens log-domain monad-extras mtl random statistics transformers
    # vector

    base bytestring cassava colour
    data-default-class
    # debug
    (pkgs.haskell.lib.dontCheck gamma)
    lazyio lens log-domain monad-extras
    mtl random statistics transformers vector
    uniplate
  ];

in

pkgs.stdenv.mkDerivation {
  name = "rOnly";

  buildInputs = [
    pkgs.libintlOrEmpty
    R-with-my-packages
    pkgs.postgresql_13
    pkgs.lhs2tex
    pkgs.cabal2nix
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ];
}
