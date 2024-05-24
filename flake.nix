{
  description = "xy_calc Hix/Pix/Plutus dApp DevEnv";

  inputs = {
    
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskellNix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskellNix = {
      url = "github:input-output-hk/haskell.nix/1c329acdaac3d5a600bcaa86b1806414ccd48db6";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage";
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?rev=35d5d7f7e7cfed87901623262ceea848239fa7f8";
      flake = false;
    };

    plutus.url = "github:IntersectMBO/plutus";

    styleguide.url = "github:cardanonix/styleguide";
  };

outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP, plutus, styleguide, ... }:
  let
  
    overlays = [
      haskellNix.overlay
      iohkNix.overlays.crypto
      (final: prev: {
        dork-manager = final.haskell-nix.project' {
          src = ./src;
          compiler-nix-name = "ghc928";
          shell.tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };
      })
    ];
    back_EndResults = flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (
      system: let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        inherit styleguide;
        hixProject = pkgs.haskell-nix.hix.project {
          src = ./.;
          evalSystem = system;
          inputMap = {"https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;};
          modules = [
            (_: {
              packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf];
              packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf pkgs.secp256k1];
            })
          ];
        };
        hixFlake = hixProject.flake {};
      in {
        apps = hixFlake.apps;
        checks = hixFlake.checks;
        # checks.format = styleguide.lib.${system}.mkCheck self; # these are for CI but they depend on ‘terraform-1.6.0’
        # formatter = styleguide.lib.${system}.mkFormatter self; # these are for CI but they depend on ‘terraform-1.6.0’
        packages = hixFlake.packages;

        legacyPackages = pkgs;

        devShell = pkgs.mkShell {
          name = "xy_calc";
          inputsFrom = [hixFlake.devShell];
          buildInputs = [
            (pkgs.haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
              # Assuming dork-manager is a package within your project
              # generators
              # haskell-language-server
              # hoogle
              # fourmolu
            ]))
            pkgs.zlib
          ];
          packages = with pkgs; [
            haskellPackages.fourmolu
            zlib
            nix-tree
            # hackage-mirror
            cabal-install
          ];
          shellHook = ''
            # Your existing shellHook here...
          '';
        };
      }
    );
  in
    back_EndResults
    // {
      # apps = back_EndResults.apps // oci_ImageResult.apps;
      # checks = back_EndResults.checks // oci_ImageResult.checks;
      packages = back_EndResults.packages;
      # legacyPackages = back_EndResults.legacyPackages;
      devShell = back_EndResults.devShell;
    };
  nixConfig = {
    extra-experimental-features = ["nix-command flakes" "ca-derivations"];
    allow-import-from-derivation = "true";
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://klarkc.cachix.org?priority=99"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
  };
}
