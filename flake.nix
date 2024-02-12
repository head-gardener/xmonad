{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ ];

      flake = {
        overlays.default = final: prev: {
          myxmonad = self.packages.x86_64-linux.default;
        };
      };

      perSystem = { self', system, pkgs, config, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              haskellPackages = prev.haskellPackages.extend (_: super: {
                myxmonad = super.developPackage {
                  root = ./.;
                  name = "myxmonad";
                };
              });
            })
          ];
        };

        formatter = pkgs.nixpkgs-fmt;

        checks = {
          build = self'.packages.default;
        };

        devShells.default = with pkgs.haskellPackages; shellFor {
          nativeBuildInputs = [
            cabal-install
            hpack
            haskell-language-server
          ];

          packages = p: [ p.myxmonad ];

          withHoogle = true;
        };

        packages.default = pkgs.haskellPackages.myxmonad;
      };
    };
}
