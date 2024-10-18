{
  inputs = {
    east-gate.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    pw-volume.url = "github:head-gardener/nixpkgs/pw-volume-fix";
    # east-gate.url = "/home/hunter/Source/east-gate";
    east-gate.url = "github:head-gardener/east-gate";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ ];

      flake = {
        overlays.default = final: prev: {
          myxmonad = self.packages.x86_64-linux.default;
        };

        nixosModules.myxmonad = ({ pkgs, config, lib, ... }:
          let
            myxmonad-pkg = self.packages.${pkgs.system}.default;
            myxmonad = config.services.xserver.windowManager.myxmonad;
          in {
            options = {
              services.xserver.windowManager.myxmonad = {
                dmenuPackage = lib.mkPackageOption pkgs "dmenu" { };
              };
            };

            config = {
              environment.systemPackages = with pkgs; [
                myxmonad.dmenuPackage
                myxmonad-pkg
                picom
                scrot
                xcolor
                xorg.xmessage
              ];

              services.xserver = {
                displayManager = {
                  session = [{
                    manage = "window";
                    name = "xmonad";

                    start = ''
                      systemd-cat -t xmonad -- ${lib.getExe myxmonad-pkg} &
                      waitPID=$!
                    '';
                  }];
                };
              };
            };
          });

        nixosModules.powerwatch = ({ lib, pkgs, ... }: {
          systemd.user.services.powerwatch = {
            enable = true;

            description = "PowerWatch daemon";
            wants = [ "upower.service" ];
            after = [ "upower.service" ];
            wantedBy = [ "default.target" ];

            serviceConfig = {
              ExecStart = "${lib.getExe' self.packages.${pkgs.stdenv.system}.default "powerwatch"}";
            };
          };

        });
      };

      perSystem = { self', system, pkgs, lib, inputs', wrapper, config, package, ... }: {
        _module.args.package = hpack: hpack.developPackage {
          root = ./.;
          name = "myxmonad";

          modifier = drv: drv.overrideAttrs (final: prev: {
            meta.mainProgram = "xmonad";

            postPatch = ''
              substituteInPlace ./lib/MyXmonad/CPanel.hs \
                --replace '"pw-volume"' '"${lib.getExe pkgs.pw-volume}"' \
                --replace '"brightnessctl"' '"${lib.getExe pkgs.brightnessctl}"' \
            '';
          });
        };

        _module.args.wrapper = drv: drv.overrideAttrs (final: prev: {
          nativeBuildInputs = prev.nativeBuildInputs ++ [ pkgs.makeWrapper ];
          postFixup = ''
            wrapProgram $out/bin/xmonad \
              --prefix PATH : ${nixpkgs.lib.makeBinPath
                [ pkgs.pipewire ]}
          '';
        });

        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              pw-volume = inputs.pw-volume.legacyPackages.${system}.pw-volume;
              east-gate = inputs'.east-gate.packages.east-gate;
              haskellPackages = prev.haskellPackages.extend (_: super: {
                east-gate = inputs'.east-gate.packages.east-gate;
                myxmonad = wrapper (package super);
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
