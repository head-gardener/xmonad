inputs: inputs.nixpkgs.lib.nixos.runTest {
  name = "xmonad-boot-test";

  nodes.machine = { config, lib, pkgs, ... }: {
    nixpkgs.overlays = [ inputs.self.overlays.default ];

    services.xserver = {
      displayManager.startx.enable = true;
      enable = true;
    };

    environment.etc.xinitrc.text = "
      xmobar &
      exec xmonad
    ";

    systemd.services.xmonad = {
      enable = true;
      description = "xmonad";
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [
        xorg.xinit
        xorg.xorgserver
        myxmonad
        xmobar
        hexdump
      ];
      environment = {
        XINITRC = "/etc/xinitrc";
      };
      serviceConfig = {
        ExecStart = "${pkgs.xorg.xinit}/bin/xinit";
      };
    };

    environment.systemPackages = with pkgs; [
      xmobar
      myxmonad
    ];
  };

  hostPkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;

  testScript = ''
    start_all()
    machine.wait_for_unit("xmonad")
  '';
}
