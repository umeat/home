{ lib, config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  system.activationScripts.linkInterpreters = lib.mkBefore ''
    mkdir -p /usr/local
    ln -f -s /run/current-system/sw/bin/ /usr/local/
  '';

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  nix.useSandbox = false;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  hardware.opengl.driSupport32Bit = true;

  networking.hostName = "nosleep";
  networking.wireless.enable = true;
  networking.firewall.allowedTCPPorts = [ 4200 ];

  virtualisation.docker.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  environment.systemPackages = with pkgs; [
    haskellPackages.X11
    haskellPackages.xmobar
    haskellPackages.ghcid
    (ghc.withPackages (hp: [hp.xmonad hp.xmonad-contrib hp.xmonad-extras]))
    rxvt_unicode
    dmenu
    pulseaudioFull
    light
    wget 
    vim
    firefox
    vlc
    zip
    git
    (eclipses.eclipseWithPlugins {
      eclipse = pkgs.eclipses.eclipse-java;
      jvmArgs = [ "-javaagent:${pkgs.lombok}/share/java/lombok.jar" ];
    })
    jdk8
    gcc
    gnumake
    fzf
    jq
    libnotify
    #notify-osd
    #python37
    #python37Packages.credstash
    go_1_14
    gotools
    yarn
    slack
    busybox
    steam
  ];

  users.extraUsers.brandon = {
    password = "change-me";
    isNormalUser = true;
    home = "/home/brandon";
    extraGroups = [ "wheel" "docker" "dialout" ];
  };

  system.stateVersion = "17.09";

  time.timeZone = "Australia/Canberra";

  services = {
    ntp.enable = true;

    # One day the scanner in the x1 will be supported, alas, not today
    #fprintd.enable = true;

    xserver = {
      enable = true;
      #videoDrivers = [ "ati" "cirrus" "vesa" "vmware" "modesetting" "displaylink" ];
      videoDrivers = [ "intel" ];

      libinput = {
        enable = true;
        tapping = true;
        naturalScrolling = false;
        middleEmulation = true;
      };

      displayManager.lightdm.greeters.mini = {
        enable = true;
        user = "brandon";
        extraConfig = ''
          [greeter]
          show-password-label = true
          [greeter-theme]
          background-image = ""
        '';
      };

      windowManager.xmonad.enable = true;
      windowManager.xmonad.enableContribAndExtras = true;
    };
  };

  fonts.fonts = with pkgs; [
    source-code-pro
    powerline-fonts
  ];
}
