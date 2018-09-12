{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  hardware.opengl.driSupport32Bit = true;

  networking.hostName = "nosleep";
  networking.wireless.enable = true;
  networking.firewall.allowedTCPPorts = [ ];

  virtualisation.docker.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  time.timeZone = "Australia/Canberra";

  nixpkgs.config = import ./nixpkgs-config.nix;

  environment.systemPackages = with pkgs; [
    wget 
    vim
    firefox
    vlc
    zip
    git
    (python36.withPackages(ps: with ps; [ 
      numpy toolz pip requests virtualenvwrapper
    ]))
    haskellPackages.X11
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.ghcid
    xorg.xbacklight
    dmenu
    pulseaudioFull
    eclipse-ee-48
    jdk8
    gcc
    gnumake
    steam
    slack
    discord
    fzf
    jq
    libnotify
    notify-osd
    lftp
    rxvt_unicode
  ];

  users.extraUsers.brandon = {
    password = "change-me";
    isNormalUser = true;
    home = "/home/brandon";
    extraGroups = [ "wheel" "docker" ];
  };

  system.stateVersion = "17.09";

  services.xserver = {
    enable = true;

    synaptics = {
      enable = true;
      tapButtons = true;
      fingersMap = [1 3 2];
      horizTwoFingerScroll = true;
      vertTwoFingerScroll = true;
      scrollDelta = 107;
      accelFactor = "0.1";
      twoFingerScroll = true;

      # palm detection
      # https://askubuntu.com/questions/229311/synaptics-touchpad-solving-2-finger-problem-triggered-by-resting-palm/772103#772103
      palmDetect = true;
      palmMinWidth = 10;
      palmMinZ = 0;
      additionalOptions = ''
        # https://askubuntu.com/a/772103
        Option "AreaLeftEdge" "2000"
        Option "AreaRightEdge" "5500"
      '';
    };

    displayManager.slim = {
      enable = true;
      defaultUser = "brandon";
      theme = pkgs.fetchurl {
        url = "https://github.com/naglis/slim-minimal/archive/65759e026e8de1f957889e81ca6faf3b8c2167a7.tar.gz";
        sha256 = "c6a4b674f281ee5a2b8227959a575f37db5a1c6cd332edf6326a730b4d10eac2";
      };
    };

    desktopManager.xterm.enable = false; # https://github.com/NixOS/nixpkgs/issues/21958

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
  };

  system.autoUpgrade = {
    enable = true;
    dates = "9:30";
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
    ];
  };

  nesting.clone = [{
    networking.proxy.default = "http://proxy.inno.lan:3128";
    networking.proxy.noProxy = "127.0.0.1,localhost";
  }];
}
