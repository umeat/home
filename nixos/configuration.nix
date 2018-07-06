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
  #networking.proxy.default = "http://proxy.inno.lan:3128";

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
    (python36.withPackages(ps: with ps; [ numpy toolz pip requests virtualenvwrapper ]))
    haskellPackages.X11
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    xorg.xbacklight
    dmenu
    pulseaudioFull
    eclipse-ee-47
    jdk8
    gcc
    gnumake
    steam
    discord
    fzf
    jq
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

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "brandon";

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
  };

  #services.xserver = {
  #  enable = true;
  #  displayManager.sddm.enable = true;
  #  desktopManager.plasma5.enable = true;
  #};
}
