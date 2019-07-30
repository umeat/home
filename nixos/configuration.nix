{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

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

  # One day the scanner in the x1 will be supported, alas, not today
  #services.fprintd.enable = true;

  time.timeZone = "Australia/Canberra";
  services.ntp.enable = true;

  environment.systemPackages = with pkgs; [
    haskellPackages.X11
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.ghcid
    rxvt_unicode
    dmenu
    wget 
    vim
    firefox
    vlc
    zip
    git
    light
    pulseaudioFull
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
    notify-osd
    python36
    python36Packages.pylint
    go
    gotools
    yarn
    slack
  ];

  users.extraUsers.brandon = {
    password = "change-me";
    isNormalUser = true;
    home = "/home/brandon";
    extraGroups = [ "wheel" "docker" "dialout" ];
  };

  system.stateVersion = "17.09";

  services.xserver = {
    enable = true;
    videoDrivers = [ "ati" "cirrus" "vesa" "vmware" "modesetting" "displaylink" ];

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
}
