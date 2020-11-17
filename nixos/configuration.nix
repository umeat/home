{ lib, pkgs, options, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  nix.useSandbox = false;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };
  services.blueman.enable = true;

  hardware.opengl.driSupport32Bit = true;

  networking.hostName = "nosleep";
  networking.wireless.enable = true;
  networking.timeServers = options.networking.timeServers.default ++ [ "10.11.16.1" ];

  virtualisation.docker.enable = true;

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "brandon" ];

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
    pavucontrol
    light
    wget 
    vim
    google-chrome
    vlc
    zip
    git
    #(eclipses.eclipseWithPlugins {
    #  eclipse = pkgs.eclipses.eclipse-java;
    #  jvmArgs = [ "-javaagent:${pkgs.lombok}/share/java/lombok.jar" ];
    #})
    #jdk8
    gcc
    gnumake
    fzf
    jq
    libnotify
    go_1_14
    gotools
    yarn
    slack
    busybox
  ];

  users.extraUsers.brandon = {
    password = "change-me";
    isNormalUser = true;
    home = "/home/brandon";
    extraGroups = [ "wheel" "docker" "dialout" ];
  };

  system.stateVersion = "17.09";

  time.timeZone = "Australia/Canberra";

  services.ntp.enable = true;

  # One day the scanner in the x1 will be supported, alas, not today
  #services.fprintd.enable = true;

  services.xserver = {
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

  fonts.fonts = with pkgs; [
    source-code-pro
    powerline-fonts
  ];

  services.tailscale.enable = true;

  # Router
  #networking.firewall.enable = false;
  #networking.nat.enable = true;
  #networking.nat.internalIPs = [ "192.168.1.0/24" ];
  #networking.nat.internalInterfaces = [ "enp0s31f6" ];
  #networking.nat.externalInterface = "wlp2s0";
  #networking.interfaces.enp0s31f6.ipAddress = "192.168.1.1";
  #networking.interfaces.enp0s31f6.prefixLength = 24;
  #services.dhcpd4 = {
  #  enable = true;
  #  interfaces = [ "enp0s31f6" ];
  #  extraConfig = ''
  #    ddns-update-style none;
  #    #option subnet-mask         255.255.255.0;
  #    one-lease-per-client true;
  #
  #    subnet 192.168.1.0 netmask 255.255.255.0 {
  #      range 192.168.1.10 192.168.1.254;
  #      authoritative;
  #      # Allows clients to request up to a week (although they won't)
  #      max-lease-time              604800;
  #      # By default a lease will expire in 24 hours.
  #      default-lease-time          86400;
  #      option subnet-mask          255.255.255.0;
  #      option broadcast-address    192.168.1.255;
  #      option routers              192.168.1.1;
  #      option domain-name-servers  8.8.8.8, 8.8.4.4;
  #    }
  #  '';
  #};
}
