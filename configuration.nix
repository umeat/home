{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  nixpkgs.config.pulseaudio = true;

  networking.hostName = "NicksOS";
#  networking.proxy.default = "http://proxy.inno.lan:3128";

  virtualisation.docker.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  time.timeZone = "Australia/Canberra";

  environment.systemPackages = with pkgs; [
    wget 
    vim
    firefox
    zip
    git
    (python36.withPackages(ps: with ps; [ numpy toolz pip ]))
    linuxPackages.virtualboxGuestAdditions
    haskellPackages.X11
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    dmenu
    pulseaudioFull
  ];

  users.extraUsers.breadedboy = {
    password = "change-me";
    isNormalUser = true;
    home = "/home/breadedboy";
    extraGroups = [ "wheel" "docker" "networkmanager" "audio" "video" ];
  };

  system.stateVersion = "17.09";

  services.xserver = {
    enable = true;

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "breadedboy";

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
  };

  #services.xserver = {
  #  enable = true;
  #  displayManager.sddm.enable = true;
  #  desktopManager.plasma5.enable = true;
  #};
}
