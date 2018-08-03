{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in rec {
    eclipse-ee-48 = super.eclipses.buildEclipse {
      name = "eclipse-ee-4.8";
      description = "Eclipse EE IDE";
      sources = {
        "x86_64-linux" = super.fetchurl {
          url = https://www.eclipse.org/downloads/download.php?r=1&nf=1&file=/technology/epp/downloads/release/photon/R/eclipse-jee-photon-R-linux-gtk-x86_64.tar.gz;
          sha256 = "1vnkzdn0vpaxn4mid0im8vnymyyia2cbmk8hvmii4palzr3h3l1v";
          name = "eclipse-jee-photon-R-linux-gtk-x86_64.tar.gz";
        };
      };
    };
  };
}
