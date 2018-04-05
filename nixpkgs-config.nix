{
  packageOverrides = super: let self = super.pkgs; in rec {
    eclipse-ee-47 = super.eclipses.buildEclipse {
      name = "eclipse-ee-4.7.2";
      description = "Eclipse EE IDE";
      sources = {
        "x86_64-linux" = super.fetchurl {
          url = http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/oxygen/2/eclipse-jee-oxygen-2-linux-gtk-x86_64.tar.gz&r=1;
          sha256 = "0qpbkbrnz8amhiz92xq179cfqnhhrp2dh2w456gbhgbf3v02npz3";
          name = "eclipse-jee-oxygen-2-linux-gtk-x86_64.tar.gz";
        };
      };
    };
  };
}
