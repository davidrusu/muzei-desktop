let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      muzeiForDesktop = self.callPackage ./. {};
    };
  
  };
  
  systemPackages = with pkgs; [
     stdenv
     git
     feh
  ];
  
  in pkgs.lib.overrideDerivation haskellPackages.muzeiForDesktop (attrs: {

    buildInputs = [ haskellPackages.cabalInstall
                  ] ++ attrs.buildInputs ++ systemPackages;
})
