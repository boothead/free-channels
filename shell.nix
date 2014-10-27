with import <nixpkgs> {};

let haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        freeChannels = self.callPackage ./. {};
      };
    };
in lib.overrideDerivation haskellPackages.freeChannels (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })
