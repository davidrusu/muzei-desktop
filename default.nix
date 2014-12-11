# { haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
# let
#   inherit (haskellPackages) cabal cabalInstall
#     hdaemonize aeson split process downloadCurl;
# 
# in cabal.mkDerivation (self: {
#   pname = "muzei-for-desktop";
#   version = "1.0.0";
#   src = ./.;
#   buildDepends = [
#     hdaemonize aeson split process downloadCurl
#   ];
#   buildTools = [ cabalInstall ];
#   enableSplitObjs = false;
# })

{ cabal
, hdaemonize, aeson, split, process, downloadCurl
}:
cabal.mkDerivation (self: {
  pname = "muzei-for-desktop";
  src = ./.;
  version = "1.0";
  buildDepends = [
    hdaemonize aeson split process downloadCurl
  ];
})
