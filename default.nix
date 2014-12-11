{ cabal, hdaemonize, aeson, split, process, downloadCurl
}:
cabal.mkDerivation (self: {
  pname = "muzei-for-desktop";
  src = ./.;
  version = "1.0";
  buildDepends = [
    hdaemonize aeson split process downloadCurl
  ];
})
