Muzei for Desktop
=============
Have your background set to a new beautiful art piece every day.

![adolphe monet reading in the garden claude monet 1866](https://github.com/davidrusu/muzei/raw/master/adolphe-monet-reading-in-the-garden-claude-monet-1866-200px.jpg)
![holyday james tissot c 1876](https://github.com/davidrusu/muzei/raw/master/holyday-james-tissot-c-1876-200px.jpg)
![tick tack briton riviere 1881](https://github.com/davidrusu/muzei/raw/master/tick-tack-briton-riviere-1881-200px.jpg)
*Muzei for Desktop* uses the same image source as the Android app [Muzei](https://play.google.com/store/apps/details?id=net.nurik.roman.muzei&hl=en).
Get the app for matching wallpaper fun!.


Install
-------
You'll need to install feh, It's used to set the wallpaper.

Building
--------

You'll need these packages:
  - [hdaemonize](http://hackage.haskell.org/package/hdaemonize-0.4) -- Simple daemonizing library
  - [aeson](http://hackage.haskell.org/package/aeson-0.6.1.0) -- Awesome JSON parser
  - [download-curl](http://hackage.haskell.org/package/download-curl) -- Used to fetch data and images
  - [split](http://hackage.haskell.org/package/split) -- package for splitting lists
  - [directory](http://hackage.haskell.org/package/directory)
  - [process](http://hackage.haskell.org/package/process)

Then just build with ghc

    ghc muzei

put the binary somewhere and add it to your init file
