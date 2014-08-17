Muzei Desktop
=============

New beautiful art wallpaper for unix everyday

Muzei uses the same image source as the Muzei android app (Which you should get if your into this sort of thing).


Install
-------
You'll need to install feh, It's used to set the wallpaper.

To automatically get the new wallpaper, set a cron job:

    0 * * * * /path/to/muzei

Building
--------

You'll need these packages:
  - [aeson](http://hackage.haskell.org/package/aeson-0.6.1.0) -- Awesome JSON parser
  - [download-curl](http://hackage.haskell.org/package/download-curl) -- Used to fetch data and images
  - [split](http://hackage.haskell.org/package/split) -- package for splitting lists
  - [directory](http://hackage.haskell.org/package/directory)
  - [process](http://hackage.haskell.org/package/process)

Then just build with ghc

    ghc muzei

