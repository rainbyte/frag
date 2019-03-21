![GIF Preview](frag.gif)

Author: Mun Hon Cheong (mhch295@cse.unsw.edu.au)

Program: Frag - a FPS i put together using Haskell and with Yampa

Year: 2005, 2007-8

License: GPL


## Requirements

* A graphics card with support for multitexturing and vertex arrays
* Debian/Ubuntu Linux: `$ sudo apt-get install freeglut3-dev`
* macOS with Homebrew: Run `$ brew cask install xquartz && brew install freeglut` and then restart.


## Building

#### Stack

```
$ stack install
$ stack build
```

#### GHC

```
ghc --make -O2 -fglasgow-exts main.hs
```

#### runhaskell

```
runhaskell Setup configure --user --prefix=/home/foo
runhaskell Setup build
runhaskell Setup install
```


## Usage

```
$ frag leveleg
```

`leveleg` can be an arbitrary Quake III Arena level; a default level is provided in this package, and is installed at the project root. For example, if installed into `~/bin`, and `frag` occupies `~/bin/frag`, a successful invocation might be `frag ../share/frag-1.1/leveleg`.


## Controls

* `w` forward
* `s` back
* `a` strafe left
* `d` strafe right

* `e` jump
* `z`/`x` lock / unlock mouse
* `Mouse0` (left click) fire


---

cheers,
Mun
