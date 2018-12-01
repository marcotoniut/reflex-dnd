(import ./reflex-platform {}).project ({ pkgs, ... }:
let nixpkgs = import <nixpkgs> {};
in
{        
  packages = {
    reflex-dnd = ./. ; 
  };

  shells = {
    ghc = ["reflex-dnd"];
    ghc8_2 = ["reflex-dnd"];
    ghcjs = ["reflex-dnd"];
  };

  withHoogle = false;
})
