{ racket2nix ? import ./racket2nix {}
}:

with racket2nix;

buildThinRacketPackage (builtins.path { path = ../src; name = "gophwr"; })
