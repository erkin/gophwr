{ racket2nix ? import ./racket2nix {}
}:

with racket2nix;

buildRacketPackage (builtins.path { path = ../src; name = "gophwr"; })
