(import <nixpkgs> {}).fetchgit
  (builtins.removeAttrs (builtins.fromJSON (builtins.readFile ./git.json)) [ "date" ])
