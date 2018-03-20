{ pkgs ? import <nixpkgs> {} }:

rec {
  applyAttrs = fattrs: attrs: pkgs.lib.mapAttrs (name: f: f attrs."${name}") fattrs;
  compose = f: g: x: f (g x);
  id = x: x;
  composeAll = builtins.foldl' compose id;
}
