{
  pkgs ? import <nixpkgs> {},
  baseDir ? ./.,
  refsDir ? baseDir + /refs,
  refsWithLocalSource ? []
}:

let
  inherit (import ./lib.nix { inherit pkgs; }) compose composeAll;
  inherit (pkgs.lib) filterAttrs mapAttrs mapAttrsToList;
  inherit (pkgs.haskell.lib) overrideCabal;
  refs =
    composeAll [
      (mapAttrs (fileName: fileType: import (refsDir + "/${fileName}")))
      (filterAttrs (fileName: fileType: fileType == "regular"))
      builtins.readDir
    ]
      refsDir;

in rec {
  sources =
    compose
      (filterAttrs (refName: source: source != null))
      (mapAttrs
        (refName: ref:
          if builtins.elem refName refsWithLocalSource then (
            if ref.scheme == "github" then
              refsDir + "/${refName}.git"
            else
              null
          )
          else (
            if ref.scheme == "github" then
              pkgs.fetchFromGitHub { inherit (ref) owner repo rev sha256; }
            else
              null
          )
        )
      )
      refs;

  relSourceOverrides =
    mapAttrs
      (refName: srcPath:
        (subDir: version: pkg:
          overrideCabal pkg
            (drv:
            {
              src = srcPath + "/${subDir}";
              inherit version;
              sha256 = null;
              revision = null;
              editedCabalFile = null;
            }
            )
        )
      )
      sources;

  sourceOverrides = mapAttrs (refName: subdirOverride: subdirOverride "") relSourceOverrides;

  relSourceImports =
    mapAttrs
      (refName: srcPath:
        subDir: import (srcPath + "/${subDir}")
      )
      sources;

  sourceImports = mapAttrs (refName: importSubdir: importSubdir "") relSourceImports;
}
