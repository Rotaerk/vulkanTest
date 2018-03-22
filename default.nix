{ refsWithLocalSource ? [] }:

let
  project = "vulkanTest";
  inherit (import ./refs.nix { inherit refsWithLocalSource; }) sources sourceImports;
  pkgs = sourceImports.nixpkgs {};
  haskellLib = pkgs.haskell.lib;
  haskellPackages =
    pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
          enableExecutableProfiling = true;
        });

        vulkan-api = haskellLib.overrideCabal super.vulkan-api (drv: {
          librarySystemDepends = [ pkgs.vulkan-loader ];
        });
      };
    };
in
  haskellLib.overrideCabal
    (haskellPackages.callCabal2nix "${project}" ./. {})
    (drv: {
      src = builtins.filterSource (path: type: baseNameOf path != ".git") drv.src;
    })
