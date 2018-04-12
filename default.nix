{ refsWithLocalSource ? [] }:

let
  project = "vulkanTest";
  enableProfiling = true;

  refs = import ./refs.nix;
  pkgs = (refs { inherit refsWithLocalSource; }).sourceImports.nixpkgs {};
  inherit (refs { inherit refsWithLocalSource pkgs; }) sources sourceImports sourceOverrides relSourceOverrides;
  inherit (pkgs.haskell.lib) overrideCabal;

  haskellPackages =
    pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = enableProfiling;
          enableExecutableProfiling = enableProfiling;
        });

        HUnit =
          # One of HUnit's tests fails when profiling is enabled.  This is a workaround.
          if enableProfiling then
            super.HUnit.overrideAttrs (oldAttrs: { doCheck = false; })
          else
            super.HUnit;

        vulkan-api = overrideCabal (relSourceOverrides.vulkan-api "vulkan-api" "1.1.0.0" super.vulkan-api) (drv: {
          librarySystemDepends = [ pkgs.vulkan-loader ];
          configureFlags = [ "-fuseNativeFFI-1-0" ];
        });

        bindings-GLFW = sourceOverrides.bindings-GLFW "3.2.1.0" super.bindings-GLFW;

        GLFW-b = sourceOverrides.GLFW-b "1.4.9.0" super.GLFW-b;
      };
    };
in
  overrideCabal
    (haskellPackages.callCabal2nix "${project}" ./. {})
    (drv: {
      src = builtins.filterSource (path: type: baseNameOf path != ".git") drv.src;
      librarySystemDepends = [ pkgs.vulkan-loader pkgs.cabal-install ];
    })
