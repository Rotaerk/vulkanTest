{ refsWithLocalSource ? [] }:

let
  projectName = "vulkanTest";
  enableProfiling = true;

  refs = import ./refs.nix;
  pkgs = (refs { inherit refsWithLocalSource; }).sourceImports.nixpkgs-fork {};
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

  main = 
    overrideCabal
      (haskellPackages.callCabal2nix projectName ./main {})
      (drv: {
        pname = "${projectName}-main";
        src = pkgs.lib.cleanSource drv.src;
        librarySystemDepends = [ pkgs.vulkan-loader pkgs.cabal-install ];
      });

  shaders =
    pkgs.callPackage ({ runCommand, glslang }:
      runCommand "${projectName}-shaders" {
        buildInputs = [ glslang ];
        src = ./shaders;
      }
      ''
        mkdir -p $out
        for shaderfile in $src/*.{vert,frag} ; do
          glslangValidator -V $shaderfile -o $out/''${shaderfile##*/}.spv
        done
      ''
    ) {};

  fullBuild =
    pkgs.writeShellScriptBin projectName ''
      exec ${main}/bin/vulkanTest --shaderspath='${shaders}' "$@"
    '';
in
  { inherit pkgs haskellPackages main shaders fullBuild; }
