{ refsWithLocalSource ? [] }:

let
  projectName = "vulkan-tutorial";
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

        vulkan-api = overrideCabal (relSourceOverrides.vulkan-api "vulkan-api" "1.1.3.0" super.vulkan-api) (drv: {
          librarySystemDepends = [ pkgs.vulkan-loader ];
          configureFlags = [ "-fuseNativeFFI-1-0" ];
        });

        dimensions = relSourceOverrides.easytensor "dimensions" "1.0.1.0" super.dimensions;
        easytensor = relSourceOverrides.easytensor "easytensor" "1.0.0.1" super.easytensor;

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

  textures =
    pkgs.callPackage ({ runCommand }:
      runCommand "${projectName}-textures" {
        buildInputs = [];
        src = ./textures;
      }
      ''
        mkdir -p $out
        cp $src/*.{jpg,png,bmp} $out
      ''
    ) {};

  models =
    pkgs.callPackage ({ runCommand }:
      runCommand "${projectName}-models" {
        buildInputs = [];
        src = ./models;
      }
      ''
        mkdir -p $out
        cp $src/*.obj $out
      ''
    ) {};

  fullBuild =
    pkgs.writeShellScriptBin projectName ''
      LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.vulkan-validation-layers}/lib"
      XDG_DATA_DIRS="$XDG_DATA_DIRS:${pkgs.vulkan-validation-layers}/share"
      exec ${main}/bin/vulkan-tutorial --shaderspath='${shaders}' --texturespath='${textures}' --modelspath='${models}' "$@"
    '';
in
  { inherit pkgs haskellPackages main shaders textures models fullBuild; }
