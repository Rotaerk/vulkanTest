{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  inherit (lib) makeLibraryPath;
  hs = haskell.packages.ghc884;
  tools = [
    (hs.ghcWithPackages (ps: [ps.shake]))
    hs.cabal-install
    hs.ghcid
    hs.hpack
    vulkan-tools
    glslang
    binutils-unwrapped
    gcc9
  ];
  libraries = [
#    vulkan-headers
    vulkan-loader
    vulkan-validation-layers
    libGL
    xorg.libX11
    xorg.libXcursor
    xorg.libXext
    xorg.libXfixes
    xorg.libXi
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXxf86vm
  ];
  libraryPath = "${makeLibraryPath libraries}";
in
  pkgs.runCommand "shell" {
    buildInputs = tools ++ libraries;
    shellHook = ''
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
      export LIBRARY_PATH="${libraryPath}"
    '';
  } ""
