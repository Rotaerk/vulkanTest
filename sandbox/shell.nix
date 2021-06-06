{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  inherit (lib) makeLibraryPath;
  hs = haskell.packages.ghc884;
  packages = [
    (hs.ghcWithPackages (ps: [ps.shake]))
    hs.cabal-install
    hs.ghcid
    hs.hpack
    vulkan-tools
    glslang
    binutils-unwrapped
    gcc9
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
in
  mkShell {
    inherit packages;
  }
