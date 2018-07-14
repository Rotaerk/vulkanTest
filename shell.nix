{ refsWithLocalSource ? [] }:
let
  vulkanTest = import ./vulkanTest.nix { inherit refsWithLocalSource; };
  pkgs = vulkanTest.pkgs;
in
  vulkanTest.main.env.overrideAttrs (oldAttrs: rec {
    shellHook = ''
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.vulkan-validation-layers}/lib"
      export XDG_DATA_DIRS="$XDG_DATA_DIRS:${pkgs.vulkan-validation-layers}/share"
    '';
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.vulkan-loader pkgs.vulkan-tools pkgs.cabal-install ];
  })
