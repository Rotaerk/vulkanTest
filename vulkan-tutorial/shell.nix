{ refsWithLocalSource ? [] }:
let
  vulkan-tutorial = import ./vulkan-tutorial.nix { inherit refsWithLocalSource; };
  pkgs = vulkan-tutorial.pkgs;
  haskellPackages = vulkan-tutorial.haskellPackages;
in
  vulkan-tutorial.main.env.overrideAttrs (oldAttrs: rec {
    shellHook = ''
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.vulkan-validation-layers}/lib"
      export XDG_DATA_DIRS="$XDG_DATA_DIRS:${pkgs.vulkan-validation-layers}/share"
    '';
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.vulkan-loader pkgs.vulkan-tools pkgs.cabal-install haskellPackages.ghcid ];
  })
