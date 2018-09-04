{ refsWithLocalSource ? [] }:
(import ./vulkan-tutorial.nix { inherit refsWithLocalSource; }).fullBuild
