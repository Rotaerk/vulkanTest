{ refsWithLocalSource ? [] }:
(import ./vulkanTest.nix { inherit refsWithLocalSource; }).fullBuild
