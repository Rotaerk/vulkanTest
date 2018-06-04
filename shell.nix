{ refsWithLocalSource ? [] }:
(import ./vulkanTest.nix { inherit refsWithLocalSource; }).binaries.env
