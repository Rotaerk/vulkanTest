{ refsWithLocalSource ? [] }:
(import ./vulkanTest.nix { inherit refsWithLocalSource; }).main.env
