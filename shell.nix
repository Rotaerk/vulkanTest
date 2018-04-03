{ refsWithLocalSource ? [] }:
(import ./default.nix { inherit refsWithLocalSource; }).env
