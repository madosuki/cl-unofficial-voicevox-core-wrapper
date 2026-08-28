{
  description = "Development shell for cl-unofficial-voicevox-core-wrapper";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              gcc
              pkg-config
              libffi.dev
              stdenv.cc.cc.lib
            ];

            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              pkgs.libffi
              pkgs.stdenv.cc.cc.lib
            ];
          };
        });
    };
}
