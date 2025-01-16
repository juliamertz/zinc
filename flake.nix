{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, ... }@inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem =
        { pkgs, ... }:
        {
          packages.default = pkgs.stdenv.mkDerivation {
            name = "mylang";
            src = ./.;

            nativeBuildInputs = with pkgs; [ zig ];

            configurePhase = ''
              export ZIG_GLOBAL_CACHE_DIR=$(mktemp -d)
            '';

            buildPhase = ''
              zig build --prefix $out
            '';
          };

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [ zig ];
          };
        };
    };
}
