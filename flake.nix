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
        let
          nativeBuildInputs = with pkgs; [ zig ];

          tests = # sh
            ''
              src=${./src}
              zig test $src/lexer.zig
              zig test $src/parser.zig
            '';
        in
        {
          packages.default = pkgs.stdenv.mkDerivation {
            name = "mylang";
            src = ./.;

            inherit nativeBuildInputs;

            doCheck = true;
            checkPhase = tests;

            configurePhase = "export ZIG_GLOBAL_CACHE_DIR=$(mktemp -d)";
            buildPhase = ''
              zig build --prefix $out
            '';
          };

          devShells.default = pkgs.mkShell {
            packages = nativeBuildInputs;
          };

          apps.test = {
            type = "app";
            program = pkgs.writeShellScriptBin "run-tests" tests;
          };
        };
    };
}
