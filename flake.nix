{
  inputs = {
    zig2nix.url = "github:Cloudef/zig2nix";
  };

  outputs =
    { zig2nix, ... }:
    let
      flake-utils = zig2nix.inputs.flake-utils;
    in
    (flake-utils.lib.eachDefaultSystem (
      system:
      let
        zig-master = zig2nix.outputs.packages.${system}.zig.master.bin;
        env = zig2nix.outputs.zig-env.${system} { zig = zig-master; };
        system-triple = env.lib.zigTripleFromString system;
      in
      with builtins;
      with env.lib;
      with env.pkgs.lib;
      rec {
        packages.target = genAttrs allTargetTriples (
          target:
          env.packageForTarget target ({
            src = cleanSource ./.;
            nativeBuildInputs = with env.pkgs; [ ];
            buildInputs = with env.pkgsForTarget target; [ ];
            zigPreferMusl = true;
            zigDisableWrap = true;
          })
        );

        packages.default = packages.target.${system-triple}.override {
          zigPreferMusl = false;
          zigDisableWrap = false;
        };

        apps.build = env.app [ ] ''zig build --prominent-compile-errors "$@"'';
        apps.default = env.app [ ] ''zig build run --prominent-compile-errors -- "$@"'';
        apps.test = env.app [ ] ''zig build test'';

        # nix develop
        devShells.default = env.mkShell {
          nativeBuildInputs = with env.pkgs; [
            (writeShellScriptBin "run" ''zig build run -- "$@"'')
          ];
        };
      }
    ));
}
