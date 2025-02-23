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

        apps.build = env.app [ ] ''zig build "$@"'';
        apps.default = env.app [ ] ''zig build run -- "$@"'';
        apps.test = env.app [ ] ''zig build test'';

        # nix develop
        devShells.default = env.mkShell {
          nativeBuildInputs = with env.pkgs; [
            (writeShellScriptBin "run" ''
              zig build run --prominent-compile-errors -- "$@"
            '')

            reflex
            (writeShellScriptBin "watch" ''
              run $@
              # zig build watch flag is currently broken
              reflex -d none --regex='^(?:.+\.zig|spec\/.*)$' run $@
            '')

            # compiler experimentation
            musl
            binutils
            gcc
            qbe
            (writeShellScriptBin "run-ssa" ''
              name="''${1%.*}"
              qbe $1 | as -o temp.o && \
              musl-gcc -o $name -static temp.o && rm temp.o && \
              ./$name && rm $name
            '')
          ];
        };
      }
    ));
}
