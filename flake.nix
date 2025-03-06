{
  inputs = {
    zig2nix.url = "github:Cloudef/zig2nix";
    zls.url = "github:zigtools/zls";
  };

  outputs =
    { zig2nix, zls, ... }:
    (zig2nix.inputs.flake-utils.lib.eachDefaultSystem (
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
            zls.packages.${system}.default

            (writeShellScriptBin "run" ''
              zig build run --prominent-compile-errors -- "$@"
            '')

            (writeShellScriptBin "run-out" ''
              ./zig-out/bin/zinc "$@"
            '')

            reflex
            (writeShellScriptBin "watch" ''
              run $@
              # zig build watch flag is currently broken
              reflex -d none --regex='^(?:.+\.zig|spec\/.*)$' run $@
            '')
            (writeShellScriptBin "watch-out" ''
              run-out $@
              # zig build watch flag is currently broken
              reflex -d none --regex='spec/*' run-out $@
            '')

            # compiler experimentation
            (writeShellApplication {
              name = "run-ssa";
              runtimeInputs = [ musl.dev binutils gcc qbe ];
              text = ''
                name="''${1%.*}"
                qbe "$1" | as -o temp.o && \
                musl-gcc -o "$name" -static temp.o && rm temp.o && \
                exec "./$name" && rm "$name"
              '';
            })
          ];
        };
      }
    ));
}
