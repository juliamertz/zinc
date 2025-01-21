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
        # zig 0.13
        # env = zig2nix.outputs.zig-env.${system} { };

        # zig 0.14
        env = zig2nix.outputs.zig-env.${system} {
          zig = zig2nix.outputs.packages.${system}.zig.master.bin;
        };

        system-triple = env.lib.zigTripleFromString system;

        # zig build run fails for some reason... this works ¯\_(ツ)_/¯
        scripts = {
          run = # sh
            ''
              zig build && ./zig-out/bin/mylang "$@"
            '';

          test = # sh
            ''
              files=("lexer.zig" "parser.zig")
              for file in "''${files[@]}"
              do
                echo Running tests for $file
                zig test ./src/$file
              done
            '';
        };
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

        apps.build = env.app [ ] "zig build \"$@\"";
        apps.default = env.app [ ] scripts.run;
        apps.test = env.app [ ] scripts.test;

        # nix develop
        devShells.default = env.mkShell {
          nativeBuildInputs = with env.pkgs; [
            (writeShellScriptBin "run" scripts.run)
            (writeShellScriptBin "run-tests" scripts.test)
            (writeShellScriptBin "clean" ''
              rm -vrf .zig-out
              rm -vrf ~/.cache/zig
            '')
          ];
        };
      }
    ));
}
