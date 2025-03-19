{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    zon2nix = {
      url = "github:nix-community/zon2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      zon2nix,
      ...
    }:
    let
      forAllSystems =
        function:
        nixpkgs.lib.genAttrs (nixpkgs.lib.systems.flakeExposed) (
          system: function nixpkgs.legacyPackages.${system}
        );
    in
    {
      packages = forAllSystems (
        pkgs: with pkgs; {

          default = stdenv.mkDerivation (finalAttrs: {
            pname = "zinc";
            version = "0.0.1";
            src = ./.;

            nativeBuildInputs = [ zig_0_14.hook ];
            zigBuildFlags = [ "-Doptimize=ReleaseFast" ];

            postPatch = ''
              ln -s ${callPackage ./deps.nix { }} $ZIG_GLOBAL_CACHE_DIR/p
            '';

            meta.mainProgram = finalAttrs.pname;
          });

          llvmPackages = llvmPackages_19.override {
            src = fetchFromGitHub {
              owner = "jacobly0";
              repo = "llvm-project";
              rev = "005a99ce2569373524bd881207aa4a1e98a2b238";
              hash = "sha256-g9AVQF48HvaOzwm6Fr935+2+Ch+nvUV2afygb3iUflw=";
            };
          };

        }
      );

      devShells = forAllSystems (
        pkgs:
        with pkgs;
        let

          shellScripts = {
            run = # sh
              "zig build run --prominent-compile-errors -- $@";
            watch = # sh
              "zig build run --watch --prominent-compile-errors -- $@";
            debug = # sh
              ''
                zig build && lldb ./zig-out/bin/zinc -- $@
              '';
            tests = # sh
              ''
                show_diff() {
                  if test -d test-out; then
                    ${lib.getExe delta} test-out/expected test-out/actual
                  fi
                }

                rm -vrf test-out
                zig build test || show_diff
              '';
          };
        in
        {
          default = mkShell {
            packages =
              [
                zon2nix.packages.${system}.default
                zig
                zls
              ]
              ++ (with self.packages.${system}.llvmPackages; [
                lldb
                llvm
              ])
              ++ (lib.mapAttrsToList (key: value: writeShellScriptBin key value) shellScripts);
          };
        }
      );
    };
}
