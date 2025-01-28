{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  packages = with pkgs; [
    musl
    binutils
    gcc
    qbe # compiler backend
  ];
}
