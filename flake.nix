{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };
  outputs =
    { nixpkgs, systems, ... }:
    let
      eachSystem =
        f: nixpkgs.lib.genAttrs (import systems) (system: f (import nixpkgs { inherit system; }));
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (emacs.pkgs.withPackages (
              epkgs: with epkgs; [
                (trivialBuild {
                  pname = "fdroid";
                  version = "0.1.1";
                  src = ./.;
                  inputs = [ embark ];
                })
              ]
            ))
            fdroidcl
          ];
        };
      });
    };
}
