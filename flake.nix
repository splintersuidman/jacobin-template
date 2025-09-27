{
  description = "Flake for jacobin-template";

  inputs = {
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mkSpagoDerivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-github-actions = {
      url = "github:nix-community/nix-github-actions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = { };
        overlays = builtins.attrValues self.overlays;
      });
    in {
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
        mkSpagoDerivation = inputs.mkSpagoDerivation.overlays.default;
      };

      githubActions = inputs.nix-github-actions.lib.mkGithubMatrix {
        checks = nixpkgs.lib.getAttrs [ "x86_64-linux" ] self.packages;
      };

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in {
          jacobin-template = pkgs.mkSpagoDerivation {
            src = ./.;
            nativeBuildInputs = [ pkgs.purs pkgs.purs-backend-es pkgs.spago-unstable pkgs.esbuild ];
            version = "0.1.0";
            buildPhase = "make all";
            installPhase = "make install PREFIX=$out";
          };
        });

      devShell = forAllSystems (system: 
        let
          pkgs = nixpkgsFor.${system};
        in pkgs.mkShell {
          buildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy-bin.purs-tidy-0_11_0
            pkgs.purs-backend-es
            pkgs.purescript-language-server
            pkgs.nodejs
            pkgs.esbuild
          ];
        });
    };
}
