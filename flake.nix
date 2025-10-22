{
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      pre-commit-hooks,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              air-fmt = {
                enable = true;
                entry = "air format";
                files = ".*\.[rR]$";
              };
            };
          };
        };
        devShells.default = pkgs.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          env.R_LIBS_USER = "./.Rlib";
          buildInputs = [
            pkgs.bashInteractive
            self.checks.${system}.pre-commit-check.enabledPackages
          ];
          packages =
            with pkgs;
            [
              R
              quarto
              air-formatter
              nodejs_24
            ]
            ++ (with rPackages; [
              quarto
              languageserver
              dotenv
              httpgd
              targets
              tarchetypes
              crew
              data_table
              future
              forcats
              lubridate
              lme4
              qs2
              mice
              ranger
              rms
              broom
              mitools
              gtsummary
              knitr
              renv
            ]);
        };
      }
    );

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    systems.url = "github:nix-systems/default";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };
}
