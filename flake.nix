{
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          packages =
            with pkgs;
            [
              radian
              R
              quarto
              air-formatter
              nodejs_24
            ]
            ++ (with rPackages; [
              quarto
              languageserver
              dotenv
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
              ggplot2
              knitr
              renv
              coxphf
              tidycmprsk
              patchwork
              stringr
              table1
              openxlsx
              scales
              gridExtra
              tidyverse
              corrplot
              svglite
            ]);
        };
      }
    );

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };
}
