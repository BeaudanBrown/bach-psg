{
  outputs =
    {
      self,
      nixpkgs,
      nixpkgsUnstable,
      flake-utils,
      pre-commit-hooks,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgsUnstable = nixpkgsUnstable.legacyPackages.${system};
        luna-base = with pkgs; stdenv.mkDerivation rec {
          pname = "luna-base";
          version = "1.0.0";
          enableParallelBuilding = true;

          src = fetchFromGitHub {
            owner = "remnrem";
            repo = "luna-base";
            rev = "refs/tags/v${version}";
            hash = "sha256-IWftBR1rU5yejdmngg6eFrLe/Pyfq/Qok/fq/cXyKX8=";
          };

          nativeBuildInputs = [
            autoPatchelfHook
          ];

          buildInputs = [
            fftw
          ];

          installPhase = ''
                runHook preInstall

                install -Dm755 luna "$out/bin/luna"
                install -Dm755 destrat "$out/bin/destrat"
                install -Dm755 behead "$out/bin/behead"
                install -Dm755 tocol "$out/bin/tocol"
                find . -name '*.h' -exec install -Dm644 {} $out/include/{} \;

                # These do not have .h extensions
                mkdir -p $out/include/stats
                find stats/Eigen/ -maxdepth 1 -type f -exec cp {} $out/include/stats/Eigen/ \;

                mkdir -p $out/lib
                cp *.a *.so *.o $out/lib/

                runHook postInstall
                '';

          meta = {
            description = "Library supporting large-scale objective studies of sleep";
            homepage = "https://zzz.bwh.harvard.edu/luna";
            license = lib.licenses.gpl3Plus;
            maintainers = with lib.maintainers; [ beaudan ];
            platforms = lib.platforms.linux;
            mainProgram = "luna";
          };
        };
        lunar = pkgs.rPackages.buildRPackage {
          name = "lunar";
          src = pkgs.fetchFromGitHub {
            owner = "remnrem";
            repo = "luna";
            rev = "036fec226135f2f4c5712ac10fec81cd06e8faf5";
            sha256 = "sha256-jtXi1VV6k5YyUEcXJued8vs3BWu2gvdSXPaCglytbnw=";
          };
          propagatedBuildInputs = with pkgs; [
            fftw
            luna-base
            eigen
          ] ++ (with rPackages; [
              data_table
              git2r
              plotrix
              geosphere
              shiny
              DT
              shinyFiles
              xtable
              shinydashboard
              lubridate
              wkb
              digest
              aws_s3
              tidyverse
            ]);

          nativeBuildInputs = with pkgs; [
            stdenv.cc.cc
            patch
          ];

          patchPhase = ''
                patch -p1 < ${./Makevars.patch}
          '';

          configurePhase = ''
                export FFTW=${pkgs.fftw}
                export LUNA_BASE=${luna-base}
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          env.R_LIBS_USER = "./.Rlib";
          buildInpus = [
            pkgs.bashInteractive
          ];
          packages =
            with pkgs;
            [
              R
              quarto
              luna-base
              air-formatter
            ] ++ (with pkgs.rPackages; [
              languageserver
              dotenv
              targets
              crew
              tarchetypes
              qs2
              future

              lunar
              data_table
              tidyverse
              here
              Hmisc
              dotenv
              compositions
            ]);
        };
      }
    );

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgsUnstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };
}
