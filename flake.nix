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
          version = "unstable-2026-03-27";
          enableParallelBuilding = true;

          # Track master here because the QC command is newer than v1.0.0.
          src = fetchFromGitHub {
            owner = "remnrem";
            repo = "luna-base";
            rev = "70e36b5d43679b1352036e18e080f2e7a8cc4021";
            hash = "sha256-yoAaiKx7/ffISPKdZyxjpaiQgBngkn5Ja628L+S6+Uc=";
          };

          nativeBuildInputs = [
            autoPatchelfHook
          ];

          buildInputs = [
            fftw
            lightgbm
          ];

          buildPhase = ''
                runHook preBuild

                make -j"$NIX_BUILD_CORES"
                if grep -q '^tocol:' Makefile; then
                  make -j"$NIX_BUILD_CORES" tocol
                fi

                runHook postBuild
                '';

          installPhase = ''
                runHook preInstall

                for bin in luna destrat behead tocol; do
                  if [ -x "$bin" ]; then
                    install -Dm755 "$bin" "$out/bin/$bin"
                  fi
                done
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
            rev = "dcc5fc2aa1b0abedbe2ccc900fc9a6fb1ba8bafe";
            sha256 = "sha256-3zdt/w84unDz57LKAwRq2oKqz4vLn15ezjvfQzP32UA=";
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
                export LGBM_PATH=${pkgs.lightgbm}
                export LGBM=1
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          env.R_LIBS_USER = "./.Rlib";
          buildInputs = [
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
              visNetwork
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
