{
  description = "lox";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      # NOTE: this is temporary due to the fact that the canonical tests depend on the last stable version of dart
      dartOverlay = final: prev: {
        dart2 =
          final.dart.override {
            version = "2.19.6";
            sources = {
              "2.19.6-x86_64-linux" = builtins.fetchurl {
                url = "https://storage.googleapis.com/dart-archive/channels/stable/release/2.19.6/sdk/dartsdk-linux-x64-release.zip";
                sha256 = "sha256:0kvhvwd2q8s7mnjgvhl6gr3y73agcd0y79sm844xd8ybd9gg5pqg";
              };
              "2.19.6-aarch64-darwin" = builtins.fetchurl {
                url = "https://storage.googleapis.com/dart-archive/channels/stable/release/2.19.6/sdk/dartsdk-macos-arm64-release.zip";
                sha256 = "sha256:1dpd8czllsxqly7hrcazp8g9b5zj6ibs93l5qyykijjbyjv58srw";
              };
            };
          };
      };
      pkgs = import nixpkgs {
        inherit system;
        overlays = [dartOverlay];
        config.allowUnfree = true;
      };
      projectName = "lox";
    in rec {
      packages.${projectName} = pkgs.rustPlatform.buildRustPackage {
        pname = projectName;
        version = "0.1.0";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
      };

      packages.default = self.packages.${system}.${projectName};

      apps.${projectName} =
        flake-utils.lib.mkApp {drv = packages.${projectName};};

      apps.default = self.apps.${system}.${projectName};

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cargo
          rustc
          rustfmt
          clippy
          rust-analyzer
          rustup

          lldb

          # for canonical tests
          zulu
          clang
          dart2
        ];

        nativeBuildInputs = with pkgs; [
          # needed for cargo
          # solves the "missing -liconv" issue
          libiconv
        ];
      };
    });
}
