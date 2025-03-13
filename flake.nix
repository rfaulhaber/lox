{
  description = "lox";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }: let
    projectName = "lox";
  in
    # https://flake.parts/module-arguments.html
    flake-parts.lib.mkFlake {inherit inputs;} (top @ {
      config,
      withSystem,
      moduleWithSystem,
      ...
    }: {
      imports = [];
      flake = {
        overlays.dartOverlay = final: prev: {
          dart2 = final.dart.override {
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
      };
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      perSystem = {
        config,
        system,
        pkgs,
        self',
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [self.overlays.dartOverlay];
        };

        packages.${projectName} = pkgs.rustPlatform.buildRustPackage {
          pname = projectName;
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
        };

        packages.default = config.packages.${projectName};

        apps.${projectName} = {
          type = "app";
          program = "${config.packages.${projectName}}/bin/rlox";
        };

        apps.default = config.apps.${projectName};

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
      };
    });
}
