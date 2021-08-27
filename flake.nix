{
  description = "OCaml P2P libraries";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    blipPkgs.url = "github:p2pcollab/ocaml-blip";
    urpsPkgs.url = "github:p2pcollab/ocaml-urps";
  };

  outputs = { self, nixpkgs, blipPkgs, urpsPkgs }:
    let
      supportedSystems = [
        "x86_64-linux" "aarch64-linux" "armv7l-linux"
        "x86_64-darwin" "aarch64-darwin"
      ];
      supportedOcamlPackages = [
        "ocamlPackages_4_10"
        "ocamlPackages_4_11"
        "ocamlPackages_4_12"
      ];
      defaultOcamlPackages = "ocamlPackages_4_12";

      forAllOcamlPackages = nixpkgs.lib.genAttrs (supportedOcamlPackages ++ [ "ocamlPackages" ]);
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor =
        forAllSystems (system:
          import nixpkgs {
            inherit system;
            overlays = [ self.overlay ];
          });
    in
      {
        overlay = final: prev:
          with final;
          let mkOcamlPackages = prevOcamlPackages:
                with prevOcamlPackages;
                let ocamlPackages = rec {
                      inherit ocaml;
                      inherit findlib;
                      inherit ocamlbuild;
                      inherit opam-file-format;
                      inherit buildDunePackage;

                      p2p =
                        buildDunePackage rec {
                          pname = "p2p";
                          version = "0.0.1";
                          src = self;

                          useDune2 = true;
                          doCheck = true;

                          nativeBuildInputs = with ocamlPackages; [
                            odoc
                            ounit
                            utop
                          ];
                          buildInputs = with ocamlPackages; [
                            bloomf
                            bitv
                            blip
                            urps
                            fmt
                            lru
                            lwt
                            lwt_ppx
                            nocrypto
                            stdint
                          ];
                        };
                    };
                in ocamlPackages;
          in
            let allOcamlPackages =
                  forAllOcamlPackages (ocamlPackages:
                    mkOcamlPackages (ocaml-ng.${ocamlPackages}
                                     // blipPkgs.packages.${system}.${ocamlPackages}
                                     // urpsPkgs.packages.${system}.${ocamlPackages}));
            in
              allOcamlPackages // {
                ocamlPackages = allOcamlPackages.${defaultOcamlPackages};
              };

        packages =
          forAllSystems (system:
            forAllOcamlPackages (ocamlPackages:
              nixpkgsFor.${system}.${ocamlPackages}));

        defaultPackage =
          forAllSystems (system:
            nixpkgsFor.${system}.ocamlPackages.p2p);
      };
}
