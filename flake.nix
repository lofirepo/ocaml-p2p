# SPDX-FileCopyrightText: 2019 TG x Thoth
# SPDX-License-Identifier: AGPL-3.0-only

{
  description = "OCaml P2P libraries";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    blip.url = "github:p2pcollab/ocaml-blip/master";
    urps.url = "github:p2pcollab/ocaml-urps/master";
  };

  outputs = { self, nixpkgs, blip, urps }: {
    packages.x86_64-linux.ocaml-bloomf =
      with import nixpkgs { system = "x86_64-linux"; };
      ocamlPackages.buildDunePackage rec {
        pname = "bloomf";
        version = "0.1.0-bits";
        src = fetchFromGitHub {
          owner = "p2pcollab";
          repo = pname;
          rev = "cbffe83255cb12f5117825f8f8ebf363e18bd627";
          sha256 = "0p4l8fib72vmbyk5izlsfawyxfz3wgcg2c2vglqf5103y28xi1jg";
        };
        useDune2 = true;

        buildInputs = with pkgs.ocamlPackages; [
          blip.packages.x86_64-linux.ocaml-bitv
        ];
      };

    packages.x86_64-linux.ocaml-p2p =
      with import nixpkgs { system = "x86_64-linux"; };
      ocamlPackages.buildDunePackage rec {
        pname = "p2p";
        version = "0.0.1";
        src = self;
        useDune2 = true;

        buildInputs = with pkgs.ocamlPackages; [
          self.packages.x86_64-linux.ocaml-bloomf
          blip.packages.x86_64-linux.ocaml-bitv
          blip.packages.x86_64-linux.ocaml-blip
          urps.packages.x86_64-linux.ocaml-urps
          fmt
          lru
          lwt
          lwt_ppx
          nocrypto
          stdint
        ];
        nativeBuildInputs = with pkgs.ocamlPackages; [
          odoc
          ounit
        ];
      };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.ocaml-p2p;
  };
}
