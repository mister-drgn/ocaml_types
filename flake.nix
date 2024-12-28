{
  description = "ocaml_types package.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    
  }; 
  outputs = { self, nixpkgs, ... }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    types = pkgs.ocamlPackages.buildDunePackage rec {
      pname = "types";
      version = "0.0.1";

      minimalOCamlVersion = "5.2";

      src = ./.;

      # propagatedBuildInputs = with pkgs; [
      #   ocamlPackages.str
      # ];

      meta = {
        description = "Haskell-like type classes, but some other features";
        # license = lib.licenses.isc;
        # maintainers = with lib.maintainers; [
        #   alexfmpe
        #   vbgl
        # ];
        # homepage = "https://github.com/mirage/ocaml-conduit";
      };
    };
  in {
    # packages.ocaml-types = types;
    defaultPackage.x86_64-linux = types;
  };
}
