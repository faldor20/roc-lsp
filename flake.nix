
{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {
    nixpkgs.url = "nixpkgs"; # also valid: "nixpkgs"
    roc={
    url="github:roc-lang/roc";
    inputs.nixpkgs.follows="nixpkgs";

    };

  };
  # Flake outputs
  outputs = { self, nixpkgs, roc,... }@inputs:
    let
      # Systems supported
      allSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
      ];

      # Helper to provide system-specific attributes
      forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
        roc=(import roc).packages.${system}  ;

      });
    in
    {
      # Development environment output
      devShells = forAllSystems ({ pkgs, roc }: {
        default = pkgs.mkShell {
          packages =  with pkgs; [
          # roc.default
       
          dotnet-sdk_8
          dotnet-runtime_8
          fantomas
          fsautocomplete
          ];
        shellHook = ''
        '';
        };

        });

    };
}
