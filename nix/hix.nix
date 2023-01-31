{pkgs, ...}: 
{
  name = "fxttr";
  compiler-nix-name = "ghc925";

  shell = {
    tools = {
      cabal = "3.8.1.0";
      hlint = "3.5";
      haskell-language-server = "1.9.0.0";
      ormolu = "0.5.2.0";
      happy = "1.20.0";
      stack = "2.9.3";
    };

    exactDeps = true;
    
    additional = ps: with ps; [ Cabal ];
  };
}
