let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.defaultCabalSteps
        haskellCi.DhallMatrix::{
        , ghc =
          [ haskellCi.GHC.GHC963
          , haskellCi.GHC.GHC947
          , haskellCi.GHC.GHC928
          , haskellCi.GHC.GHC8107
          , haskellCi.GHC.GHC884
          ]
        , cabal = [ haskellCi.Cabal.Cabal310, haskellCi.Cabal.Cabal34 ]
        , os = [ haskellCi.OS.Ubuntu2204, haskellCi.OS.Ubuntu2004 ]
        }
    : haskellCi.CI.Type
