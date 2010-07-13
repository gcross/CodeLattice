-- @+leo-ver=4-thin
-- @+node:gcross.20100315191926.2391:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100315191926.2392:<< Language extensions >>
-- @-node:gcross.20100315191926.2392:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100315191926.2393:<< Import needed modules >>
import Control.Applicative

import System.FilePath

import Blueprint.Configuration
import Blueprint.Tools.GCC
import Blueprint.Tools.GHC.Main
-- @nonl
-- @-node:gcross.20100315191926.2393:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100315191926.2394:Types
-- @+node:gcross.20100315191926.2395:AdditionalConfiguration
data AdditionalConfiguration = AdditionalConfiguration
    {   gccConfiguration :: GCCConfiguration
    }
-- @-node:gcross.20100315191926.2395:AdditionalConfiguration
-- @-node:gcross.20100315191926.2394:Types
-- @+node:gcross.20100315191926.2396:Functions
-- @+node:gcross.20100315191926.2397:configureAdditional
configureAdditional =
    AdditionalConfiguration
        <$> (configureUsingSection "GCC")
-- @-node:gcross.20100315191926.2397:configureAdditional
-- @+node:gcross.20100315191926.2398:compileAdditional
compileAdditional _ configuration build_root digest_cache_subdirectory object_subdirectory interface_subdirectory =
    gccCompileAll
        (gccConfiguration configuration)
        digest_cache_subdirectory
        gcc_flags
        object_subdirectory
-- @-node:gcross.20100315191926.2398:compileAdditional
-- @-node:gcross.20100315191926.2396:Functions
-- @+node:gcross.20100315191926.2399:Values
-- @+node:gcross.20100315191926.2400:Additional Options
additional_options =
    [   gccOptions
    ]
-- @-node:gcross.20100315191926.2400:Additional Options
-- @+node:gcross.20100315191926.2401:Flags
data FlagMode = Debug | Optimized
flag_mode = Optimized

ghc_flags =
    case flag_mode of
        Debug → []
        Optimized → ["-O2","-fvia-C","-optc=-O3"]

gcc_flags =
    case flag_mode of
        Debug → ["-g"]
        Optimized → ["-O3","-ffast-math","-funroll-loops"]
-- @nonl
-- @-node:gcross.20100315191926.2401:Flags
-- @-node:gcross.20100315191926.2399:Values
-- @+node:gcross.20100315191926.2402:main
main =
    defaultMain
        configureAdditional
        compileAdditional
        additional_options
        ("","sources")
        (Just
           (("","tests")
           ,[]
           ,["HUnit == 1.*"
            ,"test-framework >= 0.2 && < 0.4"
            ,"test-framework-hunit == 0.2.*"
            ,"test-framework-quickcheck2 == 0.2.*"
            ,"QuickCheck == 2.1.*"
            ,"random == 1.*"
            ,"array >= 0.2 && < 0.4"
            ,"ChasingBottoms >= 1.2 && < 1.4"
            ]
           )
        )
        ghc_flags
-- @-node:gcross.20100315191926.2402:main
-- @-others
-- @-node:gcross.20100315191926.2391:@thin Setup.hs
-- @-leo
