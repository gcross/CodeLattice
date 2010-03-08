-- @+leo-ver=4-thin
-- @+node:gcross.20100302201317.1268:@thin Setup.hs
-- @@language Haskell

import Blueprint.Tools.GHC.Main

main =
    simpleDefaultMain
        ("","sources")
        (Just (
         ("","tests"),
         [],
         ["HUnit == 1.*"
         ,"test-framework == 0.2.*"
         ,"test-framework-hunit == 0.2.*"
         ,"test-framework-quickcheck2 == 0.2.*"
         ,"QuickCheck == 2.1.*"
         ,"random == 1.*"
         ,"array == 0.2.*"
         ,"ChasingBottoms == 1.2.*"
         ]
        ))
        []
-- @-node:gcross.20100302201317.1268:@thin Setup.hs
-- @-leo
