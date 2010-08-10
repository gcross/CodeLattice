-- @+leo-ver=4-thin
-- @+node:gcross.20100809233207.1698:@thin plot-all-tilings-results.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100809233207.1700:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100809233207.1700:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100809233207.1702:<< Import needed modules >>
import Control.Monad

import System.Exit
import System.IO
import System.Process

import CodeLattice.Tilings
-- @-node:gcross.20100809233207.1702:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100809233207.1703:main
main = forM_ (map tilingName tilings) $ \tiling_name → do
    let filename =
            "results-"
            ++ map (\c → if c == ' ' then '-' else c) tiling_name ++
            ".p"
    exit_code ← execCmd "./plot-tiling-results" [tiling_name,filename ++ "s"]
    when (exit_code == ExitSuccess) $ do
        execCmd "ps2pdf" [filename ++ "s",filename ++ "df"]
        return ()
  where
    execCmd command arguments = do
        putStrLn . unwords . (command:) . map (('"':) . (++ "\"")) $ arguments
        rawSystem command arguments
-- @-node:gcross.20100809233207.1703:main
-- @-others
-- @-node:gcross.20100809233207.1698:@thin plot-all-tilings-results.hs
-- @-leo
