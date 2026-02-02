module Main where

import VpnRouter.CmdArgs
import VpnRouter.CmdRun
import VpnRouter.Prelude

main :: IO ()
main = execWithArgs runCmd
