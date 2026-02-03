{-# OPTIONS_GHC -freduction-depth=0 #-}
module VpnRouter.Net.Iptables where

import Data.Conduit.Process ( sourceCmdWithConsumer )
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import Text.Regex.TDFA ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import VpnRouter.App ( NetM, ex )
import VpnRouter.Bash ( bash )
import VpnRouter.Net.Types
    ( PacketMark(..),
      ClientAdr(..),
      LineNumber,
      clientAdrToDec4,
      pipeline,
      parseIpV4 )
import VpnRouter.Prelude

iptables :: IsString s => s
iptables = "iptables"

listMarkedSources :: NetM m => m [(LineNumber, PacketMark, ClientAdr)]
listMarkedSources = do
  (ec, l) <- sourceCmdWithConsumer bashCmd (pipeline parseIptablesLine)
  case ec of
    ExitSuccess -> pure l
    ExitFailure erc -> ex $ printf "Failed to list marking rules; due %d" erc
  where
    bashCmd = iptables <> " -t mangle -L PREROUTING -n --line-numbers"

rmMarkingRule  :: NetM m => LineNumber -> m ()
rmMarkingRule ln =
  bash iptables [ "-t", "mangle", "-D", "PREROUTING", show ln]

addMarkingRule :: NetM m => ClientAdr -> PacketMark -> m ()
addMarkingRule ca (PacketMark pm) =
  bash iptables [ "-t", "mangle", "-I", "PREROUTING"
                  , "-s", clientAdrToDec4 ca
                  , "-j", "MARK"
                  , "--set-mark", show pm
                  ]

{-
iptables -t mangle -L PREROUTING -n
Chain PREROUTING (policy ACCEPT)
num target     prot opt source               destination
1   MARK       all  --  192.168.11.14        0.0.0.0/0            MARK set 0x2
-}
mangleLinePattern :: String
mangleLinePattern =
  "^([[:digit:]]+)" <> -- "num" column
  "[[:space:]]+MARK" <> -- "target" column
  "[[:space:]]+[^[:space:]]+" <> -- skip "prot" column
  "[[:space:]]+[^[:space:]]+" <> -- skip "opt" column
  "[[:space:]]+([[:digit:].]+)" <> -- source column
  "[[:space:]]+[^[:space:]]+" <> -- skip "destination" column
  "[[:space:]]+MARK set (0x[[:digit:]]+)" -- extra column

parseIptablesLine :: Text -> Maybe (LineNumber, PacketMark, ClientAdr)
parseIptablesLine l =
  case getAllTextSubmatches (l =~ mangleLinePattern) of
    ([_full, lnGr, sourceIp, pmGroup] :: [Text]) ->
      case readEither $ toString lnGr of
       Left e -> error . toText $ printf "failed to parse line number (%s) in: %s due %s" lnGr l e
       Right lineNum ->
         case parseIpV4 $ toString sourceIp of
           Nothing -> error . toText @String $ printf "failed to parse client IP in: %s" l
           Just clientSourceIp ->
             case readEither $ toString pmGroup of
               Left e -> error . toText @String $ printf "failed to parse packet mark in: %s due %s" l e
               Right pm -> pure ( lineNum
                                , PacketMark pm
                                , ClientAdr clientSourceIp
                                )
    _ -> Nothing
