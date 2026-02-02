module VpnRouter.Test.Net where

import VpnRouter.Prelude
import VpnRouter.Net

import Test.Tasty
import Test.Tasty.HUnit

unit_parseIpV4 :: IO ()
unit_parseIpV4 = do
  assertEqual "4 zeros" (Just $ HostIp 0x0) (parseIpV4 "0.0.0.0")
  assertEqual "all 255" (Just $ HostIp 0xff_ff_ff_ff) (parseIpV4 "255.255.255.255")

test_line_parsers :: TestTree
test_line_parsers =
  testGroup "line parsers"
  [ testCase "parseIptablesLine"
    $ parseIptablesLine iptl @?= Just (LineNumber 1, PacketMark 2, ClientAdr $ HostIp 0x10_01_00_FE)
  , testGroup "parseDefaultRoutingLine"
    [ testCase "table match"
      $ parseDefaultRoutingLine rt3 rt3l @?= Just (Tagged $ HostIp 0x01_01_01_01, Tagged "wlp2s0")
    , testCase "table don't match" $ parseDefaultRoutingLine rt3 rt33l @?= Nothing
    , testCase "main table match"
      $ parseDefaultRoutingLine mainRoutingTableName mtl
      @?= Just (Tagged $ HostIp 0x01_01_01_01, Tagged "wlp2s0")
    ]
  , testGroup "ip rules"
    [ testCase "ok"
      $ parseRoutingTableMarkLine ruleL @?= Just (RuleId 32765, PacketMark 2)
    ]
  ]
  where
    ruleL = "32765:\tfrom all fwmark 0x2 lookup 7"
    iptl = "1   MARK       all  --  254.0.1.16        0.0.0.0/0            MARK set 0x2"
    rt3l = "default via 1.1.1.1 dev wlp2s0 table 3"
    rt33l = "default via 1.1.1.1 dev wlp2s0 table 33"
    mtl = "default via 1.1.1.1 dev wlp2s0 ee"
    rt3 = RoutingTableId 3
