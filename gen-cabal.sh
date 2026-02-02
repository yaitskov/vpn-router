#!/usr/bin/env bash

sed  "/^description:/bx ; b ; :x ; n ; e pandoc --to=haddock README.md | sed -E -e 's/^/    /' -e '1,3d' ;" \
    vpn-router.cabal.template > vpn-router.cabal
