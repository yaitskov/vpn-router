function miso() {
    STATIC="${STATIC:-assets}"
    BACKEND="${BACKEND:-http://localhost:8081}"
    CABAL_UI_APP="${CABAL_UI_APP:-jsbundle}"
    [ -d "$STATIC" ] || \
        { echo "Directory $STATIC does not exist. Check environment variable: \$STATIC"
          exit 1
        }
    grep -q -E  "^executable $CABAL_UI_APP"'$' *.cabal  || \
        { cat<<EOF
Cabal files in $PWD don't have executable: [$CABAL_UI_APP].
Check environment variable: \$CABAL_UI_APP
EOF
          exit 1
        }
    if [ $# -eq 0 ] ; then
        miso clean update build optim
    fi
    while [ $# -ne 0 ] ; do
        case "$1" in
            -v) set -x ;;
            update)
	        wasm32-wasi-cabal update ;;
            build)
	        wasm32-wasi-cabal build || break
	        MY_WASM="$(wasm32-wasi-cabal list-bin $CABAL_UI_APP | tail -n 1)"
	        $(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
                                                 --input "$MY_WASM" \
                                                 --output "$STATIC"/ghc_wasm_jsffi.js
	        cp -vf "$MY_WASM" "$STATIC"/app.wasm
                ;;
            optim)
	        wasm-opt -all -O2 "$STATIC"/app.wasm -o "$STATIC"/app.wasm
	        wasm-tools strip -o "$STATIC"/app.wasm "$STATIC"/app.wasm
            ;;
            clean)
                cabal clean
                rm -rf "$STATIC"/*wasm*
                git checkout "$STATIC";;
            serve)
                http-server "$STATIC" -P "$BACKEND" ;;
            -h|--help|help)
                cat<<EOF
Usage: miso [ command ... ]
Commands:
  clean  - remove temp files
  update - wasm cabal update
  build   - build with ghc wasm
  optim  - minimize JS bundle
  serve  - launch http server to serve the frontend

miso without args runs a sequence of: clean update build optim

Default Env Vars:
  STATIC       = $STATIC
  CABAL_UI_APP = $CABAL_UI_APP
  BACKEND      = $BACKEND
EOF
                exit 1
                ;;
            *)
                echo "Bad Command: [$1]; unprocessed arguments: $@"
                exit 1 ;;
        esac
        shift
    done
}
