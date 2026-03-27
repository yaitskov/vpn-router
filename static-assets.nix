{ runCommand
, fetchFromGitHub
, buildNpmPackage
}:

let
  browser_wasi_shim = buildNpmPackage {
    pname = "browser_wasi_shim";
    version = "0.4.2";
    src = fetchFromGitHub {
      owner = "haskell-wasm";
      repo = "browser_wasi_shim";
      rev = "0e10ea9465a098d1ee2cf3e09ed050102f0ead1a";
      hash = "sha256-j/UhO3RvTF0NFE8gfbKopjBDdBPn1UdS01PQJixJMZc=";
    };
    npmDepsHash = "sha256-eehX/bQoMo0rfCq6GF4ood0+xbRagMK4gWGXlZtpfJ4=";
    installPhase = ''
      mv dist "$out"
    '';
  };
in
runCommand "dashi-static-assets" { } ''
  mkdir -p "$out"
  cp -r "${browser_wasi_shim}" "$out/browser_wasi_shim"
''
