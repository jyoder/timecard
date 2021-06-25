let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "79fc5438a04b186c54f55161adf1d3c912a1a518";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            base
            base64-bytestring
            bytestring
            cabal-install
            cryptonite
            hlint
            hspec
            hspec-discover
            lens-aeson
            memory
            monad-loops
            p.ihp
            raw-strings-qq
            req
            string-interpolate
            text
            time
            wai
        ];
        otherDeps = p: with p; builtins.concatLists [[
            ghcid
        ]
        (if pkgs.stdenv.isDarwin then [] else [
            wkhtmltopdf
        ])];
        projectPath = ./.;
    };
in
    haskellEnv
