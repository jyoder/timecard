let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.15.0";
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
            jwt
            lens-aeson
            memory
            monad-loops
            p.ihp
            raw-strings-qq
            regex-tdfa
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
