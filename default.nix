let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.11.0";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            raw-strings-qq
            req
            lens-aeson
            cryptonite
            memory
            base64-bytestring
            bytestring
            time
            monad-loops
            pandoc
        ];
        otherDeps = p: with p; builtins.concatLists [[
        ]
        (if pkgs.stdenv.isDarwin then [] else [
            wkhtmltopdf
        ])];
        projectPath = ./.;
    };
in
    haskellEnv
