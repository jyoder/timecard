module Tests.Application.VertexAi.JwtSpec where

import qualified Application.VertexAi.Jwt as Jwt
import Control.Monad (join)
import qualified Data.Map as Map
import IHP.Prelude
import Test.Hspec
import Tests.Support
import Web.JWT

spec :: Spec
spec = do
    describe "make" do
        it "returns an error when the private key is not properly formatted" do
            Jwt.make
                Jwt.Params
                    { now = toUtc "2021-08-30 15:30:00 PDT"
                    , serviceName = "https://service.name.com/"
                    , serviceAccountEmail = "service@account.com"
                    , privateKeyId = "1234"
                    , privateKey = "invalid private key"
                    }
                `shouldBe` Left "Failed to parse private key"

        it "returns a JWT with the properly serialized fields" do
            let audience = case stringOrURI "https://service.name.com/" of
                    Just audience -> audience
                    Nothing -> error "Failed to parse audience"

            let jwt =
                    Jwt.make
                        Jwt.Params
                            { now = toUtc "2021-08-30 15:30:00 PDT"
                            , serviceName = "https://service.name.com/"
                            , serviceAccountEmail = "service@account.com"
                            , privateKeyId = "1234"
                            , privateKey
                            }
            let (jwtHeader, jwtClaims) = case jwt of
                    Left _ ->
                        (Nothing, Nothing)
                    Right jwt ->
                        let decodedJwt = decode jwt
                         in (header <$> decodedJwt, claims <$> decodedJwt)

            jwtHeader
                `shouldBe` Just
                    JOSEHeader
                        { typ = "JWT"
                        , cty = Nothing
                        , alg = Just RS256
                        , kid = "1234"
                        }

            jwtClaims
                `shouldBe` Just
                    JWTClaimsSet
                        { iss = stringOrURI "service@account.com"
                        , sub = stringOrURI "service@account.com"
                        , aud = Just $ Left audience
                        , exp = numericDate 1630363200
                        , nbf = Nothing
                        , iat = numericDate 1630362600
                        , jti = Nothing
                        , unregisteredClaims =
                            ClaimsMap
                                { unClaimsMap = Map.empty
                                }
                        }

privateKey :: Text
privateKey = "LS0tLS1CRUdJTiBQUklWQVRFIEtFWS0tLS0tCk1JSUVvd0lCQUFLQ0FRRUF2cG5hUEtMSUtkdng5OEtXNjhsejhwR2FSUmNZZXJzTkdxUGpwaWZNVmpqRThMdUMKb1hnUFUwSGVQbk5UVWpwU2hCbnluS0N2cnRXaE4raGFLYlNwK1FXWFN4aVRyVzk5SEJmQWwxTURReVdjdWtvRQpiOUN3NklOY3RWVU40aVJ2a245VDhFNnExNzRSYmNud0EvN3lUYzdwMU5DdncrNkIvYUFOOWwxRzJwUVhnUmRZCkMvK0c2bzFJWkVIdFdocXpFOTduWTVRS051VVZEMFYwOWRjNUNEWUJhS2pxZXR3d3Y2REZrL0dSZE9TRWQvNmIKVysyMHowcVNIcGEzWU5XNnFTcCt4NXB5WW1EcnpSSVIwM29zNkRhdVprQ2hTUnljL1dodnVyeDZvODVENnFwegp5d284eHdOYUxaSHhUUVBnY0lBNXN1OVpJeXR2OUxIMkUrbFN3d0lEQVFBQkFvSUJBRm1sOGNEOWE1cE1xbFczCmY5YnRUUXoxc1JMNEZ2cDdDbUhTWGh2anNqZUh3aEhja0VlME9ia1dUUnNna1RzbTFYTHU1VzhJSVRuaG4wKzEKaU5yKzc4ZUIrclJHbmdkQVhoOGRpT2RrRXkrOC9DZWU4dEZJM2p5dXRLZFJseE1id2lLc291VnZpdW1vcTNmeApPR1FZd1EwWjJsL1B2Q3d5L1k4MmZmcTN5c0M1Z0FKc2JCWXNDcmcxNGJRbzQ0dWxyRUxlNFNEV3M1SENqS1liCkVJMmI4Y09NdWNxWlNPdHhnOW5pTE4vamUyYm8vSTJIR1Nhd2liZ2NPZEJtczhrNlR2c1NyWk1yM2tKNU82SisKNzdMR3dLSDM3YnJWZ2JWWXZicTZuV1BMMHhMRzdkVXYrN0xXRW81cVFhUHk2YVhiL3piY2txTHF1Ni9Fak9WZQp5ZEc1SlFFQ2dZRUE5a0tmVFpEL1dFVkFyZUEwZHpmZUpSdTh2bG53b2FnTDdjSmFvRHhxWG9zNG1jcjVtUERUCmtiV2dGa0xGRkgvQXlVblBCbEs2QmNKcDFYSzY3QjEzRVRVYTNpOVE1dDFXdVpFb2JpS0tCTEZtOUREUUp0NDMKdUtaV0p4QktGR1N2RnJZUHRHWnN0NzE5bVpWY1BjdDJDelBqRWdOM0hscHQ2Znl3M2VPcm5vRUNnWUVBeGlPdQpqd1hDT211R2FCNytPVzJ0UjBQR0V6YnZWbEVHZGtBSjZUQy9Ib0tNMUE4cjJ1NGhMVEVKSkNyTExUZncrKzRJCmRkSEUyZExlUjRRN081OFNmTHBod2dQbUxEZXpON1dSTEdyN1Z5ZnV2N1ZtYUhqR3VDM0d2OWFnbmhXRGxBMlEKZ0JHOS9SOW9WZkwwRGM3Q2dKZ0xlVXRJdENZQzMxYkdUM3loVjBNQ2dZRUE0azNERzRMK1JONFBYRHBIdks5SQpwQTFqWEFKSEVpZmVIbmFXMWQzdldrYlNrdkptZ1ZmKzlVNVZlVitPd1JITjFxelBaVjRzdVJJNk0vOGxLOHJBCkdyNFVuTTRhcUs0Sy9xa1k0RzA1TEtyaWs5RXYyQ2dxU0xRRFJBN0NKUStKbjNOYjUwcWc2aEZuRlBhZk4rSjcKN2p1V2xuMDh3RllWNEF0cGRkKzlYUUVDZ1lCeGl6a1pGTCs5SXFrZk9jT052V0F6R28rRHExTjBMM0o0aVRJawp3NTZDS1dYeWo4OGQ0cUI0ZVVVM3lKNHVCNFM5bWlhVy9lTEV3S1pJYldwVVBGQW4wZGI3aTZoM1ptUDVaTDhRCnFTM25RQ2I5RFVMbVUyL3RVNjQxZVJVS0FtSW9rYTFnOXNuZEtBWnVXbytvNmZka0liMVJnT2JrOVhObjhSNHIKcHN2K2FRS0JnQitDSWNFeFIzMHZ5Y3Y1Ym5aTjlFRmxJWE5LYWVNSlVyWUNYY1JRTnZyblVJVUJ2QU84K2pBZQpDZEx5Z1M1UnRnT0xaaWIwSVZFcnFXc1AzRUkxQUNHdUx0czB2UTlHRkxRR2FOMVNhTVM0MEM5a3ZuczFtbER1CkxoSWhZcEo4VXNDVnQ1c25XbzJOK00rNkFOaDV0cFdkUW5FSzZ6SUxoNHRSYnV6YWlIZ2IKLS0tLS1FTkQgUFJJVkFURSBLRVktLS0tLQo="