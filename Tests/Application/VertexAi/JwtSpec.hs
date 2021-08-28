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
privateKey = "-----BEGIN PRIVATE KEY-----\nMIIEowIBAAKCAQEAvpnaPKLIKdvx98KW68lz8pGaRRcYersNGqPjpifMVjjE8LuC\noXgPU0HePnNTUjpShBnynKCvrtWhN+haKbSp+QWXSxiTrW99HBfAl1MDQyWcukoE\nb9Cw6INctVUN4iRvkn9T8E6q174RbcnwA/7yTc7p1NCvw+6B/aAN9l1G2pQXgRdY\nC/+G6o1IZEHtWhqzE97nY5QKNuUVD0V09dc5CDYBaKjqetwwv6DFk/GRdOSEd/6b\nW+20z0qSHpa3YNW6qSp+x5pyYmDrzRIR03os6DauZkChSRyc/Whvurx6o85D6qpz\nywo8xwNaLZHxTQPgcIA5su9ZIytv9LH2E+lSwwIDAQABAoIBAFml8cD9a5pMqlW3\nf9btTQz1sRL4Fvp7CmHSXhvjsjeHwhHckEe0ObkWTRsgkTsm1XLu5W8IITnhn0+1\niNr+78eB+rRGngdAXh8diOdkEy+8/Cee8tFI3jyutKdRlxMbwiKsouVviumoq3fx\nOGQYwQ0Z2l/PvCwy/Y82ffq3ysC5gAJsbBYsCrg14bQo44ulrELe4SDWs5HCjKYb\nEI2b8cOMucqZSOtxg9niLN/je2bo/I2HGSawibgcOdBms8k6TvsSrZMr3kJ5O6J+\n77LGwKH37brVgbVYvbq6nWPL0xLG7dUv+7LWEo5qQaPy6aXb/zbckqLqu6/EjOVe\nydG5JQECgYEA9kKfTZD/WEVAreA0dzfeJRu8vlnwoagL7cJaoDxqXos4mcr5mPDT\nkbWgFkLFFH/AyUnPBlK6BcJp1XK67B13ETUa3i9Q5t1WuZEobiKKBLFm9DDQJt43\nuKZWJxBKFGSvFrYPtGZst719mZVcPct2CzPjEgN3Hlpt6fyw3eOrnoECgYEAxiOu\njwXCOmuGaB7+OW2tR0PGEzbvVlEGdkAJ6TC/HoKM1A8r2u4hLTEJJCrLLTfw++4I\nddHE2dLeR4Q7O58SfLphwgPmLDezN7WRLGr7Vyfuv7VmaHjGuC3Gv9agnhWDlA2Q\ngBG9/R9oVfL0Dc7CgJgLeUtItCYC31bGT3yhV0MCgYEA4k3DG4L+RN4PXDpHvK9I\npA1jXAJHEifeHnaW1d3vWkbSkvJmgVf+9U5VeV+OwRHN1qzPZV4suRI6M/8lK8rA\nGr4UnM4aqK4K/qkY4G05LKrik9Ev2CgqSLQDRA7CJQ+Jn3Nb50qg6hFnFPafN+J7\n7juWln08wFYV4Atpdd+9XQECgYBxizkZFL+9IqkfOcONvWAzGo+Dq1N0L3J4iTIk\nw56CKWXyj88d4qB4eUU3yJ4uB4S9miaW/eLEwKZIbWpUPFAn0db7i6h3ZmP5ZL8Q\nqS3nQCb9DULmU2/tU641eRUKAmIoka1g9sndKAZuWo+o6fdkIb1RgObk9XNn8R4r\npsv+aQKBgB+CIcExR30vycv5bnZN9EFlIXNKaeMJUrYCXcRQNvrnUIUBvAO8+jAe\nCdLygS5RtgOLZib0IVErqWsP3EI1ACGuLts0vQ9GFLQGaN1SaMS40C9kvns1mlDu\nLhIhYpJ8UsCVt5snWo2N+M+6ANh5tpWdQnEK6zILh4tRbuzaiHgb\n-----END PRIVATE KEY-----"