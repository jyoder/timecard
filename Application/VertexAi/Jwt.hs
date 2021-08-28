module Application.VertexAi.Jwt (
    Params (..),
    make,
) where

import Data.Text (replace)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import IHP.Prelude hiding (exp)
import Web.JWT

data Params = Params
    { now :: !UTCTime
    , serviceName :: !Text
    , serviceAccountEmail :: !Text
    , privateKeyId :: !Text
    , privateKey :: !Text
    }
    deriving (Eq, Show)

make :: Params -> Either Text Text
make Params {..} =
    case (key, audience) of
        (Just key, Just audience) -> do
            Right $ encodeSigned key joseHeader' $ claimsSet audience
        (Nothing, _) ->
            Left "Failed to parse private key"
        (_, Nothing) ->
            Left "Failed to format audience"
  where
    key = RSAPrivateKey <$> readRsaSecret (encodeUtf8 keyWithNewlines)
    keyWithNewlines = replace "\\n" "\n" privateKey
    audience = stringOrURI serviceName
    joseHeader' = joseHeader privateKeyId
    claimsSet audience = mempty {iat, iss, exp, sub, aud = Just $ Left audience}
    iat = numericDate $ utcTimeToPOSIXSeconds now
    iss = stringOrURI serviceAccountEmail
    exp = numericDate $ utcTimeToPOSIXSeconds $ addUTCTime jwtLifespan now
    sub = stringOrURI serviceAccountEmail

joseHeader :: Text -> JOSEHeader
joseHeader privateKeyId =
    JOSEHeader
        { typ = Just "JWT"
        , cty = Nothing
        , alg = Just RS256
        , kid = Just privateKeyId
        }

jwtLifespan :: NominalDiffTime
jwtLifespan = 600
