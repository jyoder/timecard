module Tests.Support (
    testConfig,
    withApp,
    itIO,
    toDay,
    toUtc,
    toTimeOfDay,
    toTimeZone,
) where

import qualified Application.Twilio.Client as Twilio.Client
import qualified Application.VertexAi.Client as VertexAi.Client
import Control.Exception (bracket)
import qualified Data.Vault.Lazy as Vault
import qualified Database.PostgreSQL.Simple as PG
import IHP.ApplicationContext (ApplicationContext (..))
import qualified IHP.AutoRefresh.Types as AutoRefresh
import IHP.Controller.RequestContext (RequestBody (..), RequestContext (..))
import IHP.ControllerPrelude
import IHP.Environment
import qualified IHP.FrameworkConfig as FrameworkConfig
import IHP.Log.Types
import qualified IHP.Test.Database as Database
import IHP.Test.Mocking
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec
import Text.Read (read)
import Prelude (show)

newtype ForceTransactionRollback a = ForceTransactionRollback a
    deriving (Typeable)

instance Show (ForceTransactionRollback a) where
    show x = ""

instance (Typeable a) => Exception (ForceTransactionRollback a)

instance InitControllerContext RootApplication where
    initContext = pure ()

testConfig :: ConfigBuilder
testConfig = do
    option Development
    option (AppHostname "localhost")
    option Twilio.Client.DisabledConfig
    option VertexAi.Client.DisabledConfig

withApp ::
    (InitControllerContext application) =>
    application ->
    ConfigBuilder ->
    (MockContext application -> IO ()) ->
    IO ()
withApp application configBuilder hspecAction = do
    frameworkConfig@FrameworkConfig
        { dbPoolMaxConnections
        , dbPoolIdleTime
        , databaseUrl
        } <-
        FrameworkConfig.buildFrameworkConfig configBuilder

    withDatabaseConnection
        databaseUrl
        ( \databaseConnection -> do
            logger <- newLogger def {level = Warn} -- don't log queries
            modelContext <-
                createModelContext
                    dbPoolIdleTime
                    dbPoolMaxConnections
                    databaseUrl
                    logger

            autoRefreshServer <- newIORef AutoRefresh.newAutoRefreshServer
            session <- Vault.newKey
            let sessionVault = Vault.insert session mempty Vault.empty
            let applicationContext =
                    ApplicationContext
                        { modelContext = modelContext
                        , session
                        , autoRefreshServer
                        , frameworkConfig
                        }

            let requestContext =
                    RequestContext
                        { request = defaultRequest {vault = sessionVault}
                        , requestBody = FormBody [] []
                        , respond = const (pure ResponseReceived)
                        , vault = session
                        , frameworkConfig = frameworkConfig
                        }

            hspecAction MockContext {..}
        )
  where
    withDatabaseConnection databaseUrl =
        bracket (PG.connectPostgreSQL databaseUrl) PG.close

itIO ::
    Example (MockContext application -> IO ()) =>
    String ->
    ((ContextParameters application) => IO ()) ->
    SpecWith (Arg (MockContext application -> IO ()))
itIO description block =
    it ("[IO] " <> description) $
        withContext $ withTransactionRollback block

withTransactionRollback ::
    forall a.
    (?modelContext :: ModelContext, Typeable a) =>
    ((?modelContext :: ModelContext) => IO a) ->
    IO a
withTransactionRollback block = do
    catch
        ( withTransaction do
            result <- block
            throw $ ForceTransactionRollback result
        )
        (\(ForceTransactionRollback result) -> pure result)

toUtc :: String -> UTCTime
toUtc time = zonedTimeToUTC (read time :: ZonedTime)

toDay :: String -> Day
toDay = read

toTimeOfDay :: String -> TimeOfDay
toTimeOfDay = read

toTimeZone :: String -> TimeZone
toTimeZone = read
