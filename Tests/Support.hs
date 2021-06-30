module Tests.Support (
    testConfig,
    withTransactionRollback,
) where

import IHP.ControllerPrelude
import IHP.Environment
import IHP.Test.Mocking
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

testConfig :: IO ConfigBuilder
testConfig = pure do
    option Development
    option (AppHostname "localhost")

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
