module Web.Job.ProcessEvents where
import Web.Controller.Prelude

instance Job ProcessEventsJob where
    perform ProcessEventsJob { .. } = do
        putStrLn "Hello World!"
