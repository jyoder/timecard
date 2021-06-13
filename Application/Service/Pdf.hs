module Application.Service.Pdf (render) where

import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import IHP.Prelude
import System.IO (Handle, hClose, hIsOpen)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

render :: Html -> IO Lazy.ByteString
render htmlFragment = do
    bootstrap <- loadBootstrap

    let htmlDocument =
            renderHtml htmlFragment
                |> encodeUtf8
                |> fullHtmlDocument bootstrap

    bracket
        createConvertProcess
        (\(inHandle, _) -> ensureClosed inHandle)
        ( \(inHandle, outHandle) -> do
            Lazy.hPut inHandle htmlDocument
            ensureClosed inHandle
            Lazy.hGetContents outHandle
        )
  where
    ensureClosed handle = do
        isOpen <- hIsOpen handle
        if isOpen then hClose handle else pure ()

loadBootstrap :: IO Lazy.ByteString
loadBootstrap = Lazy.readFile bootstrapPath

bootstrapPath :: String
bootstrapPath = "build/ihp-lib/static/vendor/bootstrap.min.css"

fullHtmlDocument :: Lazy.ByteString -> Lazy.ByteString -> Lazy.ByteString
fullHtmlDocument bootstrap htmlFragment =
    "<html><head><style>"
        <> bootstrap
        <> "</style></head><body>"
        <> htmlFragment
        <> "</body></html>"

createConvertProcess :: IO (Handle, Handle)
createConvertProcess = do
    (Just inHandle, Just outHandle, _, _) <-
        createProcess
            convertCommand
                { std_in = CreatePipe
                , std_out = CreatePipe
                }
    pure (inHandle, outHandle)

convertCommand :: CreateProcess
convertCommand = proc convertBinary ["-", "-"]

convertBinary :: FilePath
convertBinary = "wkhtmltopdf"