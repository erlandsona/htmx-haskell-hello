{-# LANGUAGE BlockArguments #-}
module Main (main) where

import           Control.Applicative             ((<|>))
import           Control.Concurrent.Async        (async, waitAny)
import           Data.Foldable                   (traverse_)
import           GHC.Exts                        (IsString (fromString))
import           GHC.Generics                    (Generic)
import qualified Network.Wai.Handler.Warp        as Warp
import qualified Network.Wai.Handler.WarpTLS     as TLS
import qualified Options.Applicative             as OA
import           Servant                         hiding (route)
import           Servant.HTML.Blaze              (HTML)
import           System.FilePath.Posix           (addTrailingPathSeparator)
import           System.IO                       (IOMode (AppendMode), hPrint,
                                                  openFile)
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html5                (Attribute, AttributeValue,
                                                  Html, (!))
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Htmx
import           WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)


type Site = NamedRoutes Site_

data Site_ r = Site_
  { _index :: r :- Get '[HTML] Html
  , _smile :: r :- "smile" :> Get '[HTML] Html
  , _wow   :: r :- "wow" :> Get '[HTML] Html
  , static :: r :- Raw
  }
  deriving Generic

route :: Site_ (AsLink AttributeValue)
route = allFieldLinks' (fromString . show . linkURI)

serveDirectory_ :: FilePath -> Server Raw
serveDirectory_ = serveDirectoryWith . defaultWebAppSettings . addTrailingPathSeparator

style :: Html
style = H.style do
  "body {background-color: linen;}"
  "p {color: maroon;}"
  "@font-face {font-family: 'Berkeley Mono'; src: url('/BerkeleyMono-Regular.woff2') format('woff2');}"
  "span.mono {font-family: 'Berkeley Mono';}"

smile :: Html
smile =
  -- LOOK_MA: Abstraction over common styles for branded components
  btn
    -- LOOK_MA: Typesafe Routing via Servant Links!
    ! hxGet (_wow route)
    ! hxSwap "outerHTML"
    $ "Click me :)"

wow :: Html
wow =
  btn
    ! hxGet (_smile route)
    ! hxSwap "outerHTML"
    $ "Click me :o"

-- LOOK_MA: Abstraction over common styles for branded components
btn :: Html -> Html
btn = H.button ! p5

-- LOOK_MA: Abstraction over common styles for branded components
p5 :: Attribute
p5 = A.class_ "p-5"

index :: Html
index =
  H.docTypeHtml do
    H.head do
      style
      H.script ! A.src "https://unpkg.com/htmx.org@1.8.5" $ ""
      H.script ! A.src "https://cdn.tailwindcss.com" $ ""
    H.body ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "flex flex-col h-full" $ do
        H.div ! A.class_ "align-self-center pt-3 pb-5" $ do
          "Welcome to "
          H.span ! A.class_ "mono" $ "<The Index>"
        smile

makeServer :: FilePath -> Server Site
makeServer contentPath =
  Site_
    { static = serveDirectory_ contentPath
    , _index = pure index
    , _smile = pure smile
    , _wow = pure wow
    }

data TLSMode = NoTLS | TLS {tlsCertPath :: FilePath, tlsKeyPath :: FilePath, tlsPort :: Warp.Port}

tlsModeArgs :: OA.Parser TLSMode
tlsModeArgs =  (TLS <$> tlsCertPath <*> tlsKeyPath <*> tlsPort) <|> pure NoTLS
  where
    tlsPort = OA.option OA.auto (OA.long "tls-port" <> OA.value 443 <> OA.showDefault)
    tlsCertPath = OA.option OA.str (OA.long "tls-cert-path" <> OA.metavar "TLS_CERT_PATH")
    tlsKeyPath = OA.option OA.str (OA.long "tls-key-path" <> OA.metavar "TLS_KEY_PATH")

data Config = Config
  { logPath     :: Maybe FilePath
  , contentPath :: FilePath
  , httpPort    :: Warp.Port
  , tlsMode     :: TLSMode
  }

command :: OA.Parser Config
command = serveCmd
  where
    logPath = OA.optional $ OA.option OA.str (OA.long "log-path" <> OA.short 'l' <> OA.metavar "LOG_PATH")
    contentPath = OA.option OA.str (OA.long "content-path" <> OA.short 'c' <> OA.metavar "CONTENT_PATH" <> OA.value "content" <> OA.showDefault)
    httpPort = OA.option OA.auto (OA.long "http-port" <> OA.value 7373 <> OA.showDefault)
    serveCmd = Config <$> logPath <*> contentPath <*> httpPort <*> tlsModeArgs

main :: IO ()
main = do
  OA.execParser (OA.info (OA.helper <*> command) OA.fullDesc) >>= \case
    Config {..} -> do
      logHandle <- traverse (`openFile` AppendMode) logPath
      let server = makeServer contentPath
          application req response = do
            traverse_ (`hPrint` req) logHandle
            serve (Proxy @Site) server req response
          http = Warp.runSettings (Warp.setPort httpPort Warp.defaultSettings) application
          https tlsSettings tlsPort = TLS.runTLS tlsSettings (Warp.setPort tlsPort Warp.defaultSettings) application
      case tlsMode of
        NoTLS -> http
        TLS {..} -> do
          let tlsSettings = TLS.tlsSettings tlsCertPath tlsKeyPath
          http' <- async http
          https' <- async (https tlsSettings tlsPort)
          (_, ()) <- waitAny [http', https']
          return ()
