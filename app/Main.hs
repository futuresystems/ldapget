{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Monad
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Lib
import           Options.Applicative.Simple
import           System.Directory
import           System.FilePath

version :: String
version = "0.1.0.0"

theHeader :: String
theHeader = "Get values from LDAP"

description :: String
description = ""


data Attribute = SshPublicKey deriving (Eq, Show)

data Params = Params {
    host      :: Host
  , port      :: PortNumber
  , dn        :: Dn
  , project   :: ProjectId
  , attribute :: Attribute
  , outdir    :: FilePath
  } deriving (Eq, Show)


doSshPublicKeys :: Params -> Ldap -> IO ()
doSshPublicKeys prms = \token -> do
  let dn' = dn prms
      project' = project prms
      getter = getSshPublicKey

  users <- listUsers token dn' project'
  keyed <- forM users
           $ \u@(Username uid) ->
               (uid,) <$> listUserData token dn' getter u

  let results = HashMap.fromList keyed
  writeResultsTBS (outdir prms) results


ensureDir :: FilePath -> IO ()
ensureDir path =
  createDirectoryIfMissing True path

writeResultsTBS :: FilePath -> HashMap Text [ByteString] -> IO ()
writeResultsTBS out results = do
  ensureDir out
  forM_ (HashMap.toList results) $ \(k,vs) -> do
    let out' = out </> T.unpack k
    ensureDir $ out'
    forM_ (zip [0..] vs) $ \(i,v) -> do
      let file = out' </> show i
      putStrLn $ "Writing " <> file
      BS.writeFile file v

main' :: Params -> IO ()
main' prms = do
  let (h, p) = (host prms, port prms)
      work = case attribute prms of
        SshPublicKey -> doSshPublicKeys prms

  _ <- withLdap h p work
  return ()


mkAttribute :: String -> Attribute
mkAttribute "sshPublicKey" = SshPublicKey
mkAttribute other = error $ "Unknown attribute " <> other


params :: Parser Params
params = Params
         <$> (fmap Plain
              $ strOption (
                short 'h'
                <> long "host"
                <> metavar "HOST"
                <> value "localhost"
                <> showDefault
                <> help "Hostname of the LDAP server"))
         <*> (option (fmap fromIntegral auto) (
                short 'p'
                <> long "port"
                <> metavar "PORT"
                <> value 938
                <> showDefault
                <> help "Port on the LDAP server to connect to"))
         <*> (fmap (Dn . T.pack)
              $ strOption (
                short 'd'
                <> long "dn"
                <> metavar "DN"
                <> value "dc=futuregrid,dc=org"
                <> showDefault
                <> help "LDAP Distinguished Name"))
         <*> (fmap (ProjectId . BS.pack)
              $ strOption (
                short 'P'
                <> long "project"
                <> metavar "PROJECT"
                <> help "Project ID"))
         <*> (fmap mkAttribute
              $ strOption (
                short 'a'
                <> long "attribute"
                <> metavar "ATTR"
                <> value "sshPublicKey"
                <> showDefault
                <> help "Attribute to select"))
         <*> strOption (
                short 'o'
                <> long "outdir"
                <> metavar "OUT"
                <> help "Output directory")

main :: IO ()
main = do
  (opts, ()) <- simpleOptions
                 version
                 theHeader
                 description
                 params
                 empty

  main' opts
