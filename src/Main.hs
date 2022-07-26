{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified GitHub as GH

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)

import System.Environment (getArgs, lookupEnv)
import System.Exit (exitWith, ExitCode(..))


data Config = Config
  { cfgRepoOwner :: !Text
  , cfgRepoName  :: !Text
  , cfgRepoRef   :: !Text
  , cfgToken     :: !Text
  , cfgDir       :: !Text
  }

data Canonical = Canonical
  { canonicalOwner :: !Text
  , canonicalName  :: !Text
  , canonicalRef   :: !Text
  }

type M a = ExceptT GH.Error IO a

main :: IO ()
main = runExceptT (getConfig >>= getGHContents) >>= presentResults

getConfig :: M Config
getConfig = do
  (Canonical{..}, dir) <- ExceptT (parseArguments <$> getArgs)
  token <- getTokenFromEnv
  return $ Config canonicalOwner canonicalName canonicalRef token dir

getTokenFromEnv :: M Text
getTokenFromEnv =
  let var    = "GHTOKEN"
      errmsg = GH.UserError $ T.pack var <> " not found"
   in maybe (throwError errmsg) (return . T.pack) =<< liftIO (lookupEnv var)

parseArguments :: [String] -> Either GH.Error (Canonical, Text)
parseArguments (canonical:dir) =
  (,) <$> parseCanonical canonical
      <*> pure (parseDir dir)
parseArguments _ = userErrorGH "Expected at least one CLI argument"

parseCanonical :: String -> Either GH.Error Canonical
parseCanonical canonical = do
  (repo, canonicalRef) <- split '#' (T.pack canonical)
  (canonicalOwner, canonicalName) <- split '/' repo
  return Canonical{..}

parseDir :: [String] -> Text
parseDir [] = ""
parseDir (x:_) = T.pack x

split :: Char -> Text -> Either GH.Error (Text, Text)
split c txt =
  let (h, t) = T.break (== c) txt
   in if T.null t
         then userErrorGH $ "Missing " <> (T.cons c " in canonical GH repo")
         else case T.uncons t of
                Nothing -> userErrorGH $ "Empty string after " <> (T.cons c " in canonical GH repo")
                Just (_, rest) -> pure (h, rest)

getGHContents :: Config -> M [Text]
getGHContents Config{..} =
  let owner = GH.mkName (Proxy :: Proxy GH.Owner) cfgRepoOwner
      repo  = GH.mkName (Proxy :: Proxy GH.Repo) cfgRepoName
      ref   = Just cfgRepoRef
      auth  = GH.OAuth (T.encodeUtf8 cfgToken)
      req   = GH.contentsForR owner repo cfgDir ref
   in ExceptT $ (extractFiles =<<) <$> GH.github auth req

extractFiles :: GH.Content -> Either GH.Error [Text]
extractFiles (GH.ContentFile _) = userErrorGH "Expecting DIRECTORY Contents"
extractFiles (GH.ContentDirectory v) = pure (foldr fileNames [] v)
  where
    fileNames :: GH.ContentItem -> [Text] -> [Text]
    fileNames (GH.ContentItem GH.ItemFile content) = (GH.contentName content:)
    fileNames _ = id

presentResults :: Either GH.Error [Text] -> IO ()
presentResults (Right []) = putStrLn "NO FILES"
presentResults (Right rs) = do
  putStrLn "GOT SOME RESULTS"
  mapM_ print rs
presentResults (Left err) = do
  putStrLn "*** ERRORS!!!"
  print err
  exitWith (ExitFailure 1)

userErrorGH :: Text -> Either GH.Error a
userErrorGH = Left . GH.UserError
