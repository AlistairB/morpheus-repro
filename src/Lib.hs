{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    ) where

import Data.Morpheus.Client
import GHC.Generics (Generic)
import Data.Text
import Data.Time
import Data.Functor
import Data.Time.Format.ISO8601
import Control.Monad.Fail
import Data.String (fromString, IsString)
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Req
import Data.Text.Encoding

defineByDocumentFile
  "./minimal-github.graphql"
  [gql|
    query FetchRepoFile ($repoOwner: String!, $repoName: String!, $expression: String!) {
      repository(name: $repoName, owner: $repoOwner) {
        object(expression: $expression) {
          ... on Blob {
            isTruncated
            text
          }
        }
      }
    }
  |]

runApp :: IO ()
runApp = do
  result <- githubFetchRepoFile
  print result


githubFetchRepoFile :: IO (Either String FetchRepoFile)
githubFetchRepoFile = do
  let args = produceFetchRepoFileInput
      -- create new personal token https://github.com/settings/tokens
      -- it doesn't require any additional privelages (ie. ignore all the checkboxes)
      authToken = "insert token here"
  qryFetchRepoFile authToken args


qryFetchRepoFile :: Text -> FetchRepoFileArgs -> IO (Either String FetchRepoFile)
qryFetchRepoFile = fetch . executeGraphQL

produceFetchRepoFileInput :: FetchRepoFileArgs
produceFetchRepoFileInput =
  FetchRepoFileArgs
    { repoOwner = "facebook",
      repoName = "react",
      expression = "af219cc6e6c514099a667ffab4e2d80c5c0c1bcc:.nvmrc"
    }

executeGraphQL :: Text -> L.ByteString -> IO L.ByteString
executeGraphQL authToken payload = runReq defaultHttpConfig $ do
    let headers = header "Content-Type" "application/json"
               <> header "Authorization" ("token " <> encodeUtf8 authToken)
               <> header "Accept" "application/vnd.github.antiope-preview+json"
               <> header "User-Agent" "morpheus-repro"
    responseBody
        <$> req POST
                (https "api.github.com" /: "graphql")
                (ReqBodyLbs payload)
                lbsResponse
                headers

-- newtype GitObjectID = GitObjectID {
--   _gitObjectIDText :: Text
--   } deriving (Show, Generic)

-- instance GQLScalar GitObjectID where
--   parseValue (String x) = Right $ GitObjectID x
--   parseValue _          = Left "GitObjectId must be a String"
--   serialize (GitObjectID value) = String value

-- newtype GitTimestamp = GitTimestamp {
--     _gitTimestampTime :: UTCTime
--   }  deriving (Show, Generic)

-- instance GQLScalar GitTimestamp where
--   parseValue (String x) = iso8601ParseM (unpack x) <&> GitTimestamp
--   parseValue _          = Left "GitTimestamp must be a String"
--   serialize (GitTimestamp value) = String (pack $ iso8601Show value)

newtype DateTime = DateTime {
  _datetimeTime :: UTCTime
  } deriving (Eq, Show, Generic)

instance GQLScalar DateTime where
  parseValue (String x) = iso8601ParseM (unpack x) <&> DateTime
  parseValue _          = Left "DateTime must be a String"
  serialize (DateTime value) = String (pack $ iso8601Show value)

instance IsString str => MonadFail (Either str) where
    fail :: String -> Either str a
    fail = Left . fromString
    {-# INLINE fail #-}
