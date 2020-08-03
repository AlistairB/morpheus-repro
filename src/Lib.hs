{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib
    ( someFunc
    ) where

import Data.Morpheus.Client
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

defineByDocumentFile
  "./minimal-github.graphql"
  [gql|
    query FetchRepoHeadCommit ($repoOwner: String!, $repoName: String!) {
      repository(name: $repoName, owner: $repoOwner) {
        defaultBranchRef {
          target {
            ... on Commit {
               id
            }
          }
        }
      }
    }
  |]


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

-- newtype DateTime = DateTime {
--   _datetimeTime :: UTCTime
--   } deriving (Show, Generic)

-- instance GQLScalar DateTime where
--   parseValue (String x) = iso8601ParseM (unpack x) <&> DateTime
--   parseValue _          = Left "DateTime must be a String"
--   serialize (DateTime value) = String (pack $ iso8601Show value)
