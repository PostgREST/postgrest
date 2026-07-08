module Main where

import System.IO
import System.Exit (die)

import Hasql.Connection
import Hasql.Connection.Setting
import Hasql.Connection.Setting.Connection
import Hasql.Notifications

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    dbOrError <- acquire [ connection $ string "postgres://localhost/db_name" ]
    case dbOrError of
        Right db -> do
            let channelToListen = toPgIdentifier "sample-channel"
            listen db channelToListen
            waitForNotifications (\channel _ -> print $ "Just got notification on channel " <> channel) db
        _ -> die "Could not open database connection"
