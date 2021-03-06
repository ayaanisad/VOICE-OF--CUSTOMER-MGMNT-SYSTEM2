module Module.Item where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)

data LogItem
    = LogItem
        { itemId :: Int
        , noInet :: Int
        , name :: String
        , storage :: Int
        , agenctb :: String
        , description :: String
        }
    | UnknownItem
    deriving (Show, Eq)

addNewItem :: [LogItem] -> String -> Int -> Int -> String -> String -> IO [LogItem]
addNewItem oldLogItemList name noInet storage agenctb description = do
    let lastId =
            if null oldLogItemList
                then 0
                else itemId $ last oldLogItemList
        newId = lastId + 1
        newLogItem =
            LogItem
                { itemId = newId
                , noInet = noInet
                , name = name
                , storage = storage
                , agenctb = agenctb
                , description = description
                }
    let newLogItemList = oldLogItemList ++ [newLogItem]
    return newLogItemList

restockItem :: [LogItem] -> Int -> Int -> IO [LogItem]
restockItem oldLogItemList choice amount = do
    let itemExist = find (\item -> (itemId item) == choice) oldLogItemList

        extractItem :: Maybe LogItem -> LogItem
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [LogItem] -> LogItem -> Int -> [LogItem]
        replaceItem [] chosenItem amount = []
        replaceItem (item : rest) chosenItem amount
            | item == chosenItem = [item{storage = storage item + amount}] ++ replaceItem rest chosenItem amount
            | otherwise = [item] ++ replaceItem rest chosenItem amount

    let restockedLogItemList =
            if (extractItem itemExist) == UnknownItem
                then oldLogItemList
                else replaceItem oldLogItemList (extractItem itemExist) amount

    if (extractItem itemExist) == UnknownItem
        then putStrLn "Item not found. Please check your Internet Number"
        else
            if amount == 0
                then putStrLn "Amount inserted is zero. Are you sure you've input it correctly?"
                else putStrLn "Successfully restocked item!"

    return restockedLogItemList

takeItem :: [LogItem] -> Int -> Int -> IO [LogItem]
takeItem oldLogItemList choice amount = do
    let itemExist = find (\item -> (itemId item) == choice) oldLogItemList

        extractItem :: Maybe LogItem -> LogItem
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [LogItem] -> LogItem -> Int -> [LogItem]
        replaceItem [] chosenItem amount = []
        replaceItem (item : rest) chosenItem amount
            | item == chosenItem = [item{storage = storage item - amount}] ++ replaceItem rest chosenItem amount
            | otherwise = [item] ++ replaceItem rest chosenItem amount

    let updatedLogItemList =
            if (extractItem itemExist) == UnknownItem
                then oldLogItemList
                else
                    if amount > (storage $ extractItem itemExist)
                        then oldLogItemList
                        else replaceItem oldLogItemList (extractItem itemExist) amount

    if (extractItem itemExist) == UnknownItem
        then putStrLn "Item not found. Please check your ItemID"
        else
            if amount > (storage $ extractItem itemExist)
                then putStrLn "Not enough storage quantity to take items."
                else
                    if amount == 0
                        then putStrLn "Amount inserted is zero. Are you've sure you input it correctly?"
                        else putStrLn "Successfully restocked item!"

    return updatedLogItemList

parseLogItem :: [LogItem] -> IO ()
parseLogItem logItemList = do
    let convertToLog :: [LogItem] -> String
        convertToLog [] = ""
        convertToLog (logItem : rest) =
            show (itemId logItem)
                ++ " "
                ++ show (noInet logItem)
                ++ " "
                ++ name logItem
                ++ " "
                ++ show (storage logItem)
                ++ " "
                ++ agenctb logItem
                ++ " "
                ++ description logItem
                ++ "\n"
                ++ convertToLog rest
    let parsedLogItem = init $ convertToLog logItemList -- using init to remove the last \n at the end of the .log
    writeFile "log/items.log" parsedLogItem

parseItem :: String -> [LogItem]
parseItem rawContent = map parseSingleItem (lines rawContent)

parseSingleItem :: String -> LogItem
parseSingleItem str = case words str of
    (i : t : n : s : a: d) -> makeItem i t n s a d
    _ -> UnknownItem

makeItem :: String -> String -> String -> String -> String -> [String] -> LogItem
makeItem itemId name noInet storage agenctb description =
    LogItem
        { itemId = read itemId
        , noInet = read noInet
        , name = name
        , storage = read storage
        , agenctb = agenctb
        , description = unwords description
        }