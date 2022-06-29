module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import Module.Item (LogItem (UnknownItem), addNewItem, description, itemId, noInet, agenctb, name, parseItem, parseLogItem, restockItem, storage, takeItem)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import System.IO (hFlush, stdout)

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn "\n\n\n=============== Voice of Customer Caring Agen CTB  ==============="
    putStrLn $ replicate 58 '='
    putStrLn $ showItem items
    putStrLn "(a) Show all VOC  (b) Add new VOC  (c) Exit program"
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            putStrLn $ showAllItem items
            empty <- prompt "Press enter to go back"
            runProgram items messages
        
        "b" -> do
            putStrLn "\nYou're about to add new VOC, please fill the information below: "
            itemId <- prompt "No: "
            noInet <- prompt "No Internet: "
            name <- prompt "Nama pelanggan: "
            putStr "Tanggal Call (DDMMYYYY): "
            hFlush stdout
            storage <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            agenctb <- prompt "Nama Agen: "        
            description <- prompt "Hasil VOC: "
            newItems <- addNewItem items noInet name storage agenctb description
            parseLogItem newItems
            logMessage <- makeLogMessage (last newItems) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added new VOC! Press enter to continue."
            runProgram newItems messages
        "c" -> do
            putStrLn "Exiting program..."
            putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram items messages

showItem :: [LogItem] -> String
showItem items = showItemFunc (length items) (take 2 items)
  where
    showItemFunc count [] = case count of
        0 -> "The item list is currently empty.\n" ++ replicate 58 '='
        1 -> "\n" ++ replicate 58 '='
        2 -> "\n" ++ replicate 58 '='
        _ -> "...and " ++ show (count - 2) ++ " more." ++ "\n" ++ replicate 58 '='
    showItemFunc count (item : rest) =
        "No : " ++ show (itemId item)
            ++ "\nNo Internet: "
            ++ show (noInet item)
            ++ "\nNama Pelanggan: "
            ++ name item
            ++ "\nTanggal Call (DDMMYYYY): "
            ++ show (storage item)
            ++ "\nNama Agen: "
            ++ agenctb item
            ++ "\nHasil VOC: "
            ++ description item
            ++ "\n"
            ++ replicate 29 '-'
            ++ "\n"
            ++ showItemFunc count rest

showAllItem :: [LogItem] -> String
showAllItem [] = replicate 58 '='
showAllItem (item : rest) =
    "No: " ++ show (itemId item)
        ++ "\nNo Internet: "
        ++ show (noInet item)
        ++ "\nNama Pelanggan: "
        ++ name item
        ++ "\nTanggal Call (DDMMYYYY): "
        ++ show (storage item)
        ++ "\nNama Agen: "
        ++ agenctb item
        ++ "\nHasil VOC: "
        ++ description item
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllItem rest

main :: IO ()
main = do
    items <- fmap parseItem (readFile "log/items.log")
    runProgram items []

