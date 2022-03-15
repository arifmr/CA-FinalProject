{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split ( splitOn )
import Data.Char ( toLower, isSpace )
import Data.List ( elemIndices, dropWhileEnd, dropWhile )
import Data.Text ( replace, pack, unpack )
import System.Directory
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Control.Exception ( bracket_ )
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer ( WriterT, tell, runWriterT )
import Data.Functor.Identity

main :: IO ()
main = do
    putStrLn "\nWelcome to Covid-19 Patient List App"

    a <- doesFileExist "account.txt"
    b <- doesFileExist "username.txt"
    c <- doesFileExist "currentLogin.txt"
    d <- doesFileExist "log.txt"
    e <- doesFileExist "patient.txt"

    if a then return ()
        else writeFile "account.txt" "admin admin\n"
    if b then return ()
        else writeFile "username.txt" "admin\n"
    if c then return ()
        else writeFile "currentLogin.txt" ""
    if d then return ()
        else writeFile "log.txt" ""
    if e then return ()
        else writeFile "patient.txt" ""

    putStrLn "\nPlease login first. If you don't have an account please register first\n(l) Login (r) Register (x) Exit"
    answer <- getLine
    case answer of
        "l" -> do
            login
            pilihanMenu
        "r" -> do
            register
            main
        "x" -> putStrLn "Exit"
        _   -> do
            putStrLn "Please choose first letter from menu!"
            main

login :: IO ()
login = do
    putStr "\nUsername: "
    hFlush stdout
    username <- getLine
    putStr "Password: "
    hFlush stdout
    password <- withEcho False getLine
    putChar '\n'
    accountList <- readFile "account.txt"
    let splittedData = Data.List.Split.splitOn "\n" accountList
    if (username ++ " " ++ password) `elem` splittedData
        then do
            putStrLn "Login Successfully!"
            writeFile "currentLogin.txt" username
            let log = "username: " ++ username ++ " has logged in\n"
            createLog "Login" log
            putStrLn ("\n" ++ log)
            return ()
        else do
            putStrLn "Incorrect username or password"
            login

logout :: IO ()
logout = do
    username <- getCurrentUser
    let log = "username: " ++ username ++ " has logged out\n"
    createLog "Logout" log
    writeFile "currentLogin.txt" ""
    putStrLn ("\n" ++ log)

register :: IO ()
register = do
    username <- getUsername
    password <- getPassword
    appendFile "username.txt" (username ++ "\n")
    appendFile "account.txt" (username ++ " " ++ password ++ "\n")
    putStrLn "Account Created Successfully!"

getUsername :: IO String
getUsername = do
    putStr "\nUsername: "
    hFlush stdout
    username <- withEcho True getLine
    if length username > 4
        then do
            usernameList <- readFile "username.txt"
            let splittedData = Data.List.Split.splitOn "\n" usernameList
            if username `elem` splittedData
                then do
                    putStrLn "Username already used"
                    putChar '\n'
                    getUsername
                else do
                    writeFile "username2.txt" usernameList
                    removeFile "username.txt"
                    renameFile "username2.txt" "username.txt"
                    return username
        else do
            putStrLn "Username length must be greater than 4"
            putChar '\n'
            getUsername

getPassword :: IO String
getPassword = do
    putStr "Password: "
    hFlush stdout
    pass <- withEcho False getLine
    if length pass > 4
        then do
            putChar '\n'
            return pass
        else do
            putStrLn "Password length must be greater than 4"
            getPassword

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

pilihanMenu :: IO ()
pilihanMenu = do
    putStrLn "\n(a) Add New Patient Data (d) Delete Patient Data (r) Read File (u) Update Patient Data (x) Logout\n"
    menu <- getLine
    putStrLn "\n"
    case menu of
        "a" -> do
            addData
            pilihanMenu
        -- "c" -> do
        --     createFile
        --     pilihanMenu
        "d" -> do
            deleteData
            pilihanMenu
        "r" -> do
            readFileData
            pilihanMenu
        "u" -> do
            updateData
            pilihanMenu
        "x" -> do
            logout
            main
        _   -> do
            putStrLn "Please choose first letter from menu!"
            pilihanMenu
    
createLog :: String -> String -> IO ()
createLog method action = do
    user <- getCurrentUser
    let log = runIdentity . runWriterT $ setLog user method action
    appendFile "log.txt" $ "username: " ++ user ++ ", method: " ++ method ++ ", action: " ++ action

setLog :: String -> String -> String -> WriterT String Identity String
setLog user method action = do
    tell $ method ++ ", " ++ action
    return user

getCurrentUser :: IO String 
getCurrentUser = readFile "currentLogin.txt"

-- createFile :: IO ()
-- createFile = do
--     putStrLn "Please Enter File Name Without Extension: \n(Example: log instead of log.txt)"
--     fileName <- getLine
--     writeFile (fileName ++ ".txt") ""
--     username <- getCurrentUser
--     let log = "File " ++ fileName ++ ".txt" ++ " successfully created by " ++ username ++ "\n"
--     createLog "Create File" log

addData :: IO ()
addData = do
    putStrLn "Enter Patient Name: "
    patientName <- getLine

    putStrLn "Enter Patient Age: "
    patientAge <- getLine

    putStrLn "Enter Patient Gender: "
    patientGender <- getLine

    username <- getCurrentUser
    let log = patientName ++ " " ++ patientAge ++ " " ++ patientGender ++ " added to file " ++ "patient.txt by " ++ username

    appendFile "patient.txt" (patientName ++ " " ++ patientAge ++ " " ++ patientGender ++ "\n")
    createLog "Add Data" (log ++ "\n")
    putStrLn ("\n" ++ log)

readFileData :: IO ()
readFileData = do
    putStrLn "\n"
    listData <- readFile "patient.txt"
    if listData == "" then putStrLn "This file is still empty"
    else do
        let splittedData = Data.List.Split.splitOn "\n" listData
        mapM_ putStrLn splittedData
    username <- getCurrentUser
    let log = "Data on file patient.txt have been seen by " ++ username ++ "\n"
    createLog "Read File Data" log
    putStrLn ("\n" ++ log)

updateData :: IO ()
updateData = do
    listData <- readFile "patient.txt"
    let splittedData = Data.List.Split.splitOn "\n" listData

    putStrLn "\nPlease Enter the Name of the Patient You Want to Update: \n(Example: John Doe)"
    patientName <- getLine
    putStrLn "\nPlease Enter the Age of the Patient You Want to Update: \n(Example: 50)"
    patientAge <- getLine
    putStrLn "\nPlease Enter the Gender of the Patient You Want to Update: \n(Example: Male)"
    patientGender <- getLine
    putStrLn "\n"

    let patientData = patientName ++ " " ++ patientAge ++ " " ++ patientGender
    if patientData `elem` splittedData then do
        putStrLn "\nPlease Enter the Patient New Name: \n(Example: John Doe)"
        newPatientName <- getLine
        putStrLn "\nPlease Enter the Patient New Age: \n(Example: 50)"
        newPatientAge <- getLine
        putStrLn "\nPlease Enter the Patient New Gender: \n(Example: Male)"
        newPatientGender <- getLine
        let newData = newPatientName ++ " " ++ newPatientAge ++ " " ++ newPatientGender
        let newListData = Data.Text.replace (pack patientData) (pack newData) (pack listData)
        writeFile "patient2.txt" (unpack newListData)
        removeFile "patient.txt"
        renameFile "patient2.txt" "patient.txt"
        username <- getCurrentUser
        let log = patientData ++ " have been updated to " ++ newData ++ " by " ++ username
        createLog "Update Data" (log ++ "\n")
        putStrLn ("\n" ++ log)
    else putStrLn "No Data Found"

deleteData :: IO ()
deleteData = do
    listData <- readFile "patient.txt"
    let splittedData = Data.List.Split.splitOn "\n" listData

    putStrLn "\nPlease Enter the Name of the Patient You Want to Delete: \n(Example: John Doe)"
    patientName <- getLine
    putStrLn "\nPlease Enter the Age of the Patient You Want to Delete: \n(Example: 50)"
    patientAge <- getLine
    putStrLn "\nPlease Enter the Gender of the Patient You Want to Delete: \n(Example: Male)"
    patientGender <- getLine
    putStrLn "\n"

    let patientData = patientName ++ " " ++ patientAge ++ " " ++ patientGender
    if patientData `elem` splittedData
        then do
            let newListData = Data.Text.replace (pack (patientData ++ "\n")) (pack "") (pack listData)
            writeFile "patient2.txt" (unpack newListData)
            removeFile "patient.txt"
            renameFile "patient2.txt" "patient.txt"
            username <- getCurrentUser
            let log = patientData ++ " on file patient.txt have been deleted by " ++ username
            createLog "Delete Data" (log ++ "\n")
            putStrLn ("\n" ++ log)
        else putStrLn "No Data Found"