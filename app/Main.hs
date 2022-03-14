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
    if a then return ()
        else writeFile "account.txt" ""
    if b then return ()
        else writeFile "username.txt" ""
    if c then return ()
        else writeFile "currentLogin.txt" ""
    if d then return ()
        else writeFile "log.txt" ""
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
            writeFile "currentLogin.txt" (username)
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
    let log = "username: " ++ username ++ " has logged out"
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
    putStrLn "\n(a) Add New Patient Data (c) Create File (d) Delete Patient Data (r) Read File (u) Update Patient Data (x) Logout\n"
    menu <- getLine
    putStrLn "\n"
    case menu of
        "a" -> do
            addData
            pilihanMenu
        "c" -> do
            createFile
            pilihanMenu
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

createFile :: IO ()
createFile = do
    putStrLn "Please Enter File Name Without Extension: \n(Example: log instead of log.txt)"
    fileName <- getLine
    writeFile (fileName ++ ".txt") ""
    username <- getCurrentUser
    let log = "File " ++ fileName ++ ".txt" ++ " successfully created by " ++ username ++ "\n"
    createLog "Create File" log

addData :: IO ()
addData = do
    putStrLn "Where do you want to save the data? -don't put the extension\n(Example: log instead of log.txt)"
    fileName <- getLine
    a <- doesFileExist (fileName ++ ".txt")
    if a then do
            putStrLn "Enter Patient Name: "
            patientName <- getLine

            putStrLn "Enter Patient Age: "
            patientAge <- getLine

            putStrLn "Enter Patient Gender: "
            patientGender <- getLine

            username <- getCurrentUser
            let log = patientName ++ " " ++ patientAge ++ " " ++ patientGender ++ " added to file " ++ fileName ++ ".txt by " ++ username

            appendFile (fileName ++ ".txt") (patientName ++ " " ++ patientAge ++ " " ++ patientGender ++ "\n")
            createLog "Add Data" (log ++ "\n")
            putStrLn ("\n" ++ log)
        else do
            putStrLn "File doesn't exist\n"
            addData

readFileData :: IO ()
readFileData = do
    putStrLn "Please Enter File Name Without Extension: \n(Example: log instead of log.txt)"
    fileName <- getLine
    a <- doesFileExist (fileName ++ ".txt")
    if a then do
            putStrLn "\n"
            listData <- readFile (fileName ++ ".txt")
            if listData == "" then putStrLn "This File is still empty"
            else do
                let splittedData = Data.List.Split.splitOn "\n" listData
                mapM_ putStrLn splittedData
            username <- getCurrentUser
            let log = "Data on File " ++ fileName ++ ".txt have been seen by " ++ username ++ "\n"
            createLog "Read File Data" log
            putStrLn ("\n" ++ log)
        else do
            putStrLn "File doesn't exist\n"
            readFileData

updateData :: IO ()
updateData = do
    putStrLn "\nPlease Enter File Name Without Extension: \n(Example: log instead of log.txt)"
    fileName <- getLine
    a <- doesFileExist (fileName ++ ".txt")
    if a then do
            listData <- readFile (fileName ++ ".txt")
            let splittedData = Data.List.Split.splitOn "\n" listData

            putStrLn "\nPlease Enter the Name of the Patient You Want to Update: \n(Example: John Doe)"
            patientName <- getLine
            putStrLn "\nPlease Enter the Age of the Patient You Want to Update: \n(Example: 50)"
            patientAge <- getLine
            putStrLn "\nPlease Enter the Gender of the Patient You Want to Update: \n(Example: Male)"
            patientGender <- getLine
            putStrLn "\n"
            let patientData = patientName ++ " " ++ patientAge ++ " " ++ patientGender
            if patientData `elem` splittedData
                then do
                    putStrLn "\nPlease Enter the Patient New Name: \n(Example: John Doe)"
                    newPatientName <- getLine
                    putStrLn "\nPlease Enter the Patient New Age: \n(Example: 50)"
                    newPatientAge <- getLine
                    putStrLn "\nPlease Enter the Patient New Gender: \n(Example: Male)"
                    newPatientGender <- getLine
                    let newData = newPatientName ++ " " ++ newPatientAge ++ " " ++ newPatientGender
                    let newListData = Data.Text.replace (pack patientData) (pack newData) (pack listData)
                    writeFile (fileName ++ "2" ++ ".txt") (unpack newListData)
                    removeFile (fileName ++ ".txt")
                    renameFile (fileName ++ "2" ++ ".txt") (fileName ++ ".txt")
                    username <- getCurrentUser
                    let log = patientData ++ " have been updated to " ++ newData ++ " by " ++ username
                    createLog "Update Data" (log ++ "\n")
                    putStrLn ("\n" ++ log)
                else putStrLn "No Data Found"
        else do
            putStrLn "File doesn't exist\n"
            updateData

deleteData :: IO ()
deleteData = do
    putStrLn "\nPlease Enter File Name Without Extension: \n(Example: log instead of log.txt)"
    fileName <- getLine
    a <- doesFileExist (fileName ++ ".txt")
    if a then do
            listData <- readFile (fileName ++ ".txt")
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
                    writeFile (fileName ++ "2" ++ ".txt") (unpack newListData)
                    removeFile (fileName ++ ".txt")
                    renameFile (fileName ++ "2" ++ ".txt") (fileName ++ ".txt")
                    username <- getCurrentUser
                    let log = patientData ++ " have been deleted by " ++ username
                    createLog "Delete Data" (log ++ "\n")
                    putStrLn ("\n" ++ log)
                else putStrLn "No Data Found"
        else do 
            putStrLn "File doesn't exist\n"
            deleteData 