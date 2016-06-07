module InputOutput where

import System.IO
import System.Directory
import Control.Monad
import Data.Char
import Data.List

-- The empty tuple is a value of () and it also has a type of ()
-- An I/O action will be performed when we give it a name of main and then run our program

helloWorld :: IO()
helloWorld = putStrLn "Hello, Haskell!"

youRock :: IO ()
youRock = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- in a do block, the last action cannot be bound to a name; IO actions that return the unit value can safely be ignored
-- to get the value out of an I/O action, you have to perform it inside another I/O action by binding it to a name with <-
-- I/O actions will only be performed when they are given a name of main or when they're inside a bigger I/O action that we composed with a do block


-- let can be used in do-blocks similarly to list-comprehension (i.e. omitting the 'in' part)
-- use <- when you want to bind results of I/O actions to names and you can use let bindings to bind pure expressions to names
howAreyou :: IO()
howAreyou = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"


startReverseInputs :: IO()
startReverseInputs = do
    putStrLn "Reversal of all requests has started (termination on empty input)"
    reverseInputs

reverseInputs :: IO()
reverseInputs = do
    line <- getLine
    if null line
        then return () -- makes an I/O action out of a pure value
        else do
            putStrLn $ reverseWords line
            reverseInputs

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- When dealing with I/O do blocks, we mostly use return either because we need to create an I/O action that doesn't
-- do anything or because we don't want the I/O action that's made up from a do block to have the result value of its
-- last action, but we want it to have a different result value, so we use return to make an I/O action that always has
-- our desired result contained and we put it at the end.


-- putStr doesn't jump into a new line after printing out the string while putStrLn does
-- putStr is defined recursively with the help of putChar;
-- putChar takes a character and returns an I/O action that will print it out to the terminal
-- print takes a value of any type that's an instance of Show (basically: putStrLn . show)
-- getChar is an I/O action that reads a character from the input.

readTextPrintFirstWord :: IO ()
readTextPrintFirstWord = do
    putStrLn "Reading text input and printing back the first word"
    word <- readWord
    putStrLn $ "The first word of the input: " ++ word

readWord :: IO [Char]
readWord = readCharIncrementally []

-- TODO: Should be an inner function readWord; keep on getting "parse error on input ‘c’" when using this as "where" clause
readCharIncrementally :: [Char] -> IO [Char]
readCharIncrementally str = do
    c <- getChar
    if c /= ' '
      then readCharIncrementally $ str ++ [c]
      else return str

-- sequence takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other.
-- sequence :: [IO a] -> IO [a]

-- mapM takes a function and a list, maps the function over the list and then sequences it.
-- mapM_ does the same, only it throws away the result later

-- forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever.
uppercaseAllInput = forever $ do
    putStrLn "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

-- forM (located in Control.Monad) is like mapM, only that it has its parameters switched around
-- think of it as meaning: make an I/O action for every element in this list; perform those actions and bind their results to something
colorAssociation :: IO ()
colorAssociation = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM_ putStrLn colors


filterShortLines :: IO ()
filterShortLines = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

-- or alternatively: unlines . filter ((<10) . length) . lines

-- === File IO === --
-- openFile :: FilePath -> IOMode -> IO Handle
-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- hGetContents takes a file handle which tells it which file to read from

-- alternatively: withFile allows the use of a lambda
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile' path mode f = do
--     handle <- openFile path mode
--     result <- f handle
--     hClose handle
--     return result

readGirlgriends = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)


-- hGetLine, hPutStr, hPutStrLn, hGetChar work just like their counterparts without the h, only they take a handle as a
-- parameter and operate on that specific file instead of operating on standard input or standard output

-- readFile :: FilePath -> IO String
-- writeFile :: FilePath -> String -> IO ()
-- appendFile :: FilePath -> String -> IO ()

appendTodo = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

-- hSetBuffering allows the control over buffering
-- data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
-- hFlush takes a handle and returns an I/O action that will flush the buffer of the file associated with the handle

removeTodo = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

todoMenu :: IO ()
todoMenu = do
    putStrLn "What would you like to do\n1) Add a todo\n2) Remove a todo\n3) Stop"
    command <- getLine
    case command of
      "1" -> do
        appendTodo
        todoMenu
      "2" -> do
        removeTodo
        todoMenu
      "3" -> return ()
      x -> do
        putStrLn $ "ERROR!!! Not programmed to understand input: " ++ x
        todoMenu
