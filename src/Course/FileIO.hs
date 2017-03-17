{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \args ->
    case args of
      fn :. Nil -> run fn
      _         -> putStrLn "usage: runhaskell FileIO.hs filename"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run filename = readFile filename >>= (\content -> getFiles $ lines content) >>= printFiles
{-
  do
    content <- readFile filename
    results <- getFiles (lines content)
    printFiles results
-}

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles fps = sequence $ map getFile fps
--  sequence . (<$>) getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fn = readFile fn >>= \content -> pure (fn, content)
--  lift2 (<$>) (,) readFile

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles = void . sequence . map (\(fn, content) -> printFile fn content)
--  void . sequence . (<$>) (uncurry printFile)

printFile ::
  FilePath
  -> Chars
  -> IO ()
{-
printFile fn content = do
  putStr "============ "
  putStrLn fn
  putStrLn content
  putStrLn ""
-}

printFile fn content = putStr "============ " >> putStrLn fn >> putStrLn content >> putStrLn ""
{-
  putStrLn ("============ " ++ fn) >>
  putStrLn content
-}

