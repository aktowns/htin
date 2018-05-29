module Builtins.File where

import           Control.Monad.State (lift)
import           Data.Foldable       (traverse_)
import           Data.List           (find)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.IO

import           Builtins.Builtin
import           Environment
import           Types

data FileBuiltin = FileBuiltin deriving (Show)

modeMap = [ (1, "file/read-mode", ReadMode)
          , (2, "file/write-mode", WriteMode)
          , (3, "file/append-mode", AppendMode)
          , (4, "file/read-write-mode", ReadWriteMode)
          ]

lookupMode :: Integer -> Maybe IOMode
lookupMode n = (\(_,_,v) -> v) <$> find (\(k,_,_) -> k == n) modeMap

builtinFileMode :: Context ()
builtinFileMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num v) modeMap

builtinFileOpenDoc = Just "(file/open path mode)\nOpens the given path in mode returning a handle"
builtinFileOpen :: LVal -> Context LVal
builtinFileOpen (SExpr xs)
    | length xs /= 2 = return $ Err "file/open requires 2 arguments, filepath and filemode"
    | (Str file) <- head xs, (Num mode) <- xs !! 1, (Just hmode) <- lookupMode mode = do
        handle <- lift $ openFile (T.unpack file) hmode
        return $ Hnd $ IOHandle handle
    | otherwise = return $ Err $ "file/open handed incorrect arguments expected filepath and filemode, got" <> tshow xs

withHandle :: Text -> (Handle -> Context LVal) -> LVal -> Context LVal
withHandle fn  ctx (SExpr xs)
    | length xs /= 1 = return $ Err $ fn <> " requires 1 argument filehandle"
    | (Hnd (IOHandle fh)) <- head xs = ctx fh
    | otherwise = return $ Err $ fn <> "handed incorrect arguments expected filepath and filemode, got" <> tshow xs

builtinFileReadDoc = Just "(file/read handle)\nReads all contents of the given handle returning a string"
builtinFileRead :: LVal -> Context LVal
builtinFileRead = withHandle "file/read" $ \fh -> do
    contents <- lift $ T.hGetContents fh
    return $ Str contents

builtinFileReadCharDoc = Just "(file/read-char handle)\nReads a character from the handle at its current position"
builtinFileReadChar :: LVal -> Context LVal
builtinFileReadChar = withHandle "file/read-char" $ \fh -> do
    char <- lift $ hGetChar fh
    return $ Str (T.singleton char)

builtinFileReadLineDoc = Just "(file/read-line handle)\nReads a line from the handle at its current position"
builtinFileReadLine :: LVal -> Context LVal
builtinFileReadLine = withHandle "file/read-line" $ \fh -> do
    line <- lift $ T.hGetLine fh
    return $ Str line

builtinFileIsOpenDoc = Just "(file/is-open handle)\nReturns #t if the file handle is currently open"
builtinFileIsOpen :: LVal -> Context LVal
builtinFileIsOpen = withHandle "file/is-open" $ \fh -> do
    isopen <- lift $ hIsOpen fh
    return $ Boolean isopen

builtinFileIsClosedDoc = Just "(file/is-closed handle)\nReturns #t if the file handle is currently closed"
builtinFileIsClosed :: LVal -> Context LVal
builtinFileIsClosed = withHandle "file/is-closed" $ \fh -> do
    isclosed <- lift $ hIsClosed fh
    return $ Boolean isclosed

builtinFileIsReadableDoc = Just "(file/is-readable handle)\nReturns #t if the file handle is ready for reading"
builtinFileIsReadable :: LVal -> Context LVal
builtinFileIsReadable = withHandle "file/is-readable" $ \fh -> do
    isreadable <- lift $ hIsReadable fh
    return $ Boolean isreadable

builtinFileCloseDoc = Just "(file/close handle)\nCloses the given handle, does nothing if handle is already closed. returns nil"
builtinFileClose :: LVal -> Context LVal
builtinFileClose = withHandle "file/close" $ \fh -> do
    lift $ hClose fh
    return $ QExpr []

instance Builtin FileBuiltin where
    builtins _ = [ ("file/open", builtinFileOpenDoc, builtinFileOpen)
                 , ("file/read", builtinFileReadDoc, builtinFileRead)
                 , ("file/read-line", builtinFileReadLineDoc, builtinFileReadLine)
                 , ("file/read-char", builtinFileReadCharDoc, builtinFileReadChar)
                 , ("file/is-open", builtinFileIsOpenDoc, builtinFileIsOpen)
                 , ("file/is-closed", builtinFileIsClosedDoc, builtinFileIsClosed)
                 , ("file/is-readable", builtinFileIsReadableDoc, builtinFileIsReadable)
                 , ("file/close", builtinFileCloseDoc, builtinFileClose)
                 ]
    globals _ = []
    initial _ = builtinFileMode
