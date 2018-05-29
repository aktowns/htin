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

modeMap = [ (1, "file/mode-read", ReadMode)
          , (2, "file/mode-write", WriteMode)
          , (3, "file/mode-append", AppendMode)
          , (4, "file/mode-read-write", ReadWriteMode)
          ]
seekMap = [ (1, "file/seek-mode-absolute", AbsoluteSeek)
          , (2, "file/seek-mode-relative", RelativeSeek)
          , (3, "file/seek-mode-from-end", SeekFromEnd)
          ]

lookupMode :: Integer -> Maybe IOMode
lookupMode n = (\(_,_,v) -> v) <$> find (\(k,_,_) -> k == n) modeMap

lookupSeek :: Integer -> Maybe SeekMode
lookupSeek n = (\(_,_,v) -> v) <$> find (\(k,_,_) -> k == n) seekMap

fileMode :: Context ()
fileMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num v) modeMap

fileSeekMode :: Context ()
fileSeekMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num v) seekMap

fileOpenDoc = Just "(file/open path mode)\n\
                   \Opens the given path in mode returning a handle"
fileOpen :: LVal -> Context LVal
fileOpen (SExpr xs)
    | length xs /= 2 = return $ Err "file/open requires 2 arguments, filepath and filemode"
    | (Str file) <- head xs, (Num mode) <- xs !! 1, (Just hmode) <- lookupMode mode = do
        handle <- lift $ openFile (T.unpack file) hmode
        return $ Hnd $ IOHandle handle
    | otherwise = return $ Err $ "file/open handed incorrect arguments expected filepath and filemode, got" <> tshow xs

withHandle :: Text -> (Handle -> Context LVal) -> LVal -> Context LVal
withHandle fn ctx (SExpr xs)
    | length xs /= 1 = return $ Err $ fn <> " requires 1 argument filehandle"
    | (Hnd (IOHandle fh)) <- head xs = ctx fh
    | otherwise = return $ Err $ fn <> "handed incorrect arguments expected filepath and filemode, got" <> tshow xs

fileReadDoc = Just "(file/read handle)\n\
                   \Reads all contents of the given handle returning a string"
fileRead :: LVal -> Context LVal
fileRead = withHandle "file/read" $ \fh -> Str <$> (lift $ T.hGetContents fh)

fileReadCharDoc = Just "(file/read-char handle)\n\
                       \Reads a character from the handle at its current position"
fileReadChar :: LVal -> Context LVal
fileReadChar = withHandle "file/read-char" $ \fh -> do
    char <- lift $ hGetChar fh
    return $ Str (T.singleton char)

fileReadLineDoc = Just "(file/read-line handle)\n\
                       \Reads a line from the handle at its current position"
fileReadLine :: LVal -> Context LVal
fileReadLine = withHandle "file/read-line" $ \fh -> Str <$> (lift $ T.hGetLine fh)

fileWriteDoc = Just "(file/write handle data)\n\
                    \Writes the given data to the file handle"
fileWrite :: LVal -> Context LVal
fileWrite (SExpr xs)
  | length xs /= 2 = return $ Err "file/write requires 2 arguments, filehandle and data"
  | (Hnd (IOHandle fh)) <- head xs, (Str contents) <- xs !! 1 = do
    contents <- lift $ T.hPutStr fh contents
    return $ QExpr []
  | otherwise = return $ Err $ "file/write handed incorrect arguments expected filehandle and data, got" <> tshow xs

fileFlushDoc = Just "(file/flush handle)\n\
                    \Causes any items buffered for output in handle to be sent \
                    \immediately to the operating system."
fileFlush :: LVal -> Context LVal
fileFlush = withHandle "file/flush" $ \fh -> do
    line <- lift $ hFlush fh
    return $ QExpr []

fileTellDoc = Just "(file/tell handle)\n\
                   \Gives the current position in the file"
fileTell :: LVal -> Context LVal
fileTell = withHandle "file/tell" $ \fh -> Num <$> (lift $ hTell fh)

fileSeekDoc = Just "(file/seek handle mode position)\n\
                   \Sets the position of handle depending on mode. The offset \
                   \is given in terms of 8-bit bytes."
fileSeek :: LVal -> Context LVal
fileSeek (SExpr xs)
  | length xs /= 3 = return $ Err "file/seek requires 3 arguments, filehandle, mode and position"
  | (Hnd (IOHandle fh)) <- head xs
  , (Num mode')         <- xs !! 1
  , (Num pos)           <- xs !! 2
  , (Just mode) <- lookupSeek mode'
   = do
    contents <- lift $ hSeek fh mode pos
    return $ QExpr []
  | otherwise = return $ Err $ "file/seek handed incorrect arguments expected filehandle, mode and position, got" <> tshow xs

fileIsOpenDoc = Just "(file/is-open handle)\n\
                     \Returns #t if the file handle is currently open"
fileIsOpen :: LVal -> Context LVal
fileIsOpen = withHandle "file/is-open" $ \fh -> Boolean <$> (lift $ hIsOpen fh)

fileIsClosedDoc = Just "(file/is-closed handle)\n\
                       \Returns #t if the file handle is currently closed"
fileIsClosed :: LVal -> Context LVal
fileIsClosed = withHandle "file/is-closed" $ \fh -> Boolean <$> (lift $ hIsClosed fh)

fileIsReadableDoc = Just "(file/is-readable handle)\n\
                         \Returns #t if the file handle is ready for reading"
fileIsReadable :: LVal -> Context LVal
fileIsReadable = withHandle "file/is-readable" $ \fh -> Boolean <$> (lift $ hIsReadable fh)

fileIsWritableDoc = Just "(file/is-writable handle)\n\
                         \Returns #t if the file handle is ready for writing"
fileIsWritable :: LVal -> Context LVal
fileIsWritable = withHandle "file/is-writable" $ \fh -> Boolean <$> (lift $ hIsWritable fh)

fileIsSeekableDoc = Just "(file/is-seekable handle)\n\
                         \Returns #t if the file handle is ready for seeking"
fileIsSeekable :: LVal -> Context LVal
fileIsSeekable = withHandle "file/is-seekable" $ \fh -> Boolean <$> (lift $ hIsSeekable fh)

fileIsEofDoc = Just "(file/is-eof handle)\n\
                    \Returns #t if the file handle is at the end of the file"
fileIsEof :: LVal -> Context LVal
fileIsEof = withHandle "file/is-eof" $ \fh -> Boolean <$> (lift $ hIsEOF fh)

fileCloseDoc = Just "(file/close handle)\n\
                    \Closes the given handle, does nothing if handle is already \
                    \closed. returns nil"
fileClose :: LVal -> Context LVal
fileClose = withHandle "file/close" $ \fh -> do
    lift $ hClose fh
    return $ QExpr []

instance Builtin FileBuiltin where
    builtins _ = [ ("file/open",        fileOpenDoc,       fileOpen)
                 , ("file/read",        fileReadDoc,       fileRead)
                 , ("file/read-line",   fileReadLineDoc,   fileReadLine)
                 , ("file/read-char",   fileReadCharDoc,   fileReadChar)
                 , ("file/write",       fileWriteDoc,      fileWrite)
                 , ("file/flush",       fileFlushDoc,      fileFlush)
                 , ("file/tell",        fileTellDoc,       fileTell)
                 , ("file/seek",        fileSeekDoc,       fileSeek)
                 , ("file/is-open",     fileIsOpenDoc,     fileIsOpen)
                 , ("file/is-closed",   fileIsClosedDoc,   fileIsClosed)
                 , ("file/is-readable", fileIsReadableDoc, fileIsReadable)
                 , ("file/is-writable", fileIsWritableDoc, fileIsWritable)
                 , ("file/is-seekable", fileIsSeekableDoc, fileIsSeekable)
                 , ("file/is-eof",      fileIsEofDoc,      fileIsEof)
                 , ("file/close",       fileCloseDoc,      fileClose)
                 ]
    globals _ = []
    initial _ = fileMode >> fileSeekMode
