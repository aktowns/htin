module Builtins.File where

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (traverse_)
import           Data.List              (find)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
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
fileMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num builtinPos v) modeMap

fileSeekMode :: Context ()
fileSeekMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num builtinPos v) seekMap

fileOpenDoc = Just "(file/open path mode)\n\
                   \Opens the given path in mode returning a handle"
fileOpen :: LVal -> Context LVal
fileOpen (SExpr c xs)
  | length xs /= 2 = return $ err c "file/open requires 2 arguments, filepath and filemode"
  | (Str _ file) <- head xs, (Num _ mode) <- xs !! 1, (Just hmode) <- lookupMode mode = do
    handle <- liftIO $ openFile (T.unpack file) hmode
    return $ Hnd c $ IOHandle handle
  | otherwise = return $ err c $ "file/open handed incorrect arguments expected filepath and filemode, got" <> tshow xs

withHandle :: Text -> (Handle -> Context LVal) -> LVal -> Context LVal
withHandle fn ctx (SExpr c xs)
  | length xs /= 1 = return $ err c $ fn <> " requires 1 argument filehandle"
  | (Hnd _ (IOHandle fh)) <- head xs = ctx fh
  | otherwise = return $ err c $ fn <> "handed incorrect arguments expected filepath and filemode, got" <> tshow xs

fileReadDoc = Just "(file/read handle)\n\
                   \Reads all contents of the given handle returning a string"
fileRead :: LVal -> Context LVal
fileRead c = withHandle "file/read" (\fh -> (Str $ pos c) <$> liftIO (T.hGetContents fh)) c

fileReadCharDoc = Just "(file/read-char handle)\n\
                       \Reads a character from the handle at its current position"
fileReadChar :: LVal -> Context LVal
fileReadChar c = withHandle "file/read-char" (\fh -> do
  char <- liftIO $ hGetChar fh
  return $ Str (pos c) (T.singleton char)) c

fileReadLineDoc = Just "(file/read-line handle)\n\
                       \Reads a line from the handle at its current position"
fileReadLine :: LVal -> Context LVal
fileReadLine c = withHandle "file/read-line" (\fh -> Str (pos c) <$> liftIO (T.hGetLine fh)) c

fileWriteDoc = Just "(file/write handle data)\n\
                    \Writes the given data to the file handle"
fileWrite :: LVal -> Context LVal
fileWrite (SExpr c xs)
  | length xs /= 2 = return $ err c "file/write requires 2 arguments, filehandle and data"
  | (Hnd _ (IOHandle fh)) <- head xs, (Str _ contents) <- xs !! 1 = do
    contents <- liftIO $ T.hPutStr fh contents
    return $ QExpr c []
  | otherwise = return $ err c $ "file/write handed incorrect arguments expected filehandle and data, got" <> tshow xs

fileFlushDoc = Just "(file/flush handle)\n\
                    \Causes any items buffered for output in handle to be sent \
                    \immediately to the operating system."
fileFlush :: LVal -> Context LVal
fileFlush c = withHandle "file/flush" (\fh -> do
  line <- liftIO $ hFlush fh
  return $ QExpr (pos c) []) c

fileTellDoc = Just "(file/tell handle)\n\
                   \Gives the current position in the file"
fileTell :: LVal -> Context LVal
fileTell c = withHandle "file/tell" (\fh -> Num (pos c) <$> liftIO (hTell fh)) c

fileSeekDoc = Just "(file/seek handle mode position)\n\
                   \Sets the position of handle depending on mode. The offset \
                   \is given in terms of 8-bit bytes."
fileSeek :: LVal -> Context LVal
fileSeek (SExpr c xs)
  | length xs /= 3 = return $ err c "file/seek requires 3 arguments, filehandle, mode and position"
  | (Hnd _ (IOHandle fh)) <- head xs
  , (Num _ mode')         <- xs !! 1
  , (Num _ pos)           <- xs !! 2
  , (Just mode) <- lookupSeek mode'
   = do
    contents <- liftIO $ hSeek fh mode pos
    return $ QExpr c []
  | otherwise = return $ err c $ "file/seek handed incorrect arguments expected filehandle, mode and position, got" <> tshow xs

fileIsOpenDoc = Just "(file/is-open handle)\n\
                     \Returns #t if the file handle is currently open"
fileIsOpen :: LVal -> Context LVal
fileIsOpen c = withHandle "file/is-open" (\fh -> Boolean (pos c) <$> liftIO (hIsOpen fh)) c

fileIsClosedDoc = Just "(file/is-closed handle)\n\
                       \Returns #t if the file handle is currently closed"
fileIsClosed :: LVal -> Context LVal
fileIsClosed c = withHandle "file/is-closed" (\fh -> Boolean (pos c) <$> liftIO (hIsClosed fh)) c

fileIsReadableDoc = Just "(file/is-readable handle)\n\
                         \Returns #t if the file handle is ready for reading"
fileIsReadable :: LVal -> Context LVal
fileIsReadable c = withHandle "file/is-readable" (\fh -> Boolean (pos c) <$> liftIO (hIsReadable fh)) c

fileIsWritableDoc = Just "(file/is-writable handle)\n\
                         \Returns #t if the file handle is ready for writing"
fileIsWritable :: LVal -> Context LVal
fileIsWritable c = withHandle "file/is-writable" (\fh -> Boolean (pos c) <$> liftIO (hIsWritable fh)) c

fileIsSeekableDoc = Just "(file/is-seekable handle)\n\
                         \Returns #t if the file handle is ready for seeking"
fileIsSeekable :: LVal -> Context LVal
fileIsSeekable c = withHandle "file/is-seekable" (\fh -> Boolean (pos c) <$> liftIO (hIsSeekable fh)) c

fileIsEofDoc = Just "(file/is-eof handle)\n\
                    \Returns #t if the file handle is at the end of the file"
fileIsEof :: LVal -> Context LVal
fileIsEof c = withHandle "file/is-eof" (\fh -> Boolean (pos c) <$> liftIO (hIsEOF fh)) c

fileSizeDoc = Just "(file/size handle)\n\
                    \Returns the size of associated file in 8-bit bytes."
fileSize :: LVal -> Context LVal
fileSize c = withHandle "file/size" (\fh -> Num (pos c) <$> liftIO (hFileSize fh)) c

fileSetSizeDoc = Just "(file/set-size handle size)\n\
                      \Truncates the physical file with handle to size bytes."
fileSetSize :: LVal -> Context LVal
fileSetSize (SExpr c xs)
  | length xs /= 2 = return $ err c "file/set-size requires 2 arguments, filehandle and size"
  | (Hnd p (IOHandle fh)) <- head xs, (Num _ sz) <- xs !! 1 = do
      liftIO $ hSetFileSize fh sz
      return $ QExpr p []
  | otherwise = return $ err c $ "file/set-size handed incorrect arguments expected filehandle and size, got" <> tshow xs

fileCloseDoc = Just "(file/close handle)\n\
                    \Closes the given handle, does nothing if handle is already \
                    \closed. returns nil"
fileClose :: LVal -> Context LVal
fileClose c = withHandle "file/close" (\fh -> do
  liftIO $ hClose fh
  return $ QExpr (pos c) []) c

instance Builtin FileBuiltin where
  builtins _ = [ ("file/open",        fileOpenDoc,       fileOpen)
               , ("file/read",        fileReadDoc,       fileRead)
               , ("file/read-line",   fileReadLineDoc,   fileReadLine)
               , ("file/read-char",   fileReadCharDoc,   fileReadChar)
               , ("file/write",       fileWriteDoc,      fileWrite)
               , ("file/flush",       fileFlushDoc,      fileFlush)
               , ("file/tell",        fileTellDoc,       fileTell)
               , ("file/seek",        fileSeekDoc,       fileSeek)
               , ("file/size",        fileSizeDoc,       fileSize)
               , ("file/set-size",    fileSetSizeDoc,    fileSetSize)
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
