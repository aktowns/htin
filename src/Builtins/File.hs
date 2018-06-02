module Builtins.File where

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (traverse_)
import           Data.List              (find)
import           Data.Maybe             (fromJust, isJust)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.IO

import           Builtins.Builtin
import           Builtins.Guards
import           Environment
import           Types

data FileBuiltin = FileBuiltin deriving (Show)

modeMap :: [(Integer, Text, IOMode)]
modeMap = [ (1, "file/mode-read", ReadMode)
          , (2, "file/mode-write", WriteMode)
          , (3, "file/mode-append", AppendMode)
          , (4, "file/mode-read-write", ReadWriteMode)
          ]
seekMap :: [(Integer, Text, SeekMode)]
seekMap = [ (1, "file/seek-mode-absolute", AbsoluteSeek)
          , (2, "file/seek-mode-relative", RelativeSeek)
          , (3, "file/seek-mode-from-end", SeekFromEnd)
          ]

lookupMode :: Integer -> Maybe IOMode
lookupMode n = (\(_,_,v) -> v) <$> find (\(k,_,_) -> k == n) modeMap

lookupSeek :: Integer -> Maybe SeekMode
lookupSeek n = (\(_,_,v) -> v) <$> find (\(k,_,_) -> k == n) seekMap

tyMode :: Type IOMode
tyMode = Type "IOMode" (\(Num _ n) -> fromJust $ lookupMode n) (\n -> isNum n && isJust (lookupMode $ unsafeToInt n))

tySeek :: Type SeekMode
tySeek = Type "SeekMode" (\(Num _ n) -> fromJust $ lookupSeek n) (\n -> isNum n && isJust (lookupSeek $ unsafeToInt n))

fileMode :: Context ()
fileMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num builtinPos v) modeMap

fileSeekMode :: Context ()
fileSeekMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num builtinPos v) seekMap

fileOpenDoc = Just "(file/open path mode)\n\
                   \Opens the given path in mode returning a handle"
fileOpen :: LVal -> Context LVal
fileOpen = checked' (properCC *> params 2 *> (argType 1 tyStr <+> argType 2 tyNum) >^ fn)
    where
      fn p (path, mode) | (Just hmode) <- lookupMode mode = do
        handle <- liftIO $ openFile (T.unpack path) hmode
        return $ Hnd p $ IOHandle handle

fileReadDoc = Just "(file/read handle)\n\
                   \Reads all contents of the given handle returning a string"
fileRead :: LVal -> Context LVal
fileRead = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
  where
    fn pos fh = (Str pos) <$> liftIO (T.hGetContents fh)

fileReadCharDoc = Just "(file/read-char handle)\n\
                       \Reads a character from the handle at its current position"
fileReadChar :: LVal -> Context LVal
fileReadChar = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
  where
    fn pos fh = do
      char <- liftIO $ hGetChar fh
      return $ Str pos (T.singleton char)

fileReadLineDoc = Just "(file/read-line handle)\n\
                       \Reads a line from the handle at its current position"
fileReadLine :: LVal -> Context LVal
fileReadLine = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
  where
    fn pos fh = (Str pos) <$> liftIO (T.hGetLine fh)

fileWriteDoc = Just "(file/write handle data)\n\
                    \Writes the given data to the file handle"
fileWrite :: LVal -> Context LVal
fileWrite = checked' (properCC *> params 2 *> (argType 1 tyIOHandle <+> argType 2 tyStr) >^ fn)
    where
      fn pos (fh, contents) = do
        contents' <- liftIO $ T.hPutStr fh contents
        return $ QExpr pos []

fileFlushDoc = Just "(file/flush handle)\n\
                    \Causes any items buffered for output in handle to be sent \
                    \immediately to the operating system."
fileFlush :: LVal -> Context LVal
fileFlush = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where
      fn pos fh = do
        liftIO $ hFlush fh
        return $ QExpr pos []

fileTellDoc = Just "(file/tell handle)\n\
                   \Gives the current position in the file"
fileTell :: LVal -> Context LVal
fileTell = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = (Num pos) <$> liftIO (hTell fh)

fileSeekDoc = Just "(file/seek handle mode position)\n\
                   \Sets the position of handle depending on mode. The offset \
                   \is given in terms of 8-bit bytes."
fileSeek :: LVal -> Context LVal
fileSeek = checked' (properCC *> params 3 *> (argType 1 tyIOHandle <+> argType 2 tySeek <+> argType 3 tyNum >^ fn))
    where
      fn pos ((fh, mode), p) = do
        contents <- liftIO $ hSeek fh mode p
        return $ QExpr pos []

fileIsOpenDoc = Just "(file/is-open handle)\n\
                     \Returns #t if the file handle is currently open"
fileIsOpen :: LVal -> Context LVal
fileIsOpen = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = Boolean pos <$> liftIO (hIsOpen fh)

fileIsClosedDoc = Just "(file/is-closed handle)\n\
                       \Returns #t if the file handle is currently closed"
fileIsClosed :: LVal -> Context LVal
fileIsClosed = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = Boolean pos <$> liftIO (hIsClosed fh)

fileIsReadableDoc = Just "(file/is-readable handle)\n\
                         \Returns #t if the file handle is ready for reading"
fileIsReadable :: LVal -> Context LVal
fileIsReadable = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = Boolean pos <$> liftIO (hIsReadable fh)

fileIsWritableDoc = Just "(file/is-writable handle)\n\
                         \Returns #t if the file handle is ready for writing"
fileIsWritable :: LVal -> Context LVal
fileIsWritable = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = Boolean pos <$> liftIO (hIsWritable fh)

fileIsSeekableDoc = Just "(file/is-seekable handle)\n\
                         \Returns #t if the file handle is ready for seeking"
fileIsSeekable :: LVal -> Context LVal
fileIsSeekable = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = Boolean pos <$> liftIO (hIsSeekable fh)

fileIsEofDoc = Just "(file/is-eof handle)\n\
                    \Returns #t if the file handle is at the end of the file"
fileIsEof :: LVal -> Context LVal
fileIsEof = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = Boolean pos <$> liftIO (hIsEOF fh)

fileSizeDoc = Just "(file/size handle)\n\
                    \Returns the size of associated file in 8-bit bytes."
fileSize :: LVal -> Context LVal
fileSize = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where fn pos fh = Num pos <$> liftIO (hFileSize fh)

fileSetSizeDoc = Just "(file/set-size handle size)\n\
                      \Truncates the physical file with handle to size bytes."
fileSetSize :: LVal -> Context LVal
fileSetSize = checked' (properCC *> params 2 *> (argType 1 tyIOHandle <+> argType 2 tyNum) >^ fn)
    where
      fn pos (fh, sz) = do
        liftIO $ hSetFileSize fh sz
        return $ QExpr pos []

fileCloseDoc = Just "(file/close handle)\n\
                    \Closes the given handle, does nothing if handle is already \
                    \closed. returns nil"
fileClose :: LVal -> Context LVal
fileClose = checked' (properCC *> params 1 *> argType 1 tyIOHandle >^ fn)
    where
      fn pos fh = do
        liftIO (hClose fh)
        return $ QExpr pos []

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
