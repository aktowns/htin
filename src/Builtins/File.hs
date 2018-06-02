{-# LANGUAGE QuasiQuotes #-}
module Builtins.File(FileBuiltin(..)) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (traverse_)
import           Data.List              (find)
import           Data.Maybe             (fromJust, isJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.IO

import           Builtins.Builtin
import           Builtins.Guards
import           Environment
import           Types
import           Utils.Doc

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

-- tyMode :: Type IOMode
-- tyMode = Type "IOMode" (\n -> fromJust $ lookupMode $ unsafeToInt n) (\n -> isNum n && isJust (lookupMode $ unsafeToInt n))

tySeek :: Type SeekMode
tySeek = Type "SeekMode" (\n -> fromJust $ lookupSeek $ unsafeToInt n) (\n -> isNum n && isJust (lookupSeek $ unsafeToInt n))

fileMode :: Context ()
fileMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num builtinPos v) modeMap

fileSeekMode :: Context ()
fileSeekMode = traverse_ (\(v,k,_) -> addSymbolParent k $ Num builtinPos v) seekMap

[genDoc|fileOpen
(file/open path mode)

Opens the given path in mode returning a handle
|]
fileOpen :: LVal -> Context LVal
fileOpen = checked' $ (properCC *> params 2 *> (argType 1 tyStr <+> argType 2 tyNum)) >^
    \p (path, mode) -> do
        let hmode = fromJust $ lookupMode mode
        handle <- liftIO $ openFile (T.unpack path) hmode
        return $ Hnd p $ IOHandle handle

[genDoc|fileRead
(file/read handle)

Reads all contents of the given handle returning a string
|]
fileRead :: LVal -> Context LVal
fileRead = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> (Str p) <$> liftIO (T.hGetContents fh)

[genDoc|fileReadChar
(file/read-char handle)

Reads a character from the handle at its current position
|]
fileReadChar :: LVal -> Context LVal
fileReadChar = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> do
      char <- liftIO $ hGetChar fh
      return $ Str p (T.singleton char)

[genDoc|fileReadLine
(file/read-line handle)

Reads a line from the handle at its current position
|]
fileReadLine :: LVal -> Context LVal
fileReadLine = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> (Str p) <$> liftIO (T.hGetLine fh)

[genDoc|fileWrite
(file/write handle data)

Writes the given data to the file handle
|]
fileWrite :: LVal -> Context LVal
fileWrite = checked' $ (properCC *> params 2 *> (argType 1 tyIOHandle <+> argType 2 tyStr)) >^
    \p (fh, contents) -> do
        liftIO $ T.hPutStr fh contents
        return $ nil p

[genDoc|fileFlush
(file/flush handle)

Causes any items buffered for output in handle to be sent immediately to the operating system.
|]
fileFlush :: LVal -> Context LVal
fileFlush = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> do
        liftIO $ hFlush fh
        return $ nil p

[genDoc|fileTell
(file/tell handle)

Gives the current position in the file
|]
fileTell :: LVal -> Context LVal
fileTell = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> (Num p) <$> liftIO (hTell fh)

[genDoc|fileSeek
(file/seek handle mode position)

Sets the position of handle depending on mode. The offset is given in terms of 8-bit bytes.
|]
fileSeek :: LVal -> Context LVal
fileSeek = checked' $ (properCC *> params 3 *> (argType 1 tyIOHandle <+> argType 2 tySeek <+> argType 3 tyNum)) >^
    \p ((fh, mode), posi) -> do
        liftIO $ hSeek fh mode posi
        return $ nil p

[genDoc|fileIsOpen
(file/is-open handle)

Returns #t if the file handle is currently open
|]
fileIsOpen :: LVal -> Context LVal
fileIsOpen = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> Boolean p <$> liftIO (hIsOpen fh)

[genDoc|fileIsClosed
(file/is-closed handle)

Returns #t if the file handle is currently closed
|]
fileIsClosed :: LVal -> Context LVal
fileIsClosed = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> Boolean p <$> liftIO (hIsClosed fh)

[genDoc|fileIsReadable
(file/is-readable handle)

Returns #t if the file handle is ready for reading
|]
fileIsReadable :: LVal -> Context LVal
fileIsReadable = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> Boolean p <$> liftIO (hIsReadable fh)

[genDoc|fileIsWritable
(file/is-writable handle)

Returns #t if the file handle is ready for writing
|]
fileIsWritable :: LVal -> Context LVal
fileIsWritable = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> Boolean p <$> liftIO (hIsWritable fh)

[genDoc|fileIsSeekable
(file/is-seekable handle)

Returns #t if the file handle is ready for seeking
|]
fileIsSeekable :: LVal -> Context LVal
fileIsSeekable = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> Boolean p <$> liftIO (hIsSeekable fh)

[genDoc|fileIsEof
(file/is-eof handle)

Returns #t if the file handle is at the end of the file
|]
fileIsEof :: LVal -> Context LVal
fileIsEof = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> Boolean p <$> liftIO (hIsEOF fh)

[genDoc|fileSize
(file/size handle)

Returns the size of associated file in 8-bit bytes.
|]
fileSize :: LVal -> Context LVal
fileSize = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> Num p <$> liftIO (hFileSize fh)

[genDoc|fileSetSize
(file/set-size handle size)

Truncates the physical file with handle to size bytes.
|]
fileSetSize :: LVal -> Context LVal
fileSetSize = checked' $ (properCC *> params 2 *> (argType 1 tyIOHandle <+> argType 2 tyNum)) >^
    \p (fh, sz) -> do
        liftIO $ hSetFileSize fh sz
        return $ nil p

[genDoc|fileClose
(file/close handle)

Closes the given handle, does nothing if handle is already closed. returns nil
|]
fileClose :: LVal -> Context LVal
fileClose = checked' $ (properCC *> params 1 *> argType 1 tyIOHandle) >^
    \p fh -> do
        liftIO (hClose fh)
        return $ nil p

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
