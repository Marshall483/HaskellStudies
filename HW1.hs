module DirUtil where

import System.Environment
import System.Directory
import Data.ByteString (count, pack, length)
import Control.Monad.Reader
import System.Directory.Internal.Prelude (for)
import System.FilePath (joinPath)
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class
import Control.Monad.State

getPaths :: (MonadReader (FilePath, Maybe Int) m, MonadIO m) => m [FilePath]
getPaths = do
    args <- ask
    case args of
        (path, depth) -> getNestedDirs path depth

parseArgs :: [String] -> (FilePath, Maybe Int)
parseArgs argList = case argList of
    [arg1, arg2] -> (arg1, Just (read arg2))
    [arg1]       -> (arg1, Nothing)
    _            -> error "Error at parseArgs!!11!!!"

getFilesCount :: (MonadIO m) => [FilePath] -> m [Int]
getFilesCount paths = forM paths $ \path -> liftIO $ fmap Prelude.length (listDirectory path)

getDirectoryFileSizes :: (MonadIO m) => [FilePath] -> m [Integer]
getDirectoryFileSizes paths = forM paths (\path -> liftIO $ sum <$> (listDirectory path >>= mapM (getDirectoryContentSize . (\dir ->joinPath [path, dir]))))

getDirectoryInfo :: (MonadState [FilePath] m, MonadIO m) => m [(FilePath, Int, Integer)]
getDirectoryInfo = do
    paths <- get
    fileCounts <- getFilesCount paths
    fileSizes <- getDirectoryFileSizes paths
    return $ zip3 paths fileCounts fileSizes

getNestedDirs :: (MonadIO m) => FilePath -> Maybe Int -> m [FilePath]
getNestedDirs path depth = do 
    isDir <- liftIO $ doesDirectoryExist path
    if isDir 
        then do
            nestedPaths <- fmap (map (\dir ->joinPath [path, dir])) (liftIO $ listDirectory path)
            let getNestedFilesForPaths d = forM nestedPaths (`getNestedDirs` d) in
                case depth of
                    Nothing -> fmap (path:) $ concat <$> getNestedFilesForPaths Nothing
                    Just 0  -> pure [path]
                    Just n  -> fmap (path:) $ concat <$> getNestedFilesForPaths (Just (n - 1))
        else pure []

getDirectoryContentSize :: FilePath -> IO Integer
getDirectoryContentSize path = do 
    isDir <- doesDirectoryExist path
    if isDir
        then do
            paths <- fmap (map (\dir ->joinPath [path, dir])) (liftIO $ listDirectory path)
            sum <$> for paths getDirectoryContentSize
        else getFileSize path

showDirs :: (MonadIO m) => [(FilePath, Int, Integer)] -> m()
showDirs infos = do
    liftIO $ putStrLn "Counts: "
    forM_ infos (\(path, fileCount, _) -> liftIO $ putStrLn (path ++ " " ++ show fileCount))
    liftIO $ putStrLn "Sizes: "
    forM_ infos (\(path, _, fileSizes) -> liftIO $ putStrLn (path ++ " " ++ show fileSizes))
    

main :: IO ()
main = do
    args <- getArgs
    files <- runReaderT getPaths (parseArgs args)
    filesInfo <- evalStateT getDirectoryInfo files
    showDirs
 filesInfo