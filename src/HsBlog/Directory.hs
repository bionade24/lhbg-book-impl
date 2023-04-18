module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  , confirm
  )
  where

import qualified HsBlog.Markup as Markup
import HsBlog.Markup (parse)
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , (</>)
  , (<.>)
  , takeFileName, takeBaseName
  )

import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )

-- | Copy files from one directory to another, converting '.txt' files to
--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

-- | The relevant directory content for our application
data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
      -- ^ File paths and their content
    , dcFilesToCopy :: [FilePath]
      -- ^ Other file paths, to be copied directly
    }

------------------------
-- * Build index page
buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Heading 1 heading : article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                <> foldMap convertStructure (take 3 article)
                <> Html.p_ (Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      "Blog"
      ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )


-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    {dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException a) -> do
          pure $ Left (displayException a)
        )
    pure (input, maybeResult)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

-- Filter errors out
--applySafeIoOnList :: (a -> IO b) -> [a] -> IO [(a, b)]
--applySafeIoOnList action inputs =
--  applyIoOnList action inputs >>= filterAndReportFailures

-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create

-- | Convert text files to Markup, build an index, and render as html.
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
    index = ("index.html", buildIndex txtOutputFiles)
  in
    map (\(file, html) -> (file, Html.render html)) (index : map convertFile txtOutputFiles)


toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, markup) =
  (file, convert file markup)

-- | Copy files to a directory, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

-----------------------------------------------------
-- * Utilities

-- | Confirm user action
confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid Response. use y or n"
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
