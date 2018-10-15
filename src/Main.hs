{-# LANGUAGE OverloadedStrings #-}

-- Copyright 2015 Ruud van Asseldonk
-- Copyright 2018 Michael G Sloan
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.
import Control.Monad
import Data.Monoid ((<>))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Minification (minifyHtml)
import System.Environment
import System.Directory
import System.FilePath
import Shelly hiding ((</>), FilePath)
import qualified Data.Text as T
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as M
import qualified Image
import qualified Post as P
import qualified Template
import qualified Mode

-- Applies the IO-performing function f to every file in a given directory if
-- the filename satisfies the predicate p.
mapFilesIf :: (FilePath -> Bool) -> (FilePath -> IO a) -> FilePath -> IO [a]
mapFilesIf p f dir = do
  dirExists <- doesDirectoryExist dir
  if dirExists
    then enumerateFiles >>= filterM doesFileExist >>= mapM f
    else return []
  -- Prepend the directory names to the names returned by getDirectoryContents.
  where enumerateFiles = fmap (filter p . fmap (dir </>)) $ getDirectoryContents dir

-- Applies the IO-performing function f to every file in a given directory.
mapFiles :: (FilePath -> IO a) -> FilePath -> IO [a]
mapFiles = mapFilesIf $ \_ -> True

-- Copies all files in the source directory to the destination directory.
copyFiles :: FilePath -> FilePath -> IO ()
copyFiles srcDir dstDir = void $ mapFiles copy srcDir
  where copy fname = copyFile fname $ dstDir </> (takeFileName fname)

-- Applies the IO-performing function f to every file in a given directory, and
-- returns a map from the file name to the result.
mapFilesFileName :: (FilePath -> IO a) -> FilePath -> IO (M.Map FilePath a)
mapFilesFileName f = (fmap M.fromList) . (mapFiles makePair)
  where makePair fname = fmap (\x -> (takeFileName fname, x)) (f fname)

-- Reads and parses all templates in the given directory.
readTemplates :: FilePath -> IO (M.Map FilePath Template.Template)
readTemplates = mapFilesFileName $ (fmap Template.parse) . readFile

-- Reads a post from a file.
readPost :: FilePath -> IO P.Post
readPost fname = fmap makePost $ readFile fname
  where makePost body = P.parse (takeBaseName fname) body

-- Reads and renders all posts in the given directory.
readPosts :: FilePath -> IO [P.Post]
readPosts = mapFilesIf (\fp -> isMarkdown fp && isn'tLicense fp) readPost
  where
    isMarkdown = (== ".md") . takeExtension
    isn'tLicense = (/= "license.md") . takeFileName

-- Holds the output directory and input image directory.
data Config = Config { outDir   :: FilePath
                     , imageDir :: FilePath
                     , outMode  :: Mode.Mode }

-- Compresses the given file to a new file with .gz/br appended to the filename.
compressFile :: FilePath -> IO ()
compressFile _fname = do
  {-
  System.Process.callProcess "zopfli" [fname]
  System.Process.callProcess "brotli" ["--force", "--output",  fname ++ ".br", "--input", fname]
  -}
  return ()

-- Given the post template and the global context, expands the template for all
-- of the posts and writes them to the output directory. This also prints a list
-- of processed posts to the standard output. Start numbering post artifacts at
-- 53, lower indices are reserved for other pages.
writePosts :: Template.Template -> Template.Context -> [P.Post] -> Config -> IO ()
writePosts tmpl ctx posts config =
  let
    total = length posts
    -- Reverse the list of posts, so the most recent one is rendered first.
    -- This makes the preview workflow faster, because the most recent post
    -- in the list is likely the one that I want to view.
    withRelated = zip [1 :: Int ..] $ reverse $ P.selectRelated posts
    writePostAsync (i, (post, related)) = do
      putStrLn $ "[" ++ (show i) ++ " of " ++ (show total) ++ "] " ++ (P.slug post)
      Async.async $ writePost post related
    writePost post related = do
      let destFile = (outDir config) </> (drop 1 $ P.url post) </> "index.html"
          context  = M.unions [ P.context post
                              , P.relatedContext related
                              , ctx]
          html = Template.apply tmpl context
      -- Ignores referenced images - ruuda's blog uses this for font
      -- subsetting based on SVG contents, but I don't need that
      -- feature.
      (_imgPaths, withImages) <- Image.processImages (outMode config) (imageDir config) html
      let minified = minifyHtml withImages
      createDirectoryIfMissing True $ takeDirectory destFile
      writeFile destFile minified
      compressFile destFile
  in do
    subsetCmdsAsync <- mapM writePostAsync withRelated
    mapM_ Async.wait subsetCmdsAsync

-- Writes a general (non-post) page given a template and expansion context.
-- Returns the subset commands that need to be executed for that page.
writePage :: String -> Template.Context -> Template.Template -> Config -> IO ()
writePage url pageContext template config = do
  let context  = Template.stringField "url" url <> pageContext
      html     = minifyHtml $ Template.apply template context
      destDir  = (outDir config) </> (tail url)
      destFile = destDir </> "index.html"
  createDirectoryIfMissing True destDir
  writeFile destFile html
  compressFile destFile

{-
writeIndex :: Template.Context -> Template.Template -> Config -> IO ()
writeIndex globalContext = writePage "/" context
  where context = M.unions [ Template.stringField "title"     "mgsloan's blog"
                           , Template.stringField "bold-font" "true"
                           , Template.stringField "light"     "true"
                           , globalContext ]
-}

-- Given the archive template and the global context, writes the archive page
-- to the destination directory.
writeArchive :: Template.Context -> Template.Template -> [P.Post] -> Config -> IO ()
writeArchive globalContext template posts = writePage "/" context template
  where context = M.unions [ P.archiveContext posts
                           , Template.stringField "title"     "mgsloan"
                           , Template.stringField "bold-font" "true"
                           , Template.stringField "archive"   "true"
                           , globalContext ]

{-
-- Given the contact template and the global context, writes the contact page
-- to the destination directory.
writeContact :: Template.Context -> Template.Template -> Config -> IO ()
writeContact globalContext = writePage "/contact" context
  where context = M.unions [ Template.stringField "title" "Contact mgsloan"
                           , Template.stringField "light" "true"
                           , globalContext ]
-}

-- Given the feed template and list of posts, writes an atom feed.
writeFeed :: Template.Template -> [P.Post] -> Config -> IO ()
writeFeed template posts config = do
  let url      = "/feed.xml"
      context  = P.feedContext posts
      atom     = Template.apply template context
      destFile = (outDir config) </> (tail url)
  createDirectoryIfMissing True (outDir config)
  writeFile destFile atom
  compressFile destFile

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["push"] -> pushCmd
    [] -> regenerateCmd
    _ -> error $ "Unrecognized arguments: " ++ show args

regenerateCmd :: IO ()
regenerateCmd = do
  templates <- readTemplates "templates/"
  posts     <- readPosts     "posts/"

  -- Create a context with the field "year" set to the current year, and create
  -- a context that contains all of the templates, to handle includes.
  (year, _month, _day) <- fmap (toGregorian . utctDay) getCurrentTime
  let yearString = show year
      globalContext = M.unions
        [ Template.stringField "year" yearString
        , Template.stringField "year-range" $
          if yearString == "2018"
            then "2018"
            else "2018-" ++ yearString
        , Template.stringField "body-font" "'Alegreya Sans'"
        , Template.stringField "header-font" "'Playfair Display'"
        , Template.stringField "serif-font" "Alegreya"
        , fmap Template.TemplateValue templates
        ]
      config        = Config { outDir   = "out/"
                             , imageDir = "images/compressed/"
                             , outMode  = Mode.Published
                             }
      draftConfig = config { outDir = "out-drafts/"
                           , outMode = Mode.Draft
                           }

  {-
  outExists <- doesDirectoryExist "out"
  when outExists $ do
    files <- listDirectory "out"
    print files
    forM_ files $ \file -> do
      let fp = "out" </> file
      if file == ".git"
        then return ()
        else do
          isFile <- doesFileExist fp
          print (fp, isFile)
          if isFile
            then removeFile fp
            else removeDirectoryRecursive fp
  -}

  drafts    <- readPosts     "drafts/"
  if length drafts < 2
    then putStrLn "To use drafts mechanism, need more than 2 drafts"
    else do
      -- TODO Have separate draft images.  Separate images per post?
      putStrLn "Copying draft images..."
      createDirectoryIfMissing True  "out-drafts/images/"
      copyFiles "images/compressed/" "out-drafts/images/"

      putStrLn "Writing draft posts..."
      writePosts (templates M.! "post.html") globalContext drafts draftConfig

      putStrLn "Writing draft index..."
      writeArchive globalContext (templates M.! "archive.html") drafts draftConfig

  putStrLn "Copying images..."
  createDirectoryIfMissing True  "out/images/"
  copyFiles "images/compressed/" "out/images/"

  putStrLn "Writing posts..."
  writePosts (templates M.! "post.html") globalContext posts config

  putStrLn "Copying old blog..."
  createDirectoryIfMissing True "out/wordpress"
  copyFiles "old-blog/" "out/wordpress"

  putStrLn "Writing other pages..."
  {-
  writeIndex   globalContext (templates M.! "index.html")   config
  writeContact globalContext (templates M.! "contact.html") config
  -}
  writeArchive globalContext (templates M.! "archive.html") posts config

  copyFile "assets/favicon.png"          "out/favicon.png"
  copyFile "assets/CNAME"                "out/CNAME"
  copyFile "assets/keybase.txt"          "out/keybase.txt"

  putStrLn "Writing atom feed..."
  writeFeed (templates M.! "feed.xml") posts config

-- Push to both repos.
pushCmd :: IO ()
pushCmd = shelly $ do
  -- Check if the repo is clean.
  -- https://stackoverflow.com/a/3879077
  let checkIsDirty = do
        errExit False $ run_ "git" ["diff-index", "--quiet", "HEAD", "--"]
        code <- lastExitCode
        return (code /= 0)
  isDirty <- checkIsDirty
  when isDirty $
    fail "Site repository appears to be dirty, so cannot push."
  let checkMaster repo = chdir repo $ do
        output <- run "git" ["rev-parse", "--abbrev-ref", "HEAD"]
        when (output /= "master\n") $
          fail $ show repo ++ " needs to be on master branch to push."
  -- Check if both repos are on master.
  checkMaster "."
  checkMaster "out"
  -- Rebuild the site.
  liftIO regenerateCmd
  -- Add all untracked and
  shouldPush <- chdir "out" $ do
    run_ "git" ["add", "-A"]
    run_ "git" ["status"]
    echo_n "Does this status for the output repo look good? "
    response <- liftIO getLine
    case response :: String of
      "y" -> return True
      _ -> do
        echo "Response was not 'y', so not pushing"
        return False
  when shouldPush $ do
    run_ "git" ["push"]
    shortSha <- T.take 7 <$> run "git" ["rev-parse", "HEAD"]
    chdir "out" $ do
      outputDirty <- checkIsDirty
      if outputDirty
        then do
          run_ "git" ["commit", "-m", "Update to " <> shortSha]
          run_ "git" ["push"]
        else do
          echo "out/ repo is clean, so not committing or pushing it."
