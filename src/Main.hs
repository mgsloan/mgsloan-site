{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- Copyright 2015 Ruud van Asseldonk
-- Copyright 2018 Michael G Sloan
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.
import Control.Exception (finally)
import Control.Monad
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.List (isInfixOf, isPrefixOf)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Minification (minifyHtml)
import System.Environment
import System.Directory
import System.FilePath
import System.Process (callProcess)
import Shelly hiding ((</>), FilePath)
import Util
import qualified Data.Text as T
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as M
import qualified Image
import qualified Page
import qualified Post as P
import qualified Template
import qualified Mode

-- Copies all files in the source directory to the destination directory.
copyFiles :: FilePath -> FilePath -> IO ()
copyFiles srcDir dstDir = do
  fps <- map (srcDir </>) . omitEmacsFiles <$> listDirectory srcDir
  forM_ fps $ \fp -> copyFile fp (dstDir </> takeFileName fp)

-- Reads and parses all templates in the given directory.
readTemplates :: FilePath -> IO (M.Map FilePath Template.Template)
readTemplates dir = do
  templates <- map (dir </>) . omitEmacsFiles <$> listDirectory dir
  fmap M.fromList $ forM templates $ \fp -> do
    contents <- readFile fp
    return (takeFileName fp, Template.parse contents)

-- Reads and renders all posts in the given directory.
readPosts :: FilePath -> IO [P.Post]
readPosts dir = do
  createDirectoryIfMissing True dir
  posts <- map (dir </>) . omitEmacsFiles <$> listDirectory dir
  fmap concat $ forM posts $ \postDir -> do
    let postName = takeFileName postDir
    if postName `elem` [".git", "license.md", "license.md"]
      then return []
      else do
        let postPath = postDir </> "post.md"
        exists <- doesFileExist postPath
        if exists
          then (:[]) . P.parse postDir postName <$> readFile postPath
          else do
            putStrLn $ "Warning: expected file at " ++ postPath ++ ", but none found."
            return []

-- Reads and renders all non-posts in the given directory.
readPages :: FilePath -> IO [Page.Page]
readPages dir = do
  createDirectoryIfMissing True dir
  pages <- map (dir </>) . omitEmacsFiles <$> listDirectory dir
  fmap concat $ forM pages $ \pageDir -> do
    let pageName = takeFileName pageDir
    if pageName `elem` [".git", "license.md"]
      then return []
      else do
        let pagePath = pageDir </> "page.md"
        pageExists <- doesFileExist pagePath
        if pageExists
          then (:[]) . Page.parse pageDir pageName <$> readFile pagePath
          else do
            putStrLn $ "Warning: expected file at " ++ pagePath ++ ", but none found."
            return []

readPageIndex :: IO String
readPageIndex = Page.body . Page.parse pageDir pageName <$> readFile pagePath
  where
    pageDir = "non-posts/"
    pageName = "non-posts"
    pagePath = "non-posts/index.md"

-- Holds the output directory and input image directory.
data Config = Config
  { outDir   :: FilePath
  , outMode  :: Mode.Mode
  }

copyPostImages :: Config -> P.Post -> IO ()
copyPostImages config post = do
  let destFile = (outDir config) </> (drop 1 $ P.url post) </> "index.html"
      destDir = takeDirectory destFile
      imagesDir = P.sourceDir post </> "images"
      destImagesDir = destDir </> "images"
  createDirectoryIfMissing True imagesDir
  createDirectoryIfMissing True destImagesDir
  copyFiles imagesDir destImagesDir

copyPageImages :: Config -> Page.Page -> IO ()
copyPageImages config page = do
  let destFile = (outDir config) </> (drop 1 $ Page.url page) </> "index.html"
      destDir = takeDirectory destFile
      imagesDir = Page.sourceDir page </> "images"
      destImagesDir = destDir </> "images"
  createDirectoryIfMissing True imagesDir
  createDirectoryIfMissing True destImagesDir
  copyFiles imagesDir destImagesDir

-- Given the post template and the global context, expands the template for all
-- of the posts and writes them to the output directory.
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
          context  = M.unions
            [ P.context post
            , P.relatedContext related
            , ctx]
          html = Template.apply tmpl context
          imagesDir = P.sourceDir post </> "images"
      withImages <- Image.processImages (outMode config) imagesDir (outDir config) html
      let minified = minifyHtml withImages
      writeFile destFile minified
  in do
    subsetCmdsAsync <- mapM writePostAsync withRelated
    mapM_ Async.wait subsetCmdsAsync

writePages :: Template.Template -> Template.Context -> [Page.Page] -> Config -> IO ()
writePages tmpl ctx pages config =
  let
    total = length pages
    writePageAsync (i, page) = do
      putStrLn $ "[" ++ (show i) ++ " of " ++ (show total) ++ "] " ++ (Page.slug page)
      Async.async $ writePage page
    writePage page = do
      let destFile = (outDir config) </> (drop 1 $ Page.url page) </> "index.html"
          context  = M.unions [Page.context page, ctx]
          html = Template.apply tmpl context
          imagesDir = Page.sourceDir page </> "images"
      withImages <- Image.processImages (outMode config) imagesDir (outDir config) html
      let minified = minifyHtml withImages
      writeFile destFile minified
  in do
    subsetCmdsAsync <- mapM writePageAsync (zip ([1..] :: [Int]) pages)
    mapM_ Async.wait subsetCmdsAsync

-- Writes a general (non-post) page given a template and expansion context.
-- Returns the subset commands that need to be executed for that page.
writePlainPage :: String -> Template.Context -> Template.Template -> Config -> IO ()
writePlainPage url pageContext template config = do
  let context  = Template.stringField "url" url <> pageContext
      html     = minifyHtml $ Template.apply template context
      destDir  = (outDir config) </> (tail url)
      destFile = destDir </> "index.html"
  createDirectoryIfMissing True destDir
  writeFile destFile html

writeListings :: Template.Context -> Template.Template -> [P.Post] -> [Page.Page] -> String -> Config -> IO ()
writeListings globalContext template posts pages pageIndex config = do
  let ergonomicsPosts = filter ((Just P.Ergonomics ==) . P.category) posts
  let softwarePosts = filter ((Just P.Software ==) . P.category) posts
  writeListing "/" Nothing globalContext template posts pages pageIndex config
  writeListing "/category-ergonomics/" (Just P.Ergonomics) globalContext template ergonomicsPosts [] pageIndex config
  writeListing "/category-software/" (Just P.Software) globalContext template softwarePosts [] pageIndex config

-- Given the archive template and the global context, writes the listing page
-- to the destination directory.
writeListing :: String -> Maybe P.Category -> Template.Context -> Template.Template -> [P.Post] -> [Page.Page] -> String -> Config -> IO ()
writeListing url category globalContext template posts pages pageIndex =
  writePlainPage url context template
  where
    context = M.unions $
      [ P.archiveContext posts
      , Template.stringField "title"           "mgsloan"
      , Template.stringField "bold-font"       "true"
      , Template.stringField "archive"         "true"
      , Template.boolField "has-pages"         (not (null pages))
      , Template.stringField "page-index-html" pageIndex
      , Template.boolField "is-index"          (category == Nothing)
      , Template.boolField "is-ergonomics"     (category == Just P.Ergonomics)
      , Template.boolField "is-software"       (category == Just P.Software)
      , globalContext
      ]

-- Given the feed template and list of posts, writes an atom feed.
writeFeed :: Template.Template -> [P.Post] -> Config -> IO ()
writeFeed template posts config = do
  let url = "/feed.xml"
      context = P.feedContext posts
      atom = Template.apply template context
      destFile = (outDir config) </> (tail url)
  createDirectoryIfMissing True (outDir config)
  writeFile destFile atom

main :: IO ()
main = do
  chdirRepo False =<< getCurrentDirectory
  args <- getArgs
  case args of
    ["push"] -> pushCmd
    ["render-draft", draftTitlePortion] -> renderDraftCmd draftTitlePortion
    ["render-start"] -> renderStartPage
    ["render-index"] -> renderIndexCmd
    [] -> regenerateCmd
    _ -> error $ "Unrecognized arguments: " ++ show args

chdirRepo :: Bool -> FilePath -> IO ()
chdirRepo dirChanged dir = do
  let dirName = takeFileName dir
  isGitRoot <- elem ".git" <$> listDirectory dir
  if isGitRoot && dirName `notElem` ["draft", "out"]
    then when dirChanged $ do
      putStrLn $ "Changing directory to " ++ show dir
      setCurrentDirectory dir
    else do
      chdirRepo True (takeDirectory dir)

renderDraftCmd :: String -> IO ()
renderDraftCmd draftTitlePortion = do
  templates <- readTemplates "templates/"
  drafts <- readPosts "draft/posts/"
  globalContext <- makeGlobalContext templates
  [draft] <- return $ filter (matchTitle draftTitlePortion) drafts
  copyPostImages draftConfig draft
  writePosts (templates ! "post.html") globalContext [draft] draftConfig

matchTitle :: String -> P.Post -> Bool
matchTitle portion post =
  map toLower portion `isInfixOf` map toLower (P.title post)

renderIndexCmd :: IO ()
renderIndexCmd = do
  templates <- readTemplates "templates/"
  posts <- readPosts "posts/"
  pages <- readPages "non-posts/"
  pageIndex <- readPageIndex
  globalContext <- makeGlobalContext templates
  writeListings globalContext (templates ! "archive.html") posts pages pageIndex baseConfig

regenerateCmd :: IO ()
regenerateCmd = do
  templates <- readTemplates "templates/"
  posts <- readPosts "posts/"
  pages <- readPages "non-posts/"
  pageIndex <- readPageIndex
  globalContext <- makeGlobalContext templates

  -- cleanOutputDir

  createDirectoryIfMissing True "out/images"
  createDirectoryIfMissing True "draft/out/images"
  copyFile "assets/haskell/haskell-placeholder-banner.jpg" "out/images/haskell-placeholder-banner.jpg"
  copyFile "assets/haskell/haskell-placeholder-banner.jpg" "draft/out/images/haskell-placeholder-banner.jpg"

  drafts <- (++) <$> readPosts "draft/posts/" <*> readPosts "draft/posts-old/"
  unless (null drafts) $ do
    putStrLn "Writing draft posts..."
    forM_ drafts (copyPostImages draftConfig)
    writePosts (templates ! "post.html") globalContext drafts draftConfig
    putStrLn "Writing draft index..."
    writeListings globalContext (templates ! "archive.html") drafts [] "" draftConfig

  putStrLn "Writing posts..."
  forM_ posts (copyPostImages baseConfig)
  writePosts (templates ! "post.html") globalContext posts baseConfig

  unless (null pages) $ do
    putStrLn "Writing non-posts..."
    forM_ pages (copyPageImages baseConfig)
    writePages (templates ! "page.html") globalContext pages baseConfig

  putStrLn "Copying old blog..."
  createDirectoryIfMissing True "out/wordpress"
  copyFiles "assets/old-blog/" "out/wordpress"

  -- Moved entries from ergonomics log into normal posts
  let filteredPages = filter (("Ergonomics Log" /=) . Page.title) pages

  putStrLn "Writing other pages..."
  writeListings globalContext (templates ! "archive.html") posts filteredPages pageIndex baseConfig

  copyFile "assets/favicon.png" "out/favicon.png"
  copyFile "assets/favicon.png" "draft/out/favicon.png"
  copyFile "assets/CNAME"       "out/CNAME"
  copyFile "assets/keybase.txt" "out/keybase.txt"
  copyFile "assets/dark-mode-toggle/src/dark-mode-toggle.mjs" "out/dark-mode-toggle.mjs"
  copyFile "assets/dark-mode-toggle/src/dark-mode-toggle.mjs" "draft/out/dark-mode-toggle.mjs"
  copyFile "assets/redirect-index.html" "out/posts/index.html"
  copyFile "assets/theme.js" "out/theme.js"
  copyFile "assets/theme.js" "draft/out/theme.js"

  putStrLn "Writing atom feed..."
  writeFeed (templates ! "feed.xml") posts baseConfig

  putStrLn "Using rsync to copy published posts into drafts"
  shelly $ run_ "rsync" ["-a", "out/posts", "draft/out/posts"]

-- Push to both repos.
pushCmd :: IO ()
pushCmd = shelly $ do
  liftIO cleanOutputDir
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
  liftIO renderStartPage
  -- Add all untracked and
  shouldPush <- chdir "out" $ do
    topLevel <- head . T.lines <$> run "git" ["rev-parse", "--show-toplevel"]
    curDir <- pwd
    when (fromText topLevel /= curDir) $ fail $ concat
      [ "Expected out dir git repo to be at "
      , show curDir
      , ", but instead it was at "
      , show topLevel
      ]
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
          run_ "git" ["commit", "-m", "Update to mgsloan/mgsloan-site@" <> shortSha]
          run_ "git" ["push"]
        else do
          echo "out/ repo is clean, so not committing or pushing it."

makeGlobalContext :: M.Map String Template.Template -> IO (M.Map String Template.ContextValue)
makeGlobalContext templates = do
  -- Create a context with the field "year" set to the current year, and create
  -- a context that contains all of the templates, to handle includes.
  (year, _month, _day) <- fmap (toGregorian . utctDay) getCurrentTime
  let yearString = show year
  return $ M.unions
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

cleanOutputDir :: IO ()
cleanOutputDir = do
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

baseConfig :: Config
baseConfig = Config { outDir   = "out/"
                    , outMode  = Mode.Published
                    }

draftConfig :: Config
draftConfig = baseConfig { outDir = "draft/out"
                         , outMode = Mode.Draft
                         }

renderStartPage :: IO ()
renderStartPage = do
  let remindersMdFile = "/home/mgsloan/docs/reminders.md"
      remindersHtmlFile = "draft/reminders.html"
  callProcess "pandoc" [remindersMdFile, "-o", remindersHtmlFile]
  secretsHtml <- readFile "draft/start-page-secret.html"
  remindersHtml <- readFile remindersHtmlFile
  password <- head . lines <$> readFile "draft/start-page-password"
  let concatenatedHtmlFile = "draft/start-page-concatenated.html"
  writeFile concatenatedHtmlFile $ unlines
    [ "<div id=contents>"
    , secretsHtml
    , "<div id=priorities>"
    , remindersHtml
    , "</div>"
    , "</div>"
    ]
  -- staticrypt built from source of
  -- https://github.com/robinmoisson/staticrypt/tree/38a3f5b297b56c580a65cb2cadeb0007be88fe49
  createDirectoryIfMissing False "out/misc"
  callProcess "staticrypt" [concatenatedHtmlFile, password, "-f", "templates/ubwi", "-o", "out/misc/ubwi"]
    `finally` do
      removeFile remindersHtmlFile
      removeFile concatenatedHtmlFile

{-
findFileWithSuffixIn :: String -> FilePath -> IO FilePath
findFileWithSuffixIn suffix weeklyDir = do
  entries <- listDirectory weeklyDir
  let foundFileName = maximum $ filter ((suffix `isSuffixOf`) . takeFileName) entries
  return $ weeklyDir </> foundFileName
-}

omitEmacsFiles :: [FilePath] -> [FilePath]
omitEmacsFiles = filter (not . (".#" `isPrefixOf`) . takeFileName)
