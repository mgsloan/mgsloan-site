{-# LANGUAGE ViewPatterns #-}

-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Post ( Post
            , sourceDir
            , archiveContext
            , body
            , context
            , date
            , feedContext
            , longDate
            , parse
            , relatedContext
            , shortDate
            , selectRelated
            , slug
            , title
            , url
            , year ) where

import qualified Data.Map as M
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import           Data.Time.Format
import           Data.Time.Calendar (Day, showGregorian, toGregorian)
import           GHC.Exts (groupWith, sortWith)
import           Text.Pandoc
import           Text.Pandoc.Highlighting (zenburn)

import qualified Html
import qualified Template
import qualified Type

-- Front matter consists of key value pairs, both of type string.
-- There is no fancy YAML here.
type FrontMatter = M.Map String String

-- Strips off and parses front matter from the string. Front matter is
-- delimited by triple dashes. Keys are anything before ": ", the value
-- is what comes after that. Ignores first line assuming it is "---".
extractFrontMatter :: String -> (FrontMatter, String)
extractFrontMatter = parseFM M.empty . drop 1 . lines
  where parseFM _ []                = error "Post should not be empty."
        parseFM fm ("---":postBody) = (fm, unlines postBody)
        parseFM fm (line:more)      = parseFM (M.insert key value fm) more
          where (key, delimValue) = break (== ':') line
                value = drop 2 delimValue -- Drop the colon and space.

data Post = Post { sourceDir :: FilePath
                 , title     :: String
                 , header    :: String
                 , subheader :: Maybe String
                 , part      :: Maybe Int
                 , date      :: Day
                 , slug      :: String
                 , synopsis  :: String
                 , primaryImage :: Maybe String
                 , body      :: String } deriving (Show) -- TODO: This is for debugging only, remove.

-- Returns the post date, formatted like "17 April, 2015".
longDate :: Post -> String
longDate = formatTime defaultTimeLocale "%e %B, %Y" . date

-- Returns the post date, formatted like "2015-04-17".
shortDate :: Post -> String
shortDate = showGregorian . date

-- Returns the year in which the post was published.
year :: Post -> Integer
year post = y where (y, _m, _d) = toGregorian $ date post

-- Returns the canonical absolute url for a particular post.
url :: Post -> String
url post = "/posts/" ++ slug post
{- Ruud included the date, whereas I'd prefer just the post
url post = "/" ++ datePath ++ "/" ++ (slug post)
  where datePath = formatTime defaultTimeLocale "%Y/%m/%d" $ date post
-}

-- Returns whether post has code in it that requires a monospace font.
usesMonoFont :: Post -> Bool
usesMonoFont = not . null . Html.filterTags Html.isCode . Html.parseTags . body

-- Returns whether the post has <em> tags that require an italic font.
usesItalicFont :: Post -> Bool
usesItalicFont = not . null . Html.filterTags Html.isEm . Html.parseTags . body

-- Converts an integer to a Roman numeral (nothing fancy, works for 1-9).
toRoman :: Int -> String
toRoman i = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] !! (i - 1)

-- Maps a boolean to a field that can be used in a template expansion context.
booleanField :: Bool -> Maybe String
booleanField b = if b then Just "true" else Nothing

-- Given a string, and a substring consisting of two words, inserts a <br> tag
-- and a space between the two words in the original string.
addBreak :: String -> String -> String
addBreak between@(words -> [brAfter, brBefore]) =
  Text.unpack . Text.replace textBetween broken . Text.pack
  where textBetween         = Text.pack between
        broken              = Text.pack $ brAfter ++ "<br> " ++ brBefore
addBreak _ = id

-- Returns the template expansion context for the post.
context :: Post -> Template.Context
context p = fmap Template.StringValue ctx
  where ctx       = M.union fields (M.mapMaybe id optFields)
        fields    = M.fromList [ ("title", title p)
                               , ("title-html", Type.makeAbbrs $ title p)
                               , ("header", Type.makeAbbrs $ header p)
                               , ("short-date", shortDate p)
                               , ("long-date", longDate p)
                               , ("url", url p)
                               , ("synopsis", synopsis p)
                               , ("synopsis-html", Type.makeAbbrs $ synopsis p)
                               , ("content", body p) ]
        optFields = M.fromList [ ("subheader", subheader p)
                               , ("part", fmap toRoman $ part p)
                               , ("bold-font", boldFontField)
                               , ("italic-font", italicFontField)
                               , ("math", mathField)
                               , ("img", imgField)
                               , ("mono-font", monoFontField)
                               , ("serif-italic-font", serifItalicFontField)
                               -- TODO: would be nice if this supported relative paths
                               , ("primary-image", primaryImage p)]
        usesSerifItalic      = (Type.usesSerifItalicFont $ body p) || (isJust $ subheader p)
        boldFontField        = booleanField $ Type.usesBoldFont $ body p
        italicFontField      = booleanField $ usesItalicFont p
        mathField            = booleanField $ Html.hasMath $ body p
        imgField             = booleanField $ Html.hasImg $ body p
        monoFontField        = booleanField $ usesMonoFont p
        serifItalicFontField = booleanField $ usesSerifItalic

-- Given a slug and the contents of the post file (markdown with front matter),
-- renders the body to html and parses the metadata.
parse :: FilePath -> String -> String -> Post
parse dir postSlug contents = let
  (frontMatter, bodyContents) = extractFrontMatter contents
  postTitle     = frontMatter M.! "title"
  postHeading   = fromMaybe postTitle $ M.lookup "header" frontMatter
  breakAt       = M.lookup "break" frontMatter
  brokenHeading = foldr addBreak postHeading breakAt
  runIn         = M.lookup "run-in" frontMatter
  addRunIn html = foldl Html.makeRunIn html (fmap length runIn)
  refineType    = addRunIn . Type.expandPunctuation . Html.renderTags . Type.makeAbbrsTags
  parseDate     = parseTimeOrError True defaultTimeLocale "%F"
  in Post { sourceDir   = dir
          , title       = postTitle
          , header      = brokenHeading
          , subheader   = M.lookup "subheader" frontMatter
          , part        = fmap read $ M.lookup "part" frontMatter
          , date        = parseDate $ frontMatter M.! "date"
          , slug        = postSlug
          , synopsis    = frontMatter M.! "synopsis"
          , primaryImage = M.lookup "primary-image" frontMatter
          , body        = refineType
                        $ Html.modifyLinks
                        $ Html.cleanTables
                        $ Html.addAnchors
                        $ Html.parseTags
                        $ renderMarkdown bodyContents
          }

-- Renders markdown to html using Pandoc with my settings.
renderMarkdown :: String -> String
renderMarkdown md =
  case runPure render of
    Right result -> result
    Left  err    -> "Failed to parse markdown: " ++ show err
  -- Enable backtick code blocks and accept tables.
  -- For output, enable syntax highlighting.
  where ropt = def { readerExtensions = extensionsFromList [Ext_footnotes] <>
                                        githubMarkdownExtensions }
        wopt = def { writerHighlightStyle = Just zenburn }
        render = do
          pandoc <- readMarkdown ropt (Text.pack md)
          Text.unpack <$> writeHtml5String wopt pandoc

-- Related content for a post, for the further reading section in the footer.
data RelatedContent
  = Further Post
  | Series [Post]

-- Returns the template expansion context for related content.
relatedContext :: RelatedContent -> Template.Context
relatedContext related = case related of
  Further post -> Template.nestContext "further" $ context post
  Series posts -> Template.listField   "series" $ fmap context posts

-- Takes an (unordered) list of posts and produces a list of posts together with
-- related content for that post.
selectRelated :: [Post] -> [(Post, RelatedContent)]
selectRelated posts = fmap nextElsePrev prevPostNext
  where -- Create chronological triples of (previous post, post, next post).
        chronological = sortWith date posts
        prevPosts     = Nothing : (fmap Just chronological)
        nextPosts     = (drop 1 $ fmap Just chronological) ++ [Nothing]
        prevPostNext  = zip3 prevPosts chronological nextPosts

        -- Select the next post as "Further" content if there is one, otherwise
        -- take the previous post (which is assumed to exist in that case).
        nextElsePrev x = case x of
          (_, post, Just next) -> (post, Further next)
          (Just prev, post, _) -> (post, Further prev)
          (_, post, _) -> (post, Further post)

-- Returns a context for a group of posts that share the same year.
archiveYearContext :: [Post] -> Template.Context
archiveYearContext posts = yearField `M.union` postsField
  where yearField     = Template.stringField "year" $ show $ year $ head $ posts
        chronological = sortWith date posts
        recentFirst   = reverse chronological
        postsField    = Template.listField "post" $ fmap context recentFirst

-- Returns a contexts with a "archive-year" list where every year has a "posts"
-- lists.
archiveContext :: [Post] -> Template.Context
archiveContext posts  = Template.listField "archive-year" years
  where yearGroups    = groupWith year posts
        chronological = sortWith (year . head) yearGroups
        recentFirst   = reverse chronological
        years         = fmap archiveYearContext recentFirst

-- Context for generating an atom feed for the 15 most recent posts.
feedContext :: [Post] -> Template.Context
feedContext posts = updatedField `M.union` postsField
  where chronological = sortWith date posts
        recentFirst   = take 15 $ reverse chronological
        updatedField  = Template.stringField "updated" $ shortDate $ head recentFirst
        postsField    = Template.listField "post" $ fmap context recentFirst
