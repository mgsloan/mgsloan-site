-- Copyright 2015 Ruud van Asseldonk
-- Copyright 2018 Michael Sloan
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Html ( Tag
            , TagProperties
            , addAnchors
            , applyTagsWhere
            , classifyTags
            , cleanTables
            , concatMapTagsWhere
            , filterTags
            , getTextInTag
            , hasH2
            , hasImg
            , hasMath
            , hasUl
            , isA
            , isAbbr
            , isArchive
            , isArchiveLink
            , isArticle
            , isBlockQuote
            , isCode
            , isEm
            , isH1
            , isH2
            , isH3
            , isHead
            , isHeader
            , isHeading
            , isOl
            , isPre
            , isRunIn
            , isScript
            , isSmcp
            , isStrong
            , isStyle
            , isSub
            , isSubtitle
            , isSup
            , isTable
            , isTeaser
            , isTeaserLink
            , isTh
            , isUl
            , isVar
            , makeRunIn
            , mapTagsWhere
            , mapText
            , mapTextWith
            , maxOlLength
            , modifyLinks
            , parseTags
            , renderTags
            ) where

-- This module contains utility functions for dealing with html.

import           Control.Monad (join, msum)
import           Data.List (delete, find, intersperse, lookup)
import           Data.Maybe (catMaybes)
import           Network.URI (parseURIReference, uriToString, URI(..), URIAuth(..))
import qualified Text.HTML.TagSoup as S

type Tag = S.Tag String

-- Tagsoup's default escape function escapes " to &quot;, but this is only
-- required inside quoted strings and only bloats the html in other places.
-- Even worse, it can render inline stylesheets invalid. I do not have
-- quoted strings with quotes in them, so it is fine not to escape quotes.
escapeHtml :: String -> String
escapeHtml = concatMap escape
  where escape '&' = "&amp;"
        escape '<' = "&lt;"
        escape '>' = "&gt;"
        escape  c  = [c]

-- Render options for Tagsoup that use the above escape function, and and that
-- do not escape inside <style> tags in addition to the default <script> tags.
renderOptions :: S.RenderOptions String
renderOptions = S.RenderOptions escapeHtml minimize rawTag
  where minimize _ = False -- Do not omit closing tags for empty tags.
        rawTag tag = (tag == "script") || (tag == "style")

-- Like Tagsoup's renderTags, but with the above options applied.
renderTags :: [Tag] -> String
renderTags = S.renderTagsOptions renderOptions

-- Reexport of Tagsoup's parseTags for symmetry.
parseTags :: String -> [Tag]
parseTags = S.parseTags

-- Applies a function to the text of a text tag.
mapText :: (String -> String) -> Tag -> Tag
mapText f (S.TagText str) = S.TagText (f str)
mapText _ tag             = tag

-- Various classifications for tags: inside body, inside code, etc.
data TagClass = A
              | Abbr
              | Archive -- Not an html tag, but an id.
              | Article
              | BlockQuote
              | Code
              | Em
              | H1
              | H2
              | H3
              | Head
              | Header
              | Ol
              | Pre
              | RunIn  -- Not an html tag, but a class.
              | Script
              | Smcp   -- Not an html tag, but a class.
              | Style
              | Strong
              | Sub
              | Sup
              | Table
              | Teaser -- Not an html tag, but an id.
              | Th
              | Ul
              | Var
              deriving (Eq, Ord, Show)

tagClassFromName :: String -> Maybe TagClass
tagClassFromName name = case name of
  "a"          -> Just A
  "abbr"       -> Just Abbr
  "article"    -> Just Article
  "blockquote" -> Just BlockQuote
  "code"       -> Just Code
  "em"         -> Just Em
  "h1"         -> Just H1
  "h2"         -> Just H2
  "h3"         -> Just H3
  "head"       -> Just Head
  "header"     -> Just Header
  "ol"         -> Just Ol
  "pre"        -> Just Pre
  "script"     -> Just Script
  "style"      -> Just Style
  "strong"     -> Just Strong
  "sub"        -> Just Sub
  "sup"        -> Just Sup
  "table"      -> Just Table
  "th"         -> Just Th
  "ul"         -> Just Ul
  "var"        -> Just Var
  _            -> Nothing

tagClassFromAttributes :: [(String, String)] -> Maybe TagClass
tagClassFromAttributes = msum . fmap fromAttr
  where fromAttr attr = case attr of
          ("class", "smcp")    -> Just Smcp
          ("class", "run-in")  -> Just RunIn
          ("class", "archive") -> Just Archive
          ("id", "teaser")     -> Just Teaser
          _                    -> Nothing

-- Try to classify the tag based on the tag name and based on the attributes.
tagClass :: String -> [(String, String)] -> [TagClass]
tagClass name attrs = catMaybes [tagClassFromName name, tagClassFromAttributes attrs]

-- A stack of tag name (string) and classification.
type TagStack = [(String, [TagClass])]

updateTagStack :: TagStack -> Tag -> TagStack
updateTagStack ts tag = case tag of
  S.TagOpen name attrs -> case tagClass name attrs of
   []             -> ts
   classification -> (name, classification) : ts
  S.TagClose name -> case ts of
    (topName, _) : more -> if topName == name then more else ts
    _                   -> ts
  _                     -> ts

-- Determines for every tag the nested tag classifications.
tagStacks :: [Tag] -> [[TagClass]]
tagStacks = fmap (concatMap snd) . scanl updateTagStack []

data TagProperties = TagProperties { isA          :: Bool
                                   , isAbbr       :: Bool
                                   , isArchive    :: Bool
                                   , isArticle    :: Bool
                                   , isBlockQuote :: Bool
                                   , isCode       :: Bool
                                   , isEm         :: Bool
                                   , isH1         :: Bool
                                   , isH2         :: Bool
                                   , isH3         :: Bool
                                   , isHead       :: Bool
                                   , isHeader     :: Bool
                                   , isOl         :: Bool
                                   , isPre        :: Bool
                                   , isRunIn      :: Bool
                                   , isScript     :: Bool
                                   , isSmcp       :: Bool
                                   , isStyle      :: Bool
                                   , isStrong     :: Bool
                                   , isSub        :: Bool
                                   , isSup        :: Bool
                                   , isTable      :: Bool
                                   , isTeaser     :: Bool
                                   , isTh         :: Bool
                                   , isUl         :: Bool
                                   , isVar        :: Bool }

isHeading :: TagProperties -> Bool
isHeading t = (isH1 t) || (isH2 t) || (isH3 t)

isSubtitle :: TagProperties -> Bool
isSubtitle t = (isHeader t) && (isH2 t)

isArchiveLink :: TagProperties -> Bool
isArchiveLink t = (isArchive t) && (isSmcp t) && (isA t)

isTeaserLink :: TagProperties -> Bool
isTeaserLink t = (isTeaser t) && (isA t)

getProperties :: [TagClass] -> TagProperties
getProperties ts =
  let test cls = (cls `elem` ts)
  in TagProperties { isA          = test A
                   , isAbbr       = test Abbr
                   , isArchive    = test Archive
                   , isArticle    = test Article
                   , isBlockQuote = test BlockQuote
                   , isCode       = test Code
                   , isEm         = test Em
                   , isH1         = test H1
                   , isH2         = test H2
                   , isH3         = test H3
                   , isHead       = test Head
                   , isHeader     = test Header
                   , isOl         = test Ol
                   , isPre        = test Pre
                   , isRunIn      = test RunIn
                   , isScript     = test Script
                   , isSmcp       = test Smcp
                   , isStyle      = test Style
                   , isStrong     = test Strong
                   , isSub        = test Sub
                   , isSup        = test Sup
                   , isTable      = test Table
                   , isTeaser     = test Teaser
                   , isTh         = test Th
                   , isUl         = test Ul
                   , isVar        = test Var }

-- Given a list of tags, classifies them as "inside code", "inside em", etc.
classifyTags :: [Tag] -> [(Tag, TagProperties)]
classifyTags tags = zip tags $ fmap getProperties $ tagStacks tags

-- Discards tags for which the predicate returns false.
filterTags :: (TagProperties -> Bool) -> [Tag] -> [Tag]
filterTags predicate = fmap fst . filter (predicate . snd) . classifyTags

-- Applies a mapping function to the tags when the predicate p returns true for
-- that tag. The function tmap is a way to abstract over the mapping function,
-- it should not alter the length of the list.
applyTagsWhere :: (TagProperties -> Bool) -> ([Tag] -> [Tag]) -> [Tag] -> [Tag]
applyTagsWhere p tmap tags = fmap select $ zip (classifyTags tags) (tmap tags)
  where select ((orig, props), mapped) = if p props then mapped else orig

-- Applies the function f to all tags for which p returns true.
mapTagsWhere :: (TagProperties -> Bool) -> (Tag -> Tag) -> [Tag] -> [Tag]
mapTagsWhere p f = applyTagsWhere p (fmap f)

-- Applies the function f to all tags for which p returns true and flattens the result.
concatMapTagsWhere :: (TagProperties -> Bool) -> (Tag -> [Tag]) -> [Tag] -> [Tag]
concatMapTagsWhere p f = concatMap select . classifyTags
  where select (tag, props) = if (p props) then f tag else [tag]

-- Returns the the text in all tags that satisfy the selector.
getTextInTag :: (TagProperties -> Bool) -> String -> String
getTextInTag p  = join . intersperse " " . getText . (filterTags p) . parseTags
  where getText = fmap S.fromTagText . filter S.isTagText

-- Returns a list of text in text nodes, together with a value selected by f.
mapTextWith :: (TagProperties -> a) -> String -> [(String, a)]
mapTextWith f = fmap select . (filter $ S.isTagText . fst) . classifyTags . parseTags
  where select (tag, props) = (S.fromTagText tag, f props)

-- Returns whether an <ul> tag is present in the <article> in an html string.
hasUl :: String -> Bool
hasUl = not . null
      . filter (isArticle . snd)
      . filter (S.isTagOpenName "ul" . fst)
      . classifyTags
      . parseTags

-- Returns whether a <h2> tag is present in the <article> in an html string.
hasH2 :: String -> Bool
hasH2 = not . null
      . filter (isArticle . snd)
      . filter (S.isTagOpenName "h2" . fst)
      . classifyTags
      . parseTags

-- Returns whether an html snippet contains a <sub>, <sup>, or <var> tag.
hasMath :: String -> Bool
hasMath = any (\t -> isSub t || isSup t || isVar t)
        . fmap snd
        . classifyTags
        . parseTags

-- Returns whether a <img> tag is present in the html string. Note that although
-- we later replace <img> with <object> for svg images, at the time when this
-- scan runs, we still have the <img>.
hasImg :: String -> Bool
hasImg = not . null
       . filter (S.isTagOpenName "img")
       . parseTags

-- Returns the length of the longest ordered list in an html string.
maxOlLength :: String -> Int
maxOlLength = maximum . foldl listLength [0] . classifyTags . parseTags
  where listLength ns     ((S.TagOpen  "ol" _), _  )            = 0 : ns
        listLength (n:ns) ((S.TagOpen  "li" _), cls) | isOl cls = (n + 1) : ns
        listLength ns     _                                     = ns

-- Adds <span class="run-in"> around the first n characters of an html snippet.
-- Assumes that the html starts with a <p> tag.
makeRunIn :: String -> Int -> String
makeRunIn html n  = prefix ++ (drop 3 runIn) ++ "</span>" ++ after
  where (runIn, after) = splitAt (n + 3) html
        prefix         = "<p><span class=\"run-in\">"

-- Given a piece of html, strips the "align" attributes from table elements.
-- Pandoc adds these alignment tags to tables, but they are deprecated in html5.
-- If you tell Pandoc to write html5, it will just add style attributes instead.
-- I always align left anyway, so strip the attribute altogether. Furthermore,
-- I do not use the odd and even classes, so strip them to save space.
cleanTables :: [Tag] -> [Tag]
cleanTables = mapTagsWhere isTable stripAttrs
  where filterAlign = filter $ (/= "align") . fst
        filterEven  = delete ("class", "even")
        filterOdd   = delete ("class", "odd")
        filterAttrs = filterAlign . filterEven . filterOdd
        stripAttrs tag = case tag of
          S.TagOpen name attrs -> S.TagOpen name $ filterAttrs attrs
          _                    -> tag

-- Add an empty <a> tag to every <h2> that has an id, and link it to that id.
addAnchors :: [Tag] -> [Tag]
addAnchors = concatMap expandHeader
  where
    emptyA href = [S.TagOpen "a" [("href", href)], S.TagClose "a"]
    expandHeader tag = case tag of
      h2@(S.TagOpen "h2" [("id", anchor)]) -> h2 : emptyA ('#' : anchor)
      otherTag -> [otherTag]

-- Probably overkill.. Automatically add affiliate tags and footnote
-- about it.
modifyLinks :: [Tag] -> [Tag]
modifyLinks = go False False
  where
    go hasAffiliate addedNote = \case
      [] ->
        if hasAffiliate && not addedNote
          then footnotesDivider ++ footnotesHeader ++ affiliateNote
          else []
      (tag@(S.TagOpen "a" attrs) : tags) ->
        case getAttr "href" attrs of
          Nothing -> tag : go hasAffiliate addedNote tags
          Just href ->
            case parseURIReference href of
              Nothing -> error $ concat
                ["Expected href in <a> tag, ", show href, ", to be a valid url."]
              Just uri
                | isAmazonUri uri ->
                    let href' = flip (uriToString id) "" $
                          uri
                            { uriAuthority = Just URIAuth
                              { uriUserInfo = ""
                              , uriRegName = "smile.amazon.com"
                              , uriPort = ""
                              }
                            , uriQuery = "?tag=mgsloan06-20"
                            }
                     in S.TagOpen "a" (setAttr "href" href' attrs) : addSuperScript addedNote tags
                | otherwise ->
                  tag : go hasAffiliate addedNote tags
      -- Drop hr, add header, and add footnote for #amazon-links if needed.
      (divTag@(S.TagOpen "section" (lookup "class" -> Just "footnotes")) : afterDiv)
        | not addedNote ->
        case afterDiv of
          ( S.TagText _ :
            S.TagOpen "hr" _ :
            S.TagClose "hr" :
            S.TagText _ :
            rest ) ->
              footnotesDivider ++ footnotesHeader ++ [divTag] ++ go False True rest ++
              if hasAffiliate then affiliateNote else []
          _ -> error $ "Unexpected tags after footnotes div: " ++ show afterDiv
      (tag : tags) -> tag : go hasAffiliate addedNote tags
    footnotesDivider =
      [ S.TagOpen "hr" [("class", "hairline footnotes-divider")]
      , S.TagClose "hr"
      ]
    footnotesHeader =
      [ S.TagOpen "h2" []
      , S.TagText "Footnotes"
      , S.TagClose "h2"
      ]
    -- TODO: Might layout more consistently as an additional li in the
    -- footnotes list.
    --
    -- TODO: Support affiliate links in footnotes?
    affiliateNote =
      [ S.TagOpen "ul" [("class", "affiliate")]
      , S.TagOpen "li" [("id", "affiliate-note")]
      , S.TagOpen "p" []
      , S.TagText (unwords
        [ "Some links are amazon affiliate links, which sometimes send me a bit of money when you make purchases after clicking them."
        , "The purpose of this blog is sharing information and ideas, not making money."
        , "But I figure I may as well add them, and I appreciate usage of them!"
        , "If you do feel inclined to show your appreciation for these posts directly in monetary form, feel free to "
        ])
      , S.TagOpen "a" [("href", "https://buymeacoffee.com/mgsloan")]
      , S.TagText "buy me a coffee"
      , S.TagClose "a"
      , S.TagText "."
      , S.TagClose "p"
      , S.TagClose "li"
      , S.TagClose "ul"
      ]
    addSuperScript addedNote = \case
      [] -> []
      ((S.TagOpen "a" _) : _) ->
        error "Didn't expect opening <a> tag inside amazon <a> tag"
      (S.TagClose "a" : tags) ->
        S.TagClose "a" :
        S.TagOpen "sup" [] :
        S.TagOpen "a" [("href", "#affiliate-note")] :
        S.TagText "$" :
        S.TagClose "a" :
        S.TagClose "sup" :
        go True addedNote tags
      (tag : tags) ->
        tag : addSuperScript addedNote tags

getAttr :: String -> [S.Attribute String] -> Maybe String
getAttr name = fmap snd . find ((name ==) . fst)

setAttr :: String -> String -> [S.Attribute String] -> [S.Attribute String]
setAttr name val = map modifyAttr
  where
    modifyAttr unmodified@(name', _)
      | name == name' = (name', val)
      | otherwise = unmodified

isAmazonUri :: URI -> Bool
isAmazonUri uri =
  case uriAuthority uri of
    Nothing -> False
    Just authority ->
      uriRegName authority `elem` ["amazon.com", "smile.amazon.com"]
