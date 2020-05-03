module Page where

import           Util
import qualified Data.Map as M
import qualified Html
import qualified Post
import qualified Template
import qualified Type

data Page = Page
  { sourceDir :: FilePath
  , title :: String
  , slug :: String
  , primaryImage :: Maybe String
  , body :: String
  }

-- Given a slug and the contents of the nonpost file (markdown with
-- front matter), renders the body to html and parses the metadata.
parse :: FilePath -> String -> String -> Page
parse dir postSlug contents =
  Page
    { sourceDir = dir
    , title = postTitle
    , slug = postSlug
    , primaryImage = M.lookup "primary-image" frontMatter
    , body = Post.refineType
           $ Html.modifyLinks
           $ Html.cleanTables
           $ Html.addAnchors
           $ Html.parseTags
           $ Post.renderMarkdown bodyContents
    }
  where
    (frontMatter, bodyContents) = Post.extractFrontMatter contents
    postTitle = frontMatter ! "title"

url :: Page -> String
url page = "/non-posts/" ++ slug page

-- Returns the template expansion context for the page.
context :: Page -> Template.Context
context p = fmap Template.StringValue ctx
  where ctx       = M.union fields (M.mapMaybe id optFields)
        fields    = M.fromList [ ("title", title p)
                               , ("title-html", Type.makeAbbrs $ title p)
                               , ("url", url p)
                               , ("content", body p) ]
        optFields = M.fromList [ ("bold-font", boldFontField)
                               , ("italic-font", italicFontField)
                               , ("math", mathField)
                               , ("img", imgField)
                               , ("mono-font", monoFontField)
                               , ("serif-italic-font", serifItalicFontField)
                               , ("primary-image", primaryImage p) ]
        boldFontField        = Post.booleanField $ Type.usesBoldFont $ body p
        italicFontField      = Post.booleanField $ Post.usesItalicFont $ body p
        mathField            = Post.booleanField $ Html.hasMath $ body p
        imgField             = Post.booleanField $ Html.hasImg $ body p
        monoFontField        = Post.booleanField $ Post.usesMonoFont $ body p
        serifItalicFontField = Post.booleanField $ Type.usesSerifItalicFont $ body p
