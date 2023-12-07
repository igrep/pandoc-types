{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
    FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP,
    TemplateHaskell , PatternSynonyms, ViewPatterns, StrictData, MagicHash #-}

{-
Copyright (c) 2006-2023, John MacFarlane

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of John MacFarlane nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- |
   Module      : Text.Pandoc.Definition
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of 'Pandoc' data structure for format-neutral representation
of documents.
-}
module Text.Pandoc.Definition ( Pandoc(..)
                              , Meta(..)
                              , MetaValue(..)
                              , nullMeta
                              , isNullMeta
                              , lookupMeta
                              , docTitle
                              , docAuthors
                              , docDate
                              , Block(..)
                              , pattern SimpleFigure
                              , Inline(..)
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , nullAttr
                              , Caption(..)
                              , ShortCaption
                              , RowHeadColumns(..)
                              , Alignment(..)
                              , ColWidth(..)
                              , ColSpec
                              , Row(..)
                              , TableHead(..)
                              , TableBody(..)
                              , TableFoot(..)
                              , Cell(..)
                              , RowSpan(..)
                              , ColSpan(..)
                              , QuoteType(..)
                              , Target
                              , MathType(..)
                              , Citation(..)
                              , CitationMode(..)
                              , pandocTypesVersion
                              ) where

import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.String
import Control.DeepSeq
import Paths_pandoc_types (version)
import Data.Version (Version, versionBranch)
import Data.Semigroup (Semigroup(..))
import Control.Arrow (second)

import qualified Data.Aeson.Types.ToJSON
import qualified Data.Aeson.Key
import qualified Data.Aeson.Internal.ByteString
import qualified Data.Aeson.Encoding.Internal
import qualified Data.Aeson.TH
import qualified Data.Vector
import qualified Data.Vector.Mutable

data Pandoc = Pandoc Meta [Block]
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Semigroup Pandoc where
  (Pandoc m1 bs1) <> (Pandoc m2 bs2) =
    Pandoc (m1 <> m2) (bs1 <> bs2)
instance Monoid Pandoc where
  mempty = Pandoc mempty mempty
  mappend = (<>)

-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map Text MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance Semigroup Meta where
  (Meta m1) <> (Meta m2) = Meta (M.union m2 m1)
  -- note: M.union is left-biased, so if there are fields in both m2
  -- and m1, m2 wins.
instance Monoid Meta where
  mempty = Meta M.empty
  mappend = (<>)

data MetaValue = MetaMap (M.Map Text MetaValue)
               | MetaList [MetaValue]
               | MetaBool Bool
               | MetaString Text
               | MetaInlines [Inline]
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

nullMeta :: Meta
nullMeta = Meta M.empty

isNullMeta :: Meta -> Bool
isNullMeta (Meta m) = M.null m

-- Helper functions to extract metadata

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: Text -> Meta -> Maybe MetaValue
lookupMeta key (Meta m) = M.lookup key m

-- | Extract document title from metadata; works just like the old @docTitle@.
docTitle :: Meta -> [Inline]
docTitle meta =
  case lookupMeta "title" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Extract document authors from metadata; works just like the old
-- @docAuthors@.
docAuthors :: Meta -> [[Inline]]
docAuthors meta =
  case lookupMeta "author" meta of
        Just (MetaString s)    -> [[Str s]]
        Just (MetaInlines ils) -> [ils]
        Just (MetaList   ms)   -> [ils | MetaInlines ils <- ms] ++
                                  [ils | MetaBlocks [Plain ils] <- ms] ++
                                  [ils | MetaBlocks [Para ils]  <- ms] ++
                                  [[Str x] | MetaString x <- ms]
        _                      -> []

-- | Extract date from metadata; works just like the old @docDate@.
docDate :: Meta -> [Inline]
docDate meta =
  case lookupMeta "date" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | List attributes.  The first element of the triple is the
-- start number of the list.
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

-- | Style of list numbers.
data ListNumberStyle = DefaultStyle
                     | Example
                     | Decimal
                     | LowerRoman
                     | UpperRoman
                     | LowerAlpha
                     | UpperAlpha deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Delimiter of list numbers.
data ListNumberDelim = DefaultDelim
                     | Period
                     | OneParen
                     | TwoParens deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Attributes: identifier, classes, key-value pairs
type Attr = (Text, [Text], [(Text, Text)])

nullAttr :: Attr
nullAttr = ("",[],[])

-- | Formats for raw blocks
newtype Format = Format Text
               deriving (Read, Show, Typeable, Data, Generic, ToJSON, FromJSON)

instance IsString Format where
  fromString f = Format $ T.toCaseFold $ T.pack f

instance Eq Format where
  Format x == Format y = T.toCaseFold x == T.toCaseFold y

instance Ord Format where
  compare (Format x) (Format y) = compare (T.toCaseFold x) (T.toCaseFold y)

-- | The number of columns taken up by the row head of each row of a
-- 'TableBody'. The row body takes up the remaining columns.
newtype RowHeadColumns = RowHeadColumns Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Alignment of a table column.
data Alignment = AlignLeft
               | AlignRight
               | AlignCenter
               | AlignDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The width of a table column, as a percentage of the text width.
data ColWidth = ColWidth Double
              | ColWidthDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The specification for a single table column.
type ColSpec = (Alignment, ColWidth)

-- | A table row.
data Row = Row Attr [Cell]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The head of a table.
data TableHead = TableHead Attr [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A body of a table, with an intermediate head, intermediate body,
-- and the specified number of row header columns in the intermediate
-- body.
data TableBody = TableBody Attr RowHeadColumns [Row] [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The foot of a table.
data TableFoot = TableFoot Attr [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption = [Inline]

-- | The caption of a table or figure, with optional short caption.
data Caption = Caption (Maybe ShortCaption) [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A table cell.
data Cell = Cell Attr Alignment RowSpan ColSpan [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The number of rows occupied by a cell; the height of a cell.
newtype RowSpan = RowSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | The number of columns occupied by a cell; the width of a cell.
newtype ColSpan = ColSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Block element.
data Block
    -- | Plain text, not a paragraph
    = Plain [Inline]
    -- | Paragraph
    | Para [Inline]
    -- | Multiple non-breaking lines
    | LineBlock [[Inline]]
    -- | Code block (literal) with attributes
    | CodeBlock Attr Text
    -- | Raw block
    | RawBlock Format Text
    -- | Block quote (list of blocks)
    | BlockQuote [Block]
    -- | Ordered list (attributes and a list of items, each a list of
    -- blocks)
    | OrderedList ListAttributes [[Block]]
    -- | Bullet list (list of items, each a list of blocks)
    | BulletList [[Block]]
    -- | Definition list. Each list item is a pair consisting of a
    -- term (a list of inlines) and one or more definitions (each a
    -- list of blocks)
    | DefinitionList [([Inline],[[Block]])]
    -- | Header - level (integer) and text (inlines)
    | Header Int Attr [Inline]
    -- | Horizontal rule
    | HorizontalRule
    -- | Table, with attributes, caption, optional short caption,
    -- column alignments and widths (required), table head, table
    -- bodies, and table foot
    | Table Attr Caption [ColSpec] TableHead [TableBody] TableFoot
    -- | Figure, with attributes, caption, and content (list of blocks)
    | Figure Attr Caption [Block]
    -- | Generic block container with attributes
    | Div Attr [Block]
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Link target (URL, title).
type Target = (Text, Text)

isFigureTarget :: Target -> Maybe Target
isFigureTarget tgt
  | (src, Just tit) <- second (T.stripPrefix "fig:") tgt = Just (src, tit)
  | otherwise = Nothing

-- | Bidirectional pattern synonym
--
-- It can pass as a Block constructor
--
-- >>> SimpleFigure nullAttr [] (T.pack "", T.pack "title")
-- Para [Image ("",[],[]) [] ("","fig:title")]
--
--
-- It can be used to pattern match
-- >>> let img = Para [Image undefined undefined (undefined, T.pack "title")]
-- >>> case img of { SimpleFigure _ _ _ -> True; _ -> False }
-- False
-- >>> let fig = Para [Image undefined undefined (undefined, T.pack "fig:title")]
-- >>> case fig of { SimpleFigure _ _ tit -> snd tit; _ -> T.pack "" }
-- "title"
pattern SimpleFigure :: Attr -> [Inline] -> Target -> Block
pattern SimpleFigure attr figureCaption tgt <-
    Para [Image attr figureCaption
        (isFigureTarget -> Just tgt)]  where
  SimpleFigure attr figureCaption tgt =
    Para [Image attr figureCaption (second ("fig:" <>) tgt)]


-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Inline elements.
data Inline
    = Str Text            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Underline [Inline]    -- ^  Underlined text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr Text      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType Text  -- ^ TeX math (literal)
    | RawInline Format Text -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data Citation = Citation { citationId      :: Text
                         , citationPrefix  :: [Inline]
                         , citationSuffix  :: [Inline]
                         , citationMode    :: CitationMode
                         , citationNoteNum :: Int
                         , citationHash    :: Int
                         }
                deriving (Show, Eq, Read, Typeable, Data, Generic)

instance Ord Citation where
    compare = comparing citationHash

data CitationMode = AuthorInText | SuppressAuthor | NormalCitation
                    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)


-- ToJSON/FromJSON instances. Some are defined by hand so that we have
-- more control over the format.

instance ToJSON MetaValue where
  toJSON
    = let
      in
        \ value_al6D
          -> case value_al6D of
               MetaMap arg1_al6I
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "MetaMap"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_al6I))
               MetaList arg1_al6N
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "MetaList"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_al6N))
               MetaBool arg1_al6O
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "MetaBool"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_al6O))
               MetaString arg1_al6P
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "MetaString"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_al6P))
               MetaInlines arg1_al6Q
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "MetaInlines"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_al6Q))
               MetaBlocks arg1_al6R
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "MetaBlocks"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_al6R))
  toEncoding
    = let
        _let1_al6Z
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"c\":"#
        _let0_al6Y
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al6S
          -> case value_al6S of
               MetaMap arg1_al6X
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al6Y)
                         (Data.Aeson.Encoding.Internal.text (T.pack "MetaMap"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al6Z)
                             (toEncoding arg1_al6X))
               MetaList arg1_al74
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al6Y)
                         (Data.Aeson.Encoding.Internal.text (T.pack "MetaList"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al6Z)
                             (toEncoding arg1_al74))
               MetaBool arg1_al75
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al6Y)
                         (Data.Aeson.Encoding.Internal.text (T.pack "MetaBool"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al6Z)
                             (toEncoding arg1_al75))
               MetaString arg1_al76
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al6Y)
                         (Data.Aeson.Encoding.Internal.text (T.pack "MetaString"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al6Z)
                             (toEncoding arg1_al76))
               MetaInlines arg1_al77
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al6Y)
                         (Data.Aeson.Encoding.Internal.text (T.pack "MetaInlines"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al6Z)
                             (toEncoding arg1_al77))
               MetaBlocks arg1_al78
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al6Y)
                         (Data.Aeson.Encoding.Internal.text (T.pack "MetaBlocks"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al6Z)
                             (toEncoding arg1_al78))
instance FromJSON MetaValue where
  parseJSON
    = \ value_al79
        -> case value_al79 of
             Object obj_al7a
               -> do conKeyX_al7b <- (obj_al7a .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al7b of
                       _ | (conKeyX_al7b == Data.Aeson.Key.fromString "MetaMap")
                         -> do val_al7g <- (obj_al7a .: Data.Aeson.Key.fromString "c")
                               case val_al7g of arg_al7h -> (MetaMap <$> parseJSON arg_al7h)
                         | (conKeyX_al7b == Data.Aeson.Key.fromString "MetaList")
                         -> do val_al7m <- (obj_al7a .: Data.Aeson.Key.fromString "c")
                               case val_al7m of arg_al7n -> (MetaList <$> parseJSON arg_al7n)
                         | (conKeyX_al7b == Data.Aeson.Key.fromString "MetaBool")
                         -> do val_al7o <- (obj_al7a .: Data.Aeson.Key.fromString "c")
                               case val_al7o of arg_al7p -> (MetaBool <$> parseJSON arg_al7p)
                         | (conKeyX_al7b == Data.Aeson.Key.fromString "MetaString")
                         -> do val_al7q <- (obj_al7a .: Data.Aeson.Key.fromString "c")
                               case val_al7q of arg_al7r -> (MetaString <$> parseJSON arg_al7r)
                         | (conKeyX_al7b == Data.Aeson.Key.fromString "MetaInlines")
                         -> do val_al7s <- (obj_al7a .: Data.Aeson.Key.fromString "c")
                               case val_al7s of arg_al7t -> (MetaInlines <$> parseJSON arg_al7t)
                         | (conKeyX_al7b == Data.Aeson.Key.fromString "MetaBlocks")
                         -> do val_al7u <- (obj_al7a .: Data.Aeson.Key.fromString "c")
                               case val_al7u of arg_al7v -> (MetaBlocks <$> parseJSON arg_al7v)
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.MetaValue")
                               ["MetaMap", "MetaList", "MetaBool", "MetaString", "MetaInlines",
                                "MetaBlocks"])
                              (Data.Aeson.Key.toString conKeyX_al7b)
             other_al7w
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.MetaValue")
                    (Data.Aeson.TH.valueConName other_al7w)
instance ToJSON CitationMode where
  toJSON
    = let
      in
        \ value_al7x
          -> case value_al7x of
               AuthorInText
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "AuthorInText")))
               SuppressAuthor
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "SuppressAuthor")))
               NormalCitation
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "NormalCitation")))
  toEncoding
    = let
        _let0_al7z
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al7y
          -> case value_al7y of
               AuthorInText
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al7z)
                         (Data.Aeson.Encoding.Internal.text (T.pack "AuthorInText")))
               SuppressAuthor
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al7z)
                         (Data.Aeson.Encoding.Internal.text (T.pack "SuppressAuthor")))
               NormalCitation
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al7z)
                         (Data.Aeson.Encoding.Internal.text (T.pack "NormalCitation")))
instance FromJSON CitationMode where
  parseJSON
    = \ value_al7A
        -> case value_al7A of
             Object obj_al7B
               -> do conKeyX_al7C <- (obj_al7B .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al7C of
                       _ | (conKeyX_al7C == Data.Aeson.Key.fromString "AuthorInText")
                         -> pure AuthorInText
                         | (conKeyX_al7C == Data.Aeson.Key.fromString "SuppressAuthor")
                         -> pure SuppressAuthor
                         | (conKeyX_al7C == Data.Aeson.Key.fromString "NormalCitation")
                         -> pure NormalCitation
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.CitationMode")
                               ["AuthorInText", "SuppressAuthor", "NormalCitation"])
                              (Data.Aeson.Key.toString conKeyX_al7C)
             other_al7D
               -> (Data.Aeson.TH.noObjectFail
                     "Text.Pandoc.Definition.CitationMode")
                    (Data.Aeson.TH.valueConName other_al7D)
instance ToJSON Citation where
  toJSON
    = let
      in
        \ value_al7E
          -> case value_al7E of
               Citation arg1_al7F arg2_al7G arg3_al7H arg4_al7I arg5_al7J
                        arg6_al7K
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((if (const False) arg1_al7F then
                            mempty
                        else
                            (Data.Aeson.Types.ToJSON.pair
                               (Data.Aeson.Key.fromString "citationId"))
                              (toJSON arg1_al7F))
                         <>
                           ((if (const False) arg2_al7G then
                                 mempty
                             else
                                 (Data.Aeson.Types.ToJSON.pair
                                    (Data.Aeson.Key.fromString "citationPrefix"))
                                   (toJSON arg2_al7G))
                              <>
                                ((if (const False) arg3_al7H then
                                      mempty
                                  else
                                      (Data.Aeson.Types.ToJSON.pair
                                         (Data.Aeson.Key.fromString "citationSuffix"))
                                        (toJSON arg3_al7H))
                                   <>
                                     ((if (const False) arg4_al7I then
                                           mempty
                                       else
                                           (Data.Aeson.Types.ToJSON.pair
                                              (Data.Aeson.Key.fromString "citationMode"))
                                             (toJSON arg4_al7I))
                                        <>
                                          ((if (const False) arg5_al7J then
                                                mempty
                                            else
                                                (Data.Aeson.Types.ToJSON.pair
                                                   (Data.Aeson.Key.fromString
                                                      "citationNoteNum"))
                                                  (toJSON arg5_al7J))
                                             <>
                                               (if (const False) arg6_al7K then
                                                    mempty
                                                else
                                                    (Data.Aeson.Types.ToJSON.pair
                                                       (Data.Aeson.Key.fromString
                                                          "citationHash"))
                                                      (toJSON arg6_al7K)))))))
  toEncoding
    = let
        _let5_al7X
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 15)
              "\"citationHash\":"#
        _let0_al7S
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 13)
              "\"citationId\":"#
        _let3_al7V
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 15)
              "\"citationMode\":"#
        _let4_al7W
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 18)
              "\"citationNoteNum\":"#
        _let1_al7T
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 17)
              "\"citationPrefix\":"#
        _let2_al7U
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 17)
              "\"citationSuffix\":"#
      in
        \ value_al7L
          -> case value_al7L of
               Citation arg1_al7M arg2_al7N arg3_al7O arg4_al7P arg5_al7Q
                        arg6_al7R
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((if (const False) arg1_al7M then
                            mempty
                        else
                            (Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al7S)
                              (toEncoding arg1_al7M))
                         <>
                           ((if (const False) arg2_al7N then
                                 mempty
                             else
                                 (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al7T)
                                   (toEncoding arg2_al7N))
                              <>
                                ((if (const False) arg3_al7O then
                                      mempty
                                  else
                                      (Data.Aeson.Encoding.Internal.unsafePairSBS _let2_al7U)
                                        (toEncoding arg3_al7O))
                                   <>
                                     ((if (const False) arg4_al7P then
                                           mempty
                                       else
                                           (Data.Aeson.Encoding.Internal.unsafePairSBS
                                              _let3_al7V)
                                             (toEncoding arg4_al7P))
                                        <>
                                          ((if (const False) arg5_al7Q then
                                                mempty
                                            else
                                                (Data.Aeson.Encoding.Internal.unsafePairSBS
                                                   _let4_al7W)
                                                  (toEncoding arg5_al7Q))
                                             <>
                                               (if (const False) arg6_al7R then
                                                    mempty
                                                else
                                                    (Data.Aeson.Encoding.Internal.unsafePairSBS
                                                       _let5_al7X)
                                                      (toEncoding arg6_al7R)))))))
instance FromJSON Citation where
  parseJSON
    = \ value_al7Y
        -> case value_al7Y of
             Object recObj_al7Z
               -> ((((((Citation
                          <$>
                            (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                                 "Text.Pandoc.Definition.Citation")
                                "Citation")
                               recObj_al7Z)
                              (Data.Aeson.Key.fromString "citationId"))
                         <*>
                           (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                                "Text.Pandoc.Definition.Citation")
                               "Citation")
                              recObj_al7Z)
                             (Data.Aeson.Key.fromString "citationPrefix"))
                        <*>
                          (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                               "Text.Pandoc.Definition.Citation")
                              "Citation")
                             recObj_al7Z)
                            (Data.Aeson.Key.fromString "citationSuffix"))
                       <*>
                         (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                              "Text.Pandoc.Definition.Citation")
                             "Citation")
                            recObj_al7Z)
                           (Data.Aeson.Key.fromString "citationMode"))
                      <*>
                        (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                             "Text.Pandoc.Definition.Citation")
                            "Citation")
                           recObj_al7Z)
                          (Data.Aeson.Key.fromString "citationNoteNum"))
                     <*>
                       (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                            "Text.Pandoc.Definition.Citation")
                           "Citation")
                          recObj_al7Z)
                         (Data.Aeson.Key.fromString "citationHash"))
             other_al80
               -> (((Data.Aeson.TH.parseTypeMismatch' "Citation")
                      "Text.Pandoc.Definition.Citation")
                     "Object")
                    (Data.Aeson.TH.valueConName other_al80)
instance ToJSON QuoteType where
  toJSON
    = let
      in
        \ value_al81
          -> case value_al81 of
               SingleQuote
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "SingleQuote")))
               DoubleQuote
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "DoubleQuote")))
  toEncoding
    = let
        _let0_al83
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al82
          -> case value_al82 of
               SingleQuote
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al83)
                         (Data.Aeson.Encoding.Internal.text (T.pack "SingleQuote")))
               DoubleQuote
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al83)
                         (Data.Aeson.Encoding.Internal.text (T.pack "DoubleQuote")))
instance FromJSON QuoteType where
  parseJSON
    = \ value_al84
        -> case value_al84 of
             Object obj_al85
               -> do conKeyX_al86 <- (obj_al85 .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al86 of
                       _ | (conKeyX_al86 == Data.Aeson.Key.fromString "SingleQuote")
                         -> pure SingleQuote
                         | (conKeyX_al86 == Data.Aeson.Key.fromString "DoubleQuote")
                         -> pure DoubleQuote
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.QuoteType")
                               ["SingleQuote", "DoubleQuote"])
                              (Data.Aeson.Key.toString conKeyX_al86)
             other_al87
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.QuoteType")
                    (Data.Aeson.TH.valueConName other_al87)
instance ToJSON MathType where
  toJSON
    = let
      in
        \ value_al88
          -> case value_al88 of
               DisplayMath
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "DisplayMath")))
               InlineMath
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "InlineMath")))
  toEncoding
    = let
        _let0_al8a
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al89
          -> case value_al89 of
               DisplayMath
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8a)
                         (Data.Aeson.Encoding.Internal.text (T.pack "DisplayMath")))
               InlineMath
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8a)
                         (Data.Aeson.Encoding.Internal.text (T.pack "InlineMath")))
instance FromJSON MathType where
  parseJSON
    = \ value_al8b
        -> case value_al8b of
             Object obj_al8c
               -> do conKeyX_al8d <- (obj_al8c .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al8d of
                       _ | (conKeyX_al8d == Data.Aeson.Key.fromString "DisplayMath")
                         -> pure DisplayMath
                         | (conKeyX_al8d == Data.Aeson.Key.fromString "InlineMath")
                         -> pure InlineMath
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.MathType")
                               ["DisplayMath", "InlineMath"])
                              (Data.Aeson.Key.toString conKeyX_al8d)
             other_al8e
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.MathType")
                    (Data.Aeson.TH.valueConName other_al8e)
instance ToJSON ListNumberStyle where
  toJSON
    = let
      in
        \ value_al8f
          -> case value_al8f of
               DefaultStyle
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "DefaultStyle")))
               Example
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Example")))
               Decimal
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Decimal")))
               LowerRoman
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "LowerRoman")))
               UpperRoman
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "UpperRoman")))
               LowerAlpha
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "LowerAlpha")))
               UpperAlpha
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "UpperAlpha")))
  toEncoding
    = let
        _let0_al8h
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al8g
          -> case value_al8g of
               DefaultStyle
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8h)
                         (Data.Aeson.Encoding.Internal.text (T.pack "DefaultStyle")))
               Example
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8h)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Example")))
               Decimal
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8h)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Decimal")))
               LowerRoman
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8h)
                         (Data.Aeson.Encoding.Internal.text (T.pack "LowerRoman")))
               UpperRoman
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8h)
                         (Data.Aeson.Encoding.Internal.text (T.pack "UpperRoman")))
               LowerAlpha
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8h)
                         (Data.Aeson.Encoding.Internal.text (T.pack "LowerAlpha")))
               UpperAlpha
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8h)
                         (Data.Aeson.Encoding.Internal.text (T.pack "UpperAlpha")))
instance FromJSON ListNumberStyle where
  parseJSON
    = \ value_al8i
        -> case value_al8i of
             Object obj_al8j
               -> do conKeyX_al8k <- (obj_al8j .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al8k of
                       _ | (conKeyX_al8k == Data.Aeson.Key.fromString "DefaultStyle")
                         -> pure DefaultStyle
                         | (conKeyX_al8k == Data.Aeson.Key.fromString "Example")
                         -> pure Example
                         | (conKeyX_al8k == Data.Aeson.Key.fromString "Decimal")
                         -> pure Decimal
                         | (conKeyX_al8k == Data.Aeson.Key.fromString "LowerRoman")
                         -> pure LowerRoman
                         | (conKeyX_al8k == Data.Aeson.Key.fromString "UpperRoman")
                         -> pure UpperRoman
                         | (conKeyX_al8k == Data.Aeson.Key.fromString "LowerAlpha")
                         -> pure LowerAlpha
                         | (conKeyX_al8k == Data.Aeson.Key.fromString "UpperAlpha")
                         -> pure UpperAlpha
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.ListNumberStyle")
                               ["DefaultStyle", "Example", "Decimal", "LowerRoman",
                                "UpperRoman", "LowerAlpha", "UpperAlpha"])
                              (Data.Aeson.Key.toString conKeyX_al8k)
             other_al8l
               -> (Data.Aeson.TH.noObjectFail
                     "Text.Pandoc.Definition.ListNumberStyle")
                    (Data.Aeson.TH.valueConName other_al8l)
instance ToJSON ListNumberDelim where
  toJSON
    = let
      in
        \ value_al8m
          -> case value_al8m of
               DefaultDelim
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "DefaultDelim")))
               Period
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Period")))
               OneParen
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "OneParen")))
               TwoParens
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "TwoParens")))
  toEncoding
    = let
        _let0_al8o
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al8n
          -> case value_al8n of
               DefaultDelim
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8o)
                         (Data.Aeson.Encoding.Internal.text (T.pack "DefaultDelim")))
               Period
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8o)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Period")))
               OneParen
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8o)
                         (Data.Aeson.Encoding.Internal.text (T.pack "OneParen")))
               TwoParens
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8o)
                         (Data.Aeson.Encoding.Internal.text (T.pack "TwoParens")))
instance FromJSON ListNumberDelim where
  parseJSON
    = \ value_al8p
        -> case value_al8p of
             Object obj_al8q
               -> do conKeyX_al8r <- (obj_al8q .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al8r of
                       _ | (conKeyX_al8r == Data.Aeson.Key.fromString "DefaultDelim")
                         -> pure DefaultDelim
                         | (conKeyX_al8r == Data.Aeson.Key.fromString "Period")
                         -> pure Period
                         | (conKeyX_al8r == Data.Aeson.Key.fromString "OneParen")
                         -> pure OneParen
                         | (conKeyX_al8r == Data.Aeson.Key.fromString "TwoParens")
                         -> pure TwoParens
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.ListNumberDelim")
                               ["DefaultDelim", "Period", "OneParen", "TwoParens"])
                              (Data.Aeson.Key.toString conKeyX_al8r)
             other_al8s
               -> (Data.Aeson.TH.noObjectFail
                     "Text.Pandoc.Definition.ListNumberDelim")
                    (Data.Aeson.TH.valueConName other_al8s)
instance ToJSON Alignment where
  toJSON
    = let
      in
        \ value_al8t
          -> case value_al8t of
               AlignLeft
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "AlignLeft")))
               AlignRight
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "AlignRight")))
               AlignCenter
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "AlignCenter")))
               AlignDefault
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "AlignDefault")))
  toEncoding
    = let
        _let0_al8v
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al8u
          -> case value_al8u of
               AlignLeft
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8v)
                         (Data.Aeson.Encoding.Internal.text (T.pack "AlignLeft")))
               AlignRight
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8v)
                         (Data.Aeson.Encoding.Internal.text (T.pack "AlignRight")))
               AlignCenter
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8v)
                         (Data.Aeson.Encoding.Internal.text (T.pack "AlignCenter")))
               AlignDefault
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8v)
                         (Data.Aeson.Encoding.Internal.text (T.pack "AlignDefault")))
instance FromJSON Alignment where
  parseJSON
    = \ value_al8w
        -> case value_al8w of
             Object obj_al8x
               -> do conKeyX_al8y <- (obj_al8x .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al8y of
                       _ | (conKeyX_al8y == Data.Aeson.Key.fromString "AlignLeft")
                         -> pure AlignLeft
                         | (conKeyX_al8y == Data.Aeson.Key.fromString "AlignRight")
                         -> pure AlignRight
                         | (conKeyX_al8y == Data.Aeson.Key.fromString "AlignCenter")
                         -> pure AlignCenter
                         | (conKeyX_al8y == Data.Aeson.Key.fromString "AlignDefault")
                         -> pure AlignDefault
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.Alignment")
                               ["AlignLeft", "AlignRight", "AlignCenter", "AlignDefault"])
                              (Data.Aeson.Key.toString conKeyX_al8y)
             other_al8z
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.Alignment")
                    (Data.Aeson.TH.valueConName other_al8z)
instance ToJSON ColWidth where
  toJSON
    = let
      in
        \ value_al8A
          -> case value_al8A of
               ColWidth arg1_al8B
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "ColWidth"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_al8B))
               ColWidthDefault
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "ColWidthDefault")))
  toEncoding
    = let
        _let1_al8F
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"c\":"#
        _let0_al8E
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_al8C
          -> case value_al8C of
               ColWidth arg1_al8D
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8E)
                         (Data.Aeson.Encoding.Internal.text (T.pack "ColWidth"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_al8F)
                             (toEncoding arg1_al8D))
               ColWidthDefault
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_al8E)
                         (Data.Aeson.Encoding.Internal.text (T.pack "ColWidthDefault")))
instance FromJSON ColWidth where
  parseJSON
    = \ value_al8G
        -> case value_al8G of
             Object obj_al8H
               -> do conKeyX_al8I <- (obj_al8H .: Data.Aeson.Key.fromString "t")
                     case conKeyX_al8I of
                       _ | (conKeyX_al8I == Data.Aeson.Key.fromString "ColWidth")
                         -> do val_al8J <- (obj_al8H .: Data.Aeson.Key.fromString "c")
                               case val_al8J of arg_al8K -> (ColWidth <$> parseJSON arg_al8K)
                         | (conKeyX_al8I == Data.Aeson.Key.fromString "ColWidthDefault")
                         -> pure ColWidthDefault
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.ColWidth")
                               ["ColWidth", "ColWidthDefault"])
                              (Data.Aeson.Key.toString conKeyX_al8I)
             other_al8L
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.ColWidth")
                    (Data.Aeson.TH.valueConName other_al8L)
instance ToJSON Row where
  toJSON
    = let
      in
        \ value_al8M
          -> case value_al8M of
               Row arg1_al8N arg2_al8O
                 -> Array
                      (Data.Vector.create
                         (do mv_al8P <- Data.Vector.Mutable.unsafeNew
                                          2
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al8P)
                                0)
                               (toJSON arg1_al8N)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al8P)
                                1)
                               (toJSON arg2_al8O)
                             return mv_al8P))
  toEncoding
    = let
      in
        \ value_al8Q
          -> case value_al8Q of
               Row arg1_al8R arg2_al8S
                 -> Data.Aeson.Encoding.Internal.wrapArray
                      (toEncoding arg1_al8R
                         Data.Aeson.Encoding.Internal.><
                           (Data.Aeson.Encoding.Internal.comma
                              Data.Aeson.Encoding.Internal.>< toEncoding arg2_al8S))
instance FromJSON Row where
  parseJSON
    = \ value_al8T
        -> case value_al8T of
             Array arr_al8U
               -> if (Data.Vector.length
                        arr_al8U
                        == 2) then
                      ((Row
                          <$>
                            parseJSON
                              (arr_al8U
                                 `Data.Vector.unsafeIndex`
                                   0))
                         <*>
                           parseJSON
                             (arr_al8U
                                `Data.Vector.unsafeIndex`
                                  1))
                  else
                      (((Data.Aeson.TH.parseTypeMismatch' "Row")
                          "Text.Pandoc.Definition.Row")
                         "Array of length 2")
                        ("Array of length "
                           ++
                             (show
                                . Data.Vector.length)
                               arr_al8U)
             other_al8V
               -> (((Data.Aeson.TH.parseTypeMismatch' "Row")
                      "Text.Pandoc.Definition.Row")
                     "Array")
                    (Data.Aeson.TH.valueConName other_al8V)
instance ToJSON Caption where
  toJSON
    = let
      in
        \ value_al8W
          -> case value_al8W of
               Caption arg1_al8Z arg2_al90
                 -> Array
                      (Data.Vector.create
                         (do mv_al91 <- Data.Vector.Mutable.unsafeNew
                                          2
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al91)
                                0)
                               (toJSON arg1_al8Z)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al91)
                                1)
                               (toJSON arg2_al90)
                             return mv_al91))
  toEncoding
    = let
      in
        \ value_al94
          -> case value_al94 of
               Caption arg1_al97 arg2_al98
                 -> Data.Aeson.Encoding.Internal.wrapArray
                      (toEncoding arg1_al97
                         Data.Aeson.Encoding.Internal.><
                           (Data.Aeson.Encoding.Internal.comma
                              Data.Aeson.Encoding.Internal.>< toEncoding arg2_al98))
instance FromJSON Caption where
  parseJSON
    = \ value_al9b
        -> case value_al9b of
             Array arr_al9e
               -> if (Data.Vector.length
                        arr_al9e
                        == 2) then
                      ((Caption
                          <$>
                            parseJSON
                              (arr_al9e
                                 `Data.Vector.unsafeIndex`
                                   0))
                         <*>
                           parseJSON
                             (arr_al9e
                                `Data.Vector.unsafeIndex`
                                  1))
                  else
                      (((Data.Aeson.TH.parseTypeMismatch' "Caption")
                          "Text.Pandoc.Definition.Caption")
                         "Array of length 2")
                        ("Array of length "
                           ++
                             (show
                                . Data.Vector.length)
                               arr_al9e)
             other_al9h
               -> (((Data.Aeson.TH.parseTypeMismatch' "Caption")
                      "Text.Pandoc.Definition.Caption")
                     "Array")
                    (Data.Aeson.TH.valueConName other_al9h)
instance ToJSON TableHead where
  toJSON
    = let
      in
        \ value_al9i
          -> case value_al9i of
               TableHead arg1_al9j arg2_al9k
                 -> Array
                      (Data.Vector.create
                         (do mv_al9l <- Data.Vector.Mutable.unsafeNew
                                          2
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9l)
                                0)
                               (toJSON arg1_al9j)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9l)
                                1)
                               (toJSON arg2_al9k)
                             return mv_al9l))
  toEncoding
    = let
      in
        \ value_al9m
          -> case value_al9m of
               TableHead arg1_al9n arg2_al9o
                 -> Data.Aeson.Encoding.Internal.wrapArray
                      (toEncoding arg1_al9n
                         Data.Aeson.Encoding.Internal.><
                           (Data.Aeson.Encoding.Internal.comma
                              Data.Aeson.Encoding.Internal.>< toEncoding arg2_al9o))
instance FromJSON TableHead where
  parseJSON
    = \ value_al9p
        -> case value_al9p of
             Array arr_al9q
               -> if (Data.Vector.length
                        arr_al9q
                        == 2) then
                      ((TableHead
                          <$>
                            parseJSON
                              (arr_al9q
                                 `Data.Vector.unsafeIndex`
                                   0))
                         <*>
                           parseJSON
                             (arr_al9q
                                `Data.Vector.unsafeIndex`
                                  1))
                  else
                      (((Data.Aeson.TH.parseTypeMismatch' "TableHead")
                          "Text.Pandoc.Definition.TableHead")
                         "Array of length 2")
                        ("Array of length "
                           ++
                             (show
                                . Data.Vector.length)
                               arr_al9q)
             other_al9r
               -> (((Data.Aeson.TH.parseTypeMismatch' "TableHead")
                      "Text.Pandoc.Definition.TableHead")
                     "Array")
                    (Data.Aeson.TH.valueConName other_al9r)
instance ToJSON TableBody where
  toJSON
    = let
      in
        \ value_al9s
          -> case value_al9s of
               TableBody arg1_al9t arg2_al9u arg3_al9v arg4_al9w
                 -> Array
                      (Data.Vector.create
                         (do mv_al9x <- Data.Vector.Mutable.unsafeNew
                                          4
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9x)
                                0)
                               (toJSON arg1_al9t)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9x)
                                1)
                               (toJSON arg2_al9u)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9x)
                                2)
                               (toJSON arg3_al9v)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9x)
                                3)
                               (toJSON arg4_al9w)
                             return mv_al9x))
  toEncoding
    = let
      in
        \ value_al9y
          -> case value_al9y of
               TableBody arg1_al9z arg2_al9A arg3_al9B arg4_al9C
                 -> Data.Aeson.Encoding.Internal.wrapArray
                      (toEncoding arg1_al9z
                         Data.Aeson.Encoding.Internal.><
                           (Data.Aeson.Encoding.Internal.comma
                              Data.Aeson.Encoding.Internal.><
                                (toEncoding arg2_al9A
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.><
                                          (toEncoding arg3_al9B
                                             Data.Aeson.Encoding.Internal.><
                                               (Data.Aeson.Encoding.Internal.comma
                                                  Data.Aeson.Encoding.Internal.><
                                                    toEncoding arg4_al9C))))))
instance FromJSON TableBody where
  parseJSON
    = \ value_al9D
        -> case value_al9D of
             Array arr_al9E
               -> if (Data.Vector.length
                        arr_al9E
                        == 4) then
                      ((((TableBody
                            <$>
                              parseJSON
                                (arr_al9E
                                   `Data.Vector.unsafeIndex`
                                     0))
                           <*>
                             parseJSON
                               (arr_al9E
                                  `Data.Vector.unsafeIndex`
                                    1))
                          <*>
                            parseJSON
                              (arr_al9E
                                 `Data.Vector.unsafeIndex`
                                   2))
                         <*>
                           parseJSON
                             (arr_al9E
                                `Data.Vector.unsafeIndex`
                                  3))
                  else
                      (((Data.Aeson.TH.parseTypeMismatch' "TableBody")
                          "Text.Pandoc.Definition.TableBody")
                         "Array of length 4")
                        ("Array of length "
                           ++
                             (show
                                . Data.Vector.length)
                               arr_al9E)
             other_al9F
               -> (((Data.Aeson.TH.parseTypeMismatch' "TableBody")
                      "Text.Pandoc.Definition.TableBody")
                     "Array")
                    (Data.Aeson.TH.valueConName other_al9F)
instance ToJSON TableFoot where
  toJSON
    = let
      in
        \ value_al9G
          -> case value_al9G of
               TableFoot arg1_al9H arg2_al9I
                 -> Array
                      (Data.Vector.create
                         (do mv_al9J <- Data.Vector.Mutable.unsafeNew
                                          2
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9J)
                                0)
                               (toJSON arg1_al9H)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9J)
                                1)
                               (toJSON arg2_al9I)
                             return mv_al9J))
  toEncoding
    = let
      in
        \ value_al9K
          -> case value_al9K of
               TableFoot arg1_al9L arg2_al9M
                 -> Data.Aeson.Encoding.Internal.wrapArray
                      (toEncoding arg1_al9L
                         Data.Aeson.Encoding.Internal.><
                           (Data.Aeson.Encoding.Internal.comma
                              Data.Aeson.Encoding.Internal.>< toEncoding arg2_al9M))
instance FromJSON TableFoot where
  parseJSON
    = \ value_al9N
        -> case value_al9N of
             Array arr_al9O
               -> if (Data.Vector.length
                        arr_al9O
                        == 2) then
                      ((TableFoot
                          <$>
                            parseJSON
                              (arr_al9O
                                 `Data.Vector.unsafeIndex`
                                   0))
                         <*>
                           parseJSON
                             (arr_al9O
                                `Data.Vector.unsafeIndex`
                                  1))
                  else
                      (((Data.Aeson.TH.parseTypeMismatch' "TableFoot")
                          "Text.Pandoc.Definition.TableFoot")
                         "Array of length 2")
                        ("Array of length "
                           ++
                             (show
                                . Data.Vector.length)
                               arr_al9O)
             other_al9P
               -> (((Data.Aeson.TH.parseTypeMismatch' "TableFoot")
                      "Text.Pandoc.Definition.TableFoot")
                     "Array")
                    (Data.Aeson.TH.valueConName other_al9P)
instance ToJSON Cell where
  toJSON
    = let
      in
        \ value_al9Q
          -> case value_al9Q of
               Cell arg1_al9R arg2_al9S arg3_al9T arg4_al9U arg5_al9V
                 -> Array
                      (Data.Vector.create
                         (do mv_al9W <- Data.Vector.Mutable.unsafeNew
                                          5
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9W)
                                0)
                               (toJSON arg1_al9R)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9W)
                                1)
                               (toJSON arg2_al9S)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9W)
                                2)
                               (toJSON arg3_al9T)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9W)
                                3)
                               (toJSON arg4_al9U)
                             ((Data.Vector.Mutable.unsafeWrite
                                 mv_al9W)
                                4)
                               (toJSON arg5_al9V)
                             return mv_al9W))
  toEncoding
    = let
      in
        \ value_al9X
          -> case value_al9X of
               Cell arg1_al9Y arg2_al9Z arg3_ala0 arg4_ala1 arg5_ala2
                 -> Data.Aeson.Encoding.Internal.wrapArray
                      (toEncoding arg1_al9Y
                         Data.Aeson.Encoding.Internal.><
                           (Data.Aeson.Encoding.Internal.comma
                              Data.Aeson.Encoding.Internal.><
                                (toEncoding arg2_al9Z
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.><
                                          (toEncoding arg3_ala0
                                             Data.Aeson.Encoding.Internal.><
                                               (Data.Aeson.Encoding.Internal.comma
                                                  Data.Aeson.Encoding.Internal.><
                                                    (toEncoding arg4_ala1
                                                       Data.Aeson.Encoding.Internal.><
                                                         (Data.Aeson.Encoding.Internal.comma
                                                            Data.Aeson.Encoding.Internal.><
                                                              toEncoding arg5_ala2))))))))
instance FromJSON Cell where
  parseJSON
    = \ value_ala3
        -> case value_ala3 of
             Array arr_ala4
               -> if (Data.Vector.length
                        arr_ala4
                        == 5) then
                      (((((Cell
                             <$>
                               parseJSON
                                 (arr_ala4
                                    `Data.Vector.unsafeIndex`
                                      0))
                            <*>
                              parseJSON
                                (arr_ala4
                                   `Data.Vector.unsafeIndex`
                                     1))
                           <*>
                             parseJSON
                               (arr_ala4
                                  `Data.Vector.unsafeIndex`
                                    2))
                          <*>
                            parseJSON
                              (arr_ala4
                                 `Data.Vector.unsafeIndex`
                                   3))
                         <*>
                           parseJSON
                             (arr_ala4
                                `Data.Vector.unsafeIndex`
                                  4))
                  else
                      (((Data.Aeson.TH.parseTypeMismatch' "Cell")
                          "Text.Pandoc.Definition.Cell")
                         "Array of length 5")
                        ("Array of length "
                           ++
                             (show
                                . Data.Vector.length)
                               arr_ala4)
             other_ala5
               -> (((Data.Aeson.TH.parseTypeMismatch' "Cell")
                      "Text.Pandoc.Definition.Cell")
                     "Array")
                    (Data.Aeson.TH.valueConName other_ala5)
instance ToJSON Inline where
  toJSON
    = let
      in
        \ value_ala6
          -> case value_ala6 of
               Str arg1_ala7
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Str"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_ala7))
               Emph arg1_ala8
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Emph"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_ala8))
               Underline arg1_ala9
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Underline"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_ala9))
               Strong arg1_alaa
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Strong"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alaa))
               Strikeout arg1_alab
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Strikeout"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alab))
               Superscript arg1_alac
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Superscript"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alac))
               Subscript arg1_alad
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Subscript"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alad))
               SmallCaps arg1_alae
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "SmallCaps"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alae))
               Quoted arg1_alaf arg2_alag
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Quoted"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alah <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alah)
                                          0)
                                         (toJSON arg1_alaf)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alah)
                                          1)
                                         (toJSON arg2_alag)
                                       return mv_alah))))
               Cite arg1_alai arg2_alaj
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Cite"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alak <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alak)
                                          0)
                                         (toJSON arg1_alai)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alak)
                                          1)
                                         (toJSON arg2_alaj)
                                       return mv_alak))))
               Code arg1_alal arg2_alam
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Code"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alan <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alan)
                                          0)
                                         (toJSON arg1_alal)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alan)
                                          1)
                                         (toJSON arg2_alam)
                                       return mv_alan))))
               Space
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Space")))
               SoftBreak
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "SoftBreak")))
               LineBreak
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "LineBreak")))
               Math arg1_alao arg2_alap
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Math"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alaq <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alaq)
                                          0)
                                         (toJSON arg1_alao)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alaq)
                                          1)
                                         (toJSON arg2_alap)
                                       return mv_alaq))))
               RawInline arg1_alar arg2_alas
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "RawInline"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alat <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alat)
                                          0)
                                         (toJSON arg1_alar)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alat)
                                          1)
                                         (toJSON arg2_alas)
                                       return mv_alat))))
               Link arg1_alau arg2_alav arg3_alaw
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Link"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alax <- Data.Vector.Mutable.unsafeNew
                                                    3
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alax)
                                          0)
                                         (toJSON arg1_alau)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alax)
                                          1)
                                         (toJSON arg2_alav)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alax)
                                          2)
                                         (toJSON arg3_alaw)
                                       return mv_alax))))
               Image arg1_alay arg2_alaz arg3_alaA
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Image"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alaB <- Data.Vector.Mutable.unsafeNew
                                                    3
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alaB)
                                          0)
                                         (toJSON arg1_alay)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alaB)
                                          1)
                                         (toJSON arg2_alaz)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alaB)
                                          2)
                                         (toJSON arg3_alaA)
                                       return mv_alaB))))
               Note arg1_alaC
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Note"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alaC))
               Span arg1_alaD arg2_alaE
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Span"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alaF <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alaF)
                                          0)
                                         (toJSON arg1_alaD)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alaF)
                                          1)
                                         (toJSON arg2_alaE)
                                       return mv_alaF))))
  toEncoding
    = let
        _let1_alaJ
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"c\":"#
        _let0_alaI
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_alaG
          -> case value_alaG of
               Str arg1_alaH
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Str"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaH))
               Emph arg1_alaK
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Emph"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaK))
               Underline arg1_alaL
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Underline"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaL))
               Strong arg1_alaM
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Strong"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaM))
               Strikeout arg1_alaN
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Strikeout"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaN))
               Superscript arg1_alaO
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Superscript"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaO))
               Subscript arg1_alaP
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Subscript"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaP))
               SmallCaps arg1_alaQ
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "SmallCaps"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alaQ))
               Quoted arg1_alaR arg2_alaS
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Quoted"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alaR
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alaS))))
               Cite arg1_alaT arg2_alaU
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Cite"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alaT
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alaU))))
               Code arg1_alaV arg2_alaW
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Code"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alaV
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alaW))))
               Space
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Space")))
               SoftBreak
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "SoftBreak")))
               LineBreak
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "LineBreak")))
               Math arg1_alaX arg2_alaY
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Math"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alaX
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alaY))))
               RawInline arg1_alaZ arg2_alb0
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "RawInline"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alaZ
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alb0))))
               Link arg1_alb1 arg2_alb2 arg3_alb3
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Link"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alb1
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.><
                                          (toEncoding arg2_alb2
                                             Data.Aeson.Encoding.Internal.><
                                               (Data.Aeson.Encoding.Internal.comma
                                                  Data.Aeson.Encoding.Internal.><
                                                    toEncoding arg3_alb3))))))
               Image arg1_alb4 arg2_alb5 arg3_alb6
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Image"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alb4
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.><
                                          (toEncoding arg2_alb5
                                             Data.Aeson.Encoding.Internal.><
                                               (Data.Aeson.Encoding.Internal.comma
                                                  Data.Aeson.Encoding.Internal.><
                                                    toEncoding arg3_alb6))))))
               Note arg1_alb7
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Note"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (toEncoding arg1_alb7))
               Span arg1_alb8 arg2_alb9
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alaI)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Span"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alaJ)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alb8
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alb9))))
instance FromJSON Inline where
  parseJSON
    = \ value_alba
        -> case value_alba of
             Object obj_albb
               -> do conKeyX_albc <- (obj_albb .: Data.Aeson.Key.fromString "t")
                     case conKeyX_albc of
                       _ | (conKeyX_albc == Data.Aeson.Key.fromString "Str")
                         -> do val_albd <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albd of arg_albe -> (Str <$> parseJSON arg_albe)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Emph")
                         -> do val_albf <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albf of arg_albg -> (Emph <$> parseJSON arg_albg)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Underline")
                         -> do val_albh <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albh of arg_albi -> (Underline <$> parseJSON arg_albi)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Strong")
                         -> do val_albj <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albj of arg_albk -> (Strong <$> parseJSON arg_albk)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Strikeout")
                         -> do val_albl <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albl of arg_albm -> (Strikeout <$> parseJSON arg_albm)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Superscript")
                         -> do val_albn <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albn of arg_albo -> (Superscript <$> parseJSON arg_albo)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Subscript")
                         -> do val_albp <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albp of arg_albq -> (Subscript <$> parseJSON arg_albq)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "SmallCaps")
                         -> do val_albr <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albr of arg_albs -> (SmallCaps <$> parseJSON arg_albs)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Quoted")
                         -> do val_albt <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albt of
                                 Array arr_albu
                                   -> if (Data.Vector.length
                                            arr_albu
                                            == 2) then
                                          ((Quoted
                                              <$>
                                                parseJSON
                                                  (arr_albu
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_albu
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Quoted")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albu)
                                 other_albv
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Quoted")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_albv)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Cite")
                         -> do val_albw <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albw of
                                 Array arr_albx
                                   -> if (Data.Vector.length
                                            arr_albx
                                            == 2) then
                                          ((Cite
                                              <$>
                                                parseJSON
                                                  (arr_albx
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_albx
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Cite")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albx)
                                 other_alby
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Cite")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_alby)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Code")
                         -> do val_albz <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albz of
                                 Array arr_albA
                                   -> if (Data.Vector.length
                                            arr_albA
                                            == 2) then
                                          ((Code
                                              <$>
                                                parseJSON
                                                  (arr_albA
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_albA
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Code")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albA)
                                 other_albB
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Code")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_albB)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Space") -> pure Space
                         | (conKeyX_albc == Data.Aeson.Key.fromString "SoftBreak")
                         -> pure SoftBreak
                         | (conKeyX_albc == Data.Aeson.Key.fromString "LineBreak")
                         -> pure LineBreak
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Math")
                         -> do val_albC <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albC of
                                 Array arr_albD
                                   -> if (Data.Vector.length
                                            arr_albD
                                            == 2) then
                                          ((Math
                                              <$>
                                                parseJSON
                                                  (arr_albD
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_albD
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Math")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albD)
                                 other_albE
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Math")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_albE)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "RawInline")
                         -> do val_albF <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albF of
                                 Array arr_albG
                                   -> if (Data.Vector.length
                                            arr_albG
                                            == 2) then
                                          ((RawInline
                                              <$>
                                                parseJSON
                                                  (arr_albG
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_albG
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "RawInline")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albG)
                                 other_albH
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "RawInline")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_albH)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Link")
                         -> do val_albI <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albI of
                                 Array arr_albJ
                                   -> if (Data.Vector.length
                                            arr_albJ
                                            == 3) then
                                          (((Link
                                               <$>
                                                 parseJSON
                                                   (arr_albJ
                                                      `Data.Vector.unsafeIndex`
                                                        0))
                                              <*>
                                                parseJSON
                                                  (arr_albJ
                                                     `Data.Vector.unsafeIndex`
                                                       1))
                                             <*>
                                               parseJSON
                                                 (arr_albJ
                                                    `Data.Vector.unsafeIndex`
                                                      2))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Link")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 3")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albJ)
                                 other_albK
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Link")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_albK)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Image")
                         -> do val_albL <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albL of
                                 Array arr_albM
                                   -> if (Data.Vector.length
                                            arr_albM
                                            == 3) then
                                          (((Image
                                               <$>
                                                 parseJSON
                                                   (arr_albM
                                                      `Data.Vector.unsafeIndex`
                                                        0))
                                              <*>
                                                parseJSON
                                                  (arr_albM
                                                     `Data.Vector.unsafeIndex`
                                                       1))
                                             <*>
                                               parseJSON
                                                 (arr_albM
                                                    `Data.Vector.unsafeIndex`
                                                      2))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Image")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 3")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albM)
                                 other_albN
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Image")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_albN)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Note")
                         -> do val_albO <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albO of arg_albP -> (Note <$> parseJSON arg_albP)
                         | (conKeyX_albc == Data.Aeson.Key.fromString "Span")
                         -> do val_albQ <- (obj_albb .: Data.Aeson.Key.fromString "c")
                               case val_albQ of
                                 Array arr_albR
                                   -> if (Data.Vector.length
                                            arr_albR
                                            == 2) then
                                          ((Span
                                              <$>
                                                parseJSON
                                                  (arr_albR
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_albR
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Span")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_albR)
                                 other_albS
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Span")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_albS)
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.Inline")
                               ["Str", "Emph", "Underline", "Strong", "Strikeout",
                                "Superscript", "Subscript", "SmallCaps", "Quoted", "Cite",
                                "Code", "Space", "SoftBreak", "LineBreak", "Math", "RawInline",
                                "Link", "Image", "Note", "Span"])
                              (Data.Aeson.Key.toString conKeyX_albc)
             other_albT
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.Inline")
                    (Data.Aeson.TH.valueConName other_albT)
instance ToJSON Block where
  toJSON
    = let
      in
        \ value_albU
          -> case value_albU of
               Plain arg1_albV
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Plain"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_albV))
               Para arg1_albW
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Para"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_albW))
               LineBlock arg1_albX
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "LineBlock"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_albX))
               CodeBlock arg1_albY arg2_albZ
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "CodeBlock"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alc0 <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alc0)
                                          0)
                                         (toJSON arg1_albY)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alc0)
                                          1)
                                         (toJSON arg2_albZ)
                                       return mv_alc0))))
               RawBlock arg1_alc1 arg2_alc2
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "RawBlock"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alc3 <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alc3)
                                          0)
                                         (toJSON arg1_alc1)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alc3)
                                          1)
                                         (toJSON arg2_alc2)
                                       return mv_alc3))))
               BlockQuote arg1_alc4
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "BlockQuote"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alc4))
               OrderedList arg1_alc5 arg2_alc6
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "OrderedList"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alc7 <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alc7)
                                          0)
                                         (toJSON arg1_alc5)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alc7)
                                          1)
                                         (toJSON arg2_alc6)
                                       return mv_alc7))))
               BulletList arg1_alc8
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "BulletList"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alc8))
               DefinitionList arg1_alc9
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "DefinitionList"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (toJSON arg1_alc9))
               Header arg1_alca arg2_alcb arg3_alcc
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Header"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alcd <- Data.Vector.Mutable.unsafeNew
                                                    3
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alcd)
                                          0)
                                         (toJSON arg1_alca)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alcd)
                                          1)
                                         (toJSON arg2_alcb)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alcd)
                                          2)
                                         (toJSON arg3_alcc)
                                       return mv_alcd))))
               HorizontalRule
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "HorizontalRule")))
               Table arg1_alce arg2_alcf arg3_alcg arg4_alch arg5_alci arg6_alcj
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Table"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alck <- Data.Vector.Mutable.unsafeNew
                                                    6
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alck)
                                          0)
                                         (toJSON arg1_alce)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alck)
                                          1)
                                         (toJSON arg2_alcf)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alck)
                                          2)
                                         (toJSON arg3_alcg)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alck)
                                          3)
                                         (toJSON arg4_alch)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alck)
                                          4)
                                         (toJSON arg5_alci)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alck)
                                          5)
                                         (toJSON arg6_alcj)
                                       return mv_alck))))
               Figure arg1_alcl arg2_alcm arg3_alcn
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Figure"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alco <- Data.Vector.Mutable.unsafeNew
                                                    3
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alco)
                                          0)
                                         (toJSON arg1_alcl)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alco)
                                          1)
                                         (toJSON arg2_alcm)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alco)
                                          2)
                                         (toJSON arg3_alcn)
                                       return mv_alco))))
               Div arg1_alcp arg2_alcq
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "t"))
                         (String (T.pack "Div"))
                         <>
                           (Data.Aeson.Types.ToJSON.pair (Data.Aeson.Key.fromString "c"))
                             (Array
                                (Data.Vector.create
                                   (do mv_alcr <- Data.Vector.Mutable.unsafeNew
                                                    2
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alcr)
                                          0)
                                         (toJSON arg1_alcp)
                                       ((Data.Vector.Mutable.unsafeWrite
                                           mv_alcr)
                                          1)
                                         (toJSON arg2_alcq)
                                       return mv_alcr))))
  toEncoding
    = let
        _let1_alcv
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"c\":"#
        _let0_alcu
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral 4) "\"t\":"#
      in
        \ value_alcs
          -> case value_alcs of
               Plain arg1_alct
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Plain"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (toEncoding arg1_alct))
               Para arg1_alcw
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Para"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (toEncoding arg1_alcw))
               LineBlock arg1_alcx
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "LineBlock"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (toEncoding arg1_alcx))
               CodeBlock arg1_alcy arg2_alcz
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "CodeBlock"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alcy
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alcz))))
               RawBlock arg1_alcA arg2_alcB
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "RawBlock"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alcA
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alcB))))
               BlockQuote arg1_alcC
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "BlockQuote"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (toEncoding arg1_alcC))
               OrderedList arg1_alcD arg2_alcE
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "OrderedList"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alcD
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alcE))))
               BulletList arg1_alcF
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "BulletList"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (toEncoding arg1_alcF))
               DefinitionList arg1_alcG
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "DefinitionList"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (toEncoding arg1_alcG))
               Header arg1_alcH arg2_alcI arg3_alcJ
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Header"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alcH
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.><
                                          (toEncoding arg2_alcI
                                             Data.Aeson.Encoding.Internal.><
                                               (Data.Aeson.Encoding.Internal.comma
                                                  Data.Aeson.Encoding.Internal.><
                                                    toEncoding arg3_alcJ))))))
               HorizontalRule
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "HorizontalRule")))
               Table arg1_alcK arg2_alcL arg3_alcM arg4_alcN arg5_alcO arg6_alcP
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Table"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alcK
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.><
                                          (toEncoding arg2_alcL
                                             Data.Aeson.Encoding.Internal.><
                                               (Data.Aeson.Encoding.Internal.comma
                                                  Data.Aeson.Encoding.Internal.><
                                                    (toEncoding arg3_alcM
                                                       Data.Aeson.Encoding.Internal.><
                                                         (Data.Aeson.Encoding.Internal.comma
                                                            Data.Aeson.Encoding.Internal.><
                                                              (toEncoding arg4_alcN
                                                                 Data.Aeson.Encoding.Internal.><
                                                                   (Data.Aeson.Encoding.Internal.comma
                                                                      Data.Aeson.Encoding.Internal.><
                                                                        (toEncoding arg5_alcO
                                                                           Data.Aeson.Encoding.Internal.><
                                                                             (Data.Aeson.Encoding.Internal.comma
                                                                                Data.Aeson.Encoding.Internal.><
                                                                                  toEncoding
                                                                                    arg6_alcP))))))))))))
               Figure arg1_alcQ arg2_alcR arg3_alcS
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Figure"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alcQ
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.><
                                          (toEncoding arg2_alcR
                                             Data.Aeson.Encoding.Internal.><
                                               (Data.Aeson.Encoding.Internal.comma
                                                  Data.Aeson.Encoding.Internal.><
                                                    toEncoding arg3_alcS))))))
               Div arg1_alcT arg2_alcU
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_alcu)
                         (Data.Aeson.Encoding.Internal.text (T.pack "Div"))
                         <>
                           (Data.Aeson.Encoding.Internal.unsafePairSBS _let1_alcv)
                             (Data.Aeson.Encoding.Internal.wrapArray
                                (toEncoding arg1_alcT
                                   Data.Aeson.Encoding.Internal.><
                                     (Data.Aeson.Encoding.Internal.comma
                                        Data.Aeson.Encoding.Internal.>< toEncoding arg2_alcU))))
instance FromJSON Block where
  parseJSON
    = \ value_alcV
        -> case value_alcV of
             Object obj_alcW
               -> do conKeyX_alcX <- (obj_alcW .: Data.Aeson.Key.fromString "t")
                     case conKeyX_alcX of
                       _ | (conKeyX_alcX == Data.Aeson.Key.fromString "Plain")
                         -> do val_alcY <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_alcY of arg_alcZ -> (Plain <$> parseJSON arg_alcZ)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "Para")
                         -> do val_ald0 <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_ald0 of arg_ald1 -> (Para <$> parseJSON arg_ald1)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "LineBlock")
                         -> do val_ald2 <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_ald2 of arg_ald3 -> (LineBlock <$> parseJSON arg_ald3)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "CodeBlock")
                         -> do val_ald4 <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_ald4 of
                                 Array arr_ald5
                                   -> if (Data.Vector.length
                                            arr_ald5
                                            == 2) then
                                          ((CodeBlock
                                              <$>
                                                parseJSON
                                                  (arr_ald5
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_ald5
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "CodeBlock")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_ald5)
                                 other_ald6
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "CodeBlock")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_ald6)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "RawBlock")
                         -> do val_ald7 <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_ald7 of
                                 Array arr_ald8
                                   -> if (Data.Vector.length
                                            arr_ald8
                                            == 2) then
                                          ((RawBlock
                                              <$>
                                                parseJSON
                                                  (arr_ald8
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_ald8
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "RawBlock")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_ald8)
                                 other_ald9
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "RawBlock")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_ald9)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "BlockQuote")
                         -> do val_alda <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_alda of arg_aldb -> (BlockQuote <$> parseJSON arg_aldb)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "OrderedList")
                         -> do val_aldc <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_aldc of
                                 Array arr_aldd
                                   -> if (Data.Vector.length
                                            arr_aldd
                                            == 2) then
                                          ((OrderedList
                                              <$>
                                                parseJSON
                                                  (arr_aldd
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_aldd
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "OrderedList")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_aldd)
                                 other_alde
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "OrderedList")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_alde)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "BulletList")
                         -> do val_aldf <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_aldf of arg_aldg -> (BulletList <$> parseJSON arg_aldg)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "DefinitionList")
                         -> do val_aldh <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_aldh of
                                 arg_aldi -> (DefinitionList <$> parseJSON arg_aldi)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "Header")
                         -> do val_aldj <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_aldj of
                                 Array arr_aldk
                                   -> if (Data.Vector.length
                                            arr_aldk
                                            == 3) then
                                          (((Header
                                               <$>
                                                 parseJSON
                                                   (arr_aldk
                                                      `Data.Vector.unsafeIndex`
                                                        0))
                                              <*>
                                                parseJSON
                                                  (arr_aldk
                                                     `Data.Vector.unsafeIndex`
                                                       1))
                                             <*>
                                               parseJSON
                                                 (arr_aldk
                                                    `Data.Vector.unsafeIndex`
                                                      2))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Header")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 3")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_aldk)
                                 other_aldl
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Header")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_aldl)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "HorizontalRule")
                         -> pure HorizontalRule
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "Table")
                         -> do val_aldm <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_aldm of
                                 Array arr_aldn
                                   -> if (Data.Vector.length
                                            arr_aldn
                                            == 6) then
                                          ((((((Table
                                                  <$>
                                                    parseJSON
                                                      (arr_aldn
                                                         `Data.Vector.unsafeIndex`
                                                           0))
                                                 <*>
                                                   parseJSON
                                                     (arr_aldn
                                                        `Data.Vector.unsafeIndex`
                                                          1))
                                                <*>
                                                  parseJSON
                                                    (arr_aldn
                                                       `Data.Vector.unsafeIndex`
                                                         2))
                                               <*>
                                                 parseJSON
                                                   (arr_aldn
                                                      `Data.Vector.unsafeIndex`
                                                        3))
                                              <*>
                                                parseJSON
                                                  (arr_aldn
                                                     `Data.Vector.unsafeIndex`
                                                       4))
                                             <*>
                                               parseJSON
                                                 (arr_aldn
                                                    `Data.Vector.unsafeIndex`
                                                      5))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Table")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 6")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_aldn)
                                 other_aldo
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Table")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_aldo)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "Figure")
                         -> do val_aldp <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_aldp of
                                 Array arr_aldq
                                   -> if (Data.Vector.length
                                            arr_aldq
                                            == 3) then
                                          (((Figure
                                               <$>
                                                 parseJSON
                                                   (arr_aldq
                                                      `Data.Vector.unsafeIndex`
                                                        0))
                                              <*>
                                                parseJSON
                                                  (arr_aldq
                                                     `Data.Vector.unsafeIndex`
                                                       1))
                                             <*>
                                               parseJSON
                                                 (arr_aldq
                                                    `Data.Vector.unsafeIndex`
                                                      2))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Figure")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 3")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_aldq)
                                 other_aldr
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Figure")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_aldr)
                         | (conKeyX_alcX == Data.Aeson.Key.fromString "Div")
                         -> do val_alds <- (obj_alcW .: Data.Aeson.Key.fromString "c")
                               case val_alds of
                                 Array arr_aldt
                                   -> if (Data.Vector.length
                                            arr_aldt
                                            == 2) then
                                          ((Div
                                              <$>
                                                parseJSON
                                                  (arr_aldt
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_aldt
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((Data.Aeson.TH.parseTypeMismatch' "Div")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show
                                                    . Data.Vector.length)
                                                   arr_aldt)
                                 other_aldu
                                   -> (((Data.Aeson.TH.parseTypeMismatch' "Div")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (Data.Aeson.TH.valueConName other_aldu)
                         | otherwise
                         -> ((Data.Aeson.TH.conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.Block")
                               ["Plain", "Para", "LineBlock", "CodeBlock", "RawBlock",
                                "BlockQuote", "OrderedList", "BulletList", "DefinitionList",
                                "Header", "HorizontalRule", "Table", "Figure", "Div"])
                              (Data.Aeson.Key.toString conKeyX_alcX)
             other_aldv
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.Block")
                    (Data.Aeson.TH.valueConName other_aldv)


instance FromJSON Meta where
  parseJSON = fmap Meta . parseJSON
instance ToJSON Meta where
  toJSON (Meta m) = toJSON m
  toEncoding (Meta m) = toEncoding m

instance FromJSON Pandoc where
  parseJSON (Object v) = do
    mbJVersion <- v .:? "pandoc-api-version" :: Aeson.Parser (Maybe [Int])
    case mbJVersion of
      Just jVersion  | x : y : _ <- jVersion
                     , x' : y' : _ <- versionBranch pandocTypesVersion
                     , x == x'
                     , y == y' -> Pandoc <$> v .: "meta" <*> v .: "blocks"
                     | otherwise ->
                         fail $ mconcat [ "Incompatible API versions: "
                                        , "encoded with "
                                        , show jVersion
                                        , " but attempted to decode with "
                                        , show $ versionBranch pandocTypesVersion
                                        , "."
                                        ]
      _ -> fail "JSON missing pandoc-api-version."
  parseJSON _ = mempty
instance ToJSON Pandoc where
  toJSON (Pandoc meta blks) =
    object [ "pandoc-api-version" .= versionBranch pandocTypesVersion
           , "meta"               .= meta
           , "blocks"             .= blks
           ]
  toEncoding (Pandoc meta blks) =
    pairs $ mconcat [ "pandoc-api-version" .= versionBranch pandocTypesVersion
                    , "meta"               .= meta
                    , "blocks"             .= blks
                    ]

-- Instances for deepseq
instance NFData MetaValue
instance NFData Meta
instance NFData Citation
instance NFData Alignment
instance NFData RowSpan
instance NFData ColSpan
instance NFData Cell
instance NFData Row
instance NFData TableHead
instance NFData TableBody
instance NFData TableFoot
instance NFData Caption
instance NFData Inline
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData ColWidth
instance NFData RowHeadColumns
instance NFData Block
instance NFData Pandoc

pandocTypesVersion :: Version
pandocTypesVersion = version
