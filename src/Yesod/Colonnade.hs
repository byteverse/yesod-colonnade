{- | Build HTML tables using @yesod@ and @colonnade@. To learn
  how to use this module, first read the documentation for @colonnade@,
  and then read the documentation for @blaze-colonnade@. This library
  and @blaze-colonnade@ are entirely distinct; neither depends on the
  other. However, the interfaces they expose are very similar, and
  the explanations provided counterpart are sufficient to understand
  this library.
-}
module Yesod.Colonnade
  ( -- * Build
    Cell (..)
  , cell
  , stringCell
  , textCell
  , builderCell
  , anchorCell
  , anchorWidget

    -- * Apply
  , encodeWidgetTable
  , encodeCellTable
  , encodeDefinitionTable
  , encodeListItems
  ) where

import Colonnade (Colonnade, Headed)
import qualified Colonnade.Encode as E
import Control.Monad
import Data.Foldable
import qualified Data.Semigroup as SG
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import Text.Blaze (Attribute, toValue)
import qualified Text.Blaze.Html5.Attributes as HA
import Yesod.Core
import Yesod.Elements (a_, li_, table_, tbody_, td_, th_, thead_, tr_)

{- | The attributes that will be applied to a @<td>@ and
  the HTML content that will go inside it.
-}
data Cell site = Cell
  { cellAttrs :: [Attribute]
  , cellContents :: !(WidgetFor site ())
  }

instance IsString (Cell site) where
  fromString = stringCell

instance Semigroup (Cell site) where
  Cell a1 c1 <> Cell a2 c2 = Cell (mappend a1 a2) (mappend c1 c2)
instance Monoid (Cell site) where
  mempty = Cell mempty mempty
  mappend = (SG.<>)

-- | Create a 'Cell' from a 'Widget'
cell :: WidgetFor site () -> Cell site
cell = Cell mempty

-- | Create a 'Cell' from a 'String'
stringCell :: String -> Cell site
stringCell = cell . fromString

-- | Create a 'Cell' from a 'Text'
textCell :: Text -> Cell site
textCell = cell . toWidget . toHtml

-- | Create a 'Cell' from a text builder
builderCell :: TBuilder.Builder -> Cell site
builderCell = cell . toWidget . toHtml . LText.toStrict . TBuilder.toLazyText

{- | Create a 'Cell' whose content is hyperlinked by wrapping
  it in an @\<a\>@.
-}
anchorCell ::
  -- | Route that will go in @href@ attribute
  (a -> Route site) ->
  -- | Content wrapped by @<a>@ tag
  (a -> WidgetFor site ()) ->
  -- | Value
  a ->
  Cell site
anchorCell getRoute getContent = cell . anchorWidget getRoute getContent

{- | Create a widget whose content is hyperlinked by wrapping
  it in an @\<a\>@.
-}
anchorWidget ::
  -- | Route that will go in @href@ attribute
  (a -> Route site) ->
  -- | Content wrapped by @<a>@ tag
  (a -> WidgetFor site ()) ->
  -- | Value
  a ->
  WidgetFor site ()
anchorWidget getRoute getContent a = do
  urlRender <- getUrlRender
  a_ [HA.href (toValue (urlRender (getRoute a)))] (getContent a)

{- | This determines the attributes that are added
  to the individual @li@s by concatenating the header\'s
  attributes with the data\'s attributes.
-}
encodeListItems ::
  -- | Wrapper for items, often @ul@
  (WidgetFor site () -> WidgetFor site ()) ->
  -- | Combines header with data
  (WidgetFor site () -> WidgetFor site () -> WidgetFor site ()) ->
  -- | How to encode data as a row
  Colonnade Headed a (Cell site) ->
  -- | The value to display
  a ->
  WidgetFor site ()
encodeListItems ulWrap combine enc =
  ulWrap
    . E.bothMonadic_
      enc
      ( \(Cell ha hc) (Cell ba bc) ->
          li_ (ha <> ba) (combine hc bc)
      )

{- | A two-column table with the header content displayed in the
  first column and the data displayed in the second column. Note
  that the generated HTML table does not have a @thead@.
-}
encodeDefinitionTable ::
  -- | Attributes of @table@ element.
  [Attribute] ->
  -- | How to encode data as a row
  Colonnade Headed a (Cell site) ->
  -- | The value to display
  a ->
  WidgetFor site ()
encodeDefinitionTable attrs enc a =
  table_ attrs $
    tbody_ [] $
      E.bothMonadic_
        enc
        ( \theKey theValue -> tr_ [] $ do
            widgetFromCell td_ theKey
            widgetFromCell td_ theValue
        )
        a

{- | Encode an html table with attributes on the table cells.
  If you are using the bootstrap css framework, then you may want
  to call this with the first argument as:

  > encodeCellTable (HA.class_ "table table-striped") ...
-}
encodeCellTable ::
  (Foldable f, E.Headedness h) =>
  -- | Attributes of @table@ element
  [Attribute] ->
  -- | How to encode data as a row
  Colonnade h a (Cell site) ->
  -- | Rows of data
  f a ->
  WidgetFor site ()
encodeCellTable =
  encodeTable
    (E.headednessPure mempty)
    mempty
    (const mempty)
    widgetFromCell

-- | Encode an html table.
encodeWidgetTable ::
  (Foldable f, E.Headedness h) =>
  -- | Attributes of @\<table\>@ element
  [Attribute] ->
  -- | How to encode data as columns
  Colonnade h a (WidgetFor site ()) ->
  -- | Rows of data
  f a ->
  WidgetFor site ()
encodeWidgetTable =
  encodeTable
    (E.headednessPure mempty)
    mempty
    (const mempty)
    ($ mempty)

{- | Encode a table. This handles a very general case and
  is seldom needed by users. One of the arguments provided is
  used to add attributes to the generated @\<tr\>@ elements.
-}
encodeTable ::
  (Foldable f, E.Headedness h) =>
  -- | Attributes of @\<thead\>@
  h [Attribute] ->
  -- | Attributes of @\<tbody\>@ element
  [Attribute] ->
  -- | Attributes of each @\<tr\>@ element
  (a -> [Attribute]) ->
  -- | Wrap content and convert to 'Html'
  (([Attribute] -> WidgetFor site () -> WidgetFor site ()) -> c -> WidgetFor site ()) ->
  -- | Attributes of @\<table\>@ element
  [Attribute] ->
  -- | How to encode data as a row
  Colonnade h a c ->
  -- | Collection of data
  f a ->
  WidgetFor site ()
encodeTable theadAttrs tbodyAttrs trAttrs wrapContent tableAttrs colonnade xs =
  table_ tableAttrs $ do
    for_ E.headednessExtract $ \unhead ->
      thead_ (unhead theadAttrs) $ do
        E.headerMonadicGeneral_ colonnade (wrapContent th_)
    tbody_ tbodyAttrs $ do
      forM_ xs $ \x -> do
        tr_ (trAttrs x) (E.rowMonadic_ colonnade (wrapContent td_) x)

widgetFromCell ::
  ([Attribute] -> WidgetFor site () -> WidgetFor site ()) ->
  Cell site ->
  WidgetFor site ()
widgetFromCell f (Cell attrs contents) =
  f attrs contents
