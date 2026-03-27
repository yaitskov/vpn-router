module TypedCss (CssClass, css, class_) where

import Data.CSS.Syntax.Tokens ( tokenize, Token(Ident, Delim) )
import Data.Char
    ( isAlpha, isAlphaNum, isUpper, toLower, toUpper )
import Data.Set ( insert, Set, toList )
import Data.String ( IsString )
import Data.Text ( Text, pack, unpack )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax
    ( mkName,
      Exp(LitE, AppE, ConE),
      Clause(Clause),
      Type(VarT, ForallT, AppT, ConT),
      Dec(PragmaD, SigD, FunD),
      Body(NormalB),
      Inline(Inline),
      Lit(StringL),
      Phases(AllPhases),
      Pragma(InlineP),
      RuleMatch(FunLike),
      Specificity(InferredSpec),
      TyVarBndr(PlainTV) )

import Prelude

newtype CssClass s = CssClass { unCssClass :: s } deriving (Show, Eq, Ord)

instance Semigroup (CssClass Text) where
  CssClass l <> CssClass r = CssClass $ l <> " " <> r

class_ ::  CssClass Text -> Text
class_ = unCssClass

css :: QuasiQuoter
css = QuasiQuoter
  { quoteExp  = \_ -> fail "quoteExp: not implemented"
  , quotePat  = \_ -> fail "quotePat: not implemented"
  , quoteType = \_ -> fail "quoteType: not implemented"
  , quoteDec  = pure . cssToDecs
  }

{-
Delim '.',Ident "skeleton-block",Colon,Function "not",Colon,Ident "last-child",RightParen
-}
collectReferedClasses :: Set (CssClass Text) -> [Token] -> Set (CssClass Text)
collectReferedClasses s = \case
  Delim '.' : Ident cn : t ->
    collectReferedClasses (insert (CssClass cn) s) t
  _ : t -> collectReferedClasses s t
  [] -> s

{- | generate definition like:

@@
  {-# INLINE foo #-}
  foo :: IsString s => CssClass s
  foo = "foo"
@@

-}
cssClassConstDec :: CssClass Text -> [Dec]
cssClassConstDec (CssClass cn) =
  [ PragmaD (InlineP n Inline FunLike AllPhases)
  , SigD n (AppT (ConT ''CssClass) (ConT ''Text))
  , FunD n [ Clause [] body [] ]
  ]
  where
    ns = unpack cn
    n = mkName $ escapeIden ns
    body = NormalB (AppE (ConE 'CssClass) (LitE (StringL ns)))

cssToDecs :: String -> [Dec]
cssToDecs s = go $ pack s
  where
    go = (cssAsLiteralTextD s <>) . concatMap cssClassConstDec . toList . collectReferedClasses mempty . tokenize

{- | generate definition like:
@@
  {-# INLINE cssAsLiteralText #-}
  cssAsLiteralText :: IsString s => s
  cssAsLiteralText = s
@@
-}
cssAsLiteralTextD :: String -> [Dec]
cssAsLiteralTextD s =
  [ SigD n
    (ForallT
      [PlainTV st InferredSpec]
      [AppT (ConT ''IsString) (VarT st)]
      (VarT st))
  , FunD n [ Clause [] body [] ]
  , PragmaD (InlineP n Inline FunLike AllPhases)
  ]
  where
    st = mkName "s"
    n = mkName "cssAsLiteralText"
    body = NormalB (LitE (StringL s))

escapeIden :: String -> String
escapeIden s =
  case escapeIdenChar <$> hyphensToCamelCase s of
    s'@(fl:ol)
      | not (isAlpha fl) -> '_' : s'
      | isUpper fl -> toLower fl : ol
      | otherwise -> s'
    [] -> []

escapeIdenChar :: Char -> Char
escapeIdenChar c
  | isAlphaNum c || c == '_' = c
  | otherwise = '_'

hyphensToCamelCase :: String -> String
hyphensToCamelCase = concatMap ucFirst . splitOn '-'

ucFirst :: String -> String
ucFirst "" = ""
ucFirst (h:t) = toUpper h : t

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
  where
    go [] acc = [reverse acc]
    go (y : ys) acc =
      if x == y
      then reverse acc : go ys []
      else go ys (y : acc)
