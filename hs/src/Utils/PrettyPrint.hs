module Utils.PrettyPrint where

import Data.Semigroup (Semigroup(..))

sep :: String
sep = " "

emptyLine :: String
emptyLine = "\n"

quoted :: String -> String
quoted = enclosed "\""

enclosed :: String -> String -> String
enclosed bracket str = menclosed bracket str

menclosed :: (Semigroup m) => m -> m -> m
menclosed bracket x = bracket <> x <> bracket

(+--+) :: String -> String -> String
prefix +--+ suffix = prefix ++ sep ++ suffix

(+\\+) :: String -> String -> String
firstLine +\\+ secondLine = firstLine ++ "\n" ++ secondLine

(%%) :: (Show s) => String -> s -> String
pre %% o = pre ++ (show o)

commaSeparated :: (Foldable t) => t String -> String
commaSeparated = concatWithSep ", "

showAll :: (Functor f, Show s) => f s -> f String
showAll = fmap show

indent :: String -> String
indent text = properUnlines $ map (\l -> "  " ++ l) (lines text)

properUnlines :: [String] -> String
properUnlines = concatWithSep "\n"

concatWithSep :: (Foldable t) => String -> t String -> String
concatWithSep = mconcatWithSep

mconcatWithSep :: (Monoid m, Semigroup m, Foldable t) => m -> t m -> m
mconcatWithSep msep ms
  | (null ms) = mempty
  | otherwise = foldr1 (\prefix -> \suffix -> prefix <> msep <> suffix) ms

formatStringList :: String -> String -> String -> [String] -> String
formatStringList = mconcatWithStartSepEnd

mconcatWithStartSepEnd :: (Monoid m, Semigroup m, Foldable t) => m -> m -> m -> t m -> m
mconcatWithStartSepEnd mstart msep mend ms = mstart <> (mconcatWithSep msep ms) <> mend
