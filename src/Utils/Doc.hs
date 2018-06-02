{-# LANGUAGE TemplateHaskell #-}
module Utils.Doc where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

gen :: String -> String -> Q [Dec]
gen name body = return $ [ SigD (mkName $ name ++ "Doc") (AppT (ConT $ mkName "Maybe") (ConT $ mkName "Text"))
                         , FunD (mkName $ name ++ "Doc") [Clause [] (NormalB $ AppE (ConE $ mkName "Just") (LitE $ StringL body)) []]
                         ]

gendecl :: String -> Q [Dec]
gendecl x = gen name body
    where split = lines x
          name  = head split
          body  = unlines $ tail split

genDoc :: QuasiQuoter
genDoc = QuasiQuoter (error "Cannot use q as a expr")
                     (error "Cannot use q as a pattern")
                     (error "Cannot use q as a type")
                     gendecl
