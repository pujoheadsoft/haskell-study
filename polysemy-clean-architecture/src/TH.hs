{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- stub :: Quote m => Name -> m Exp
-- stub n = do 
--   info <- reify n
--   show info


-- 指定された名前の関数の引数とその型を取得
getFunctionArgsAndTypes :: Name -> Q [(String, Type)]
getFunctionArgsAndTypes funcName = do
  -- 関数の情報を取得
  VarI _ funcType _  <- reify funcName
  -- 型を解析して引数の情報を抽出
  return $ getArgs funcType

-- 関数の型を解析して引数の情報を抽出するヘルパー関数
getArgs :: Type -> [(String, Type)]
getArgs (AppT (AppT ArrowT argType) rest) = ("arg", argType) : getArgs rest
getArgs resultType = [("result", resultType)]

functionTypeToList :: Type -> [Type]
functionTypeToList type' =
  case type' of
    AppT (AppT ArrowT t1) t2 ->
      t1: functionTypeToList t2

    SigT t _ ->
      functionTypeToList t

    ForallT _ _ t ->
      functionTypeToList t

    t ->
      [t]