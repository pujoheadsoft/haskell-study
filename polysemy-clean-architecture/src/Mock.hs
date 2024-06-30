{-# LANGUAGE TemplateHaskell #-}
module Mock where

import Language.Haskell.TH

--stub :: Name -> String
stub n = runQ $ stringE . pprint =<< reify n

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
