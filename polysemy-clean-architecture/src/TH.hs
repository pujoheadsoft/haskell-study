{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad

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

-- genStub :: Name -> Q [Dec]
-- genStub fName = do
--     -- Get the type of the function
--     VarI _ fType _ <- reify fName

--     -- Extract argument types and return type
--     let (argTypes, returnType) = splitFunctionType fType

--     -- Generate variable names for the arguments
--     argNames <- replicateM (length argTypes) (newName "arg")
--     yName <- newName "y"

--     -- Generate the check for each argument
--     let checks = zipWith (\argName argType -> [| if $(varE argName) == $(varE yName) then return () else error ("Arguments do not match: expected " ++ show $(varE argName) ++ " but got " ++ show $(varE yName))|]) argNames argTypes

--     -- Combine all checks
--     let combinedCheck = foldl (\acc check -> [| $acc >> $check |]) [| return () |] checks

--     -- Generate the body of the stub function
--     let body = normalB [| \ $(varP yName) -> $(combinedCheck) >> $(varE fName) $(listE (map varE argNames)) |]

--     -- Create the stub function declaration
--     pure $ funD (mkName "stub") [clause (map varP (fName : argNames)) body []]

-- Helper function to split a function type into argument types and return type
splitFunctionType :: Type -> ([Type], Type)
splitFunctionType (AppT (AppT ArrowT argType) restType) =
    let (argTypes, returnType) = splitFunctionType restType
    in (argType : argTypes, returnType)
splitFunctionType returnType = ([], returnType)

-- stub :: Name -> Q [Dec]
-- stub name = do
--   info <- reify name
--   let x = show info
--   y <- funD (mkName "generatedFunction") [clause [] (normalB [| \s -> x |]) []]
--   return [y]

-- stub2 :: Name -> Q Exp
-- stub2 name = do
--   info <- reify name
--   let x = show info
--   [|\s -> x|]

stub3 :: Name -> Q [Dec]
stub3 name = do
  VarI _ fType _ <- reify name
  let (argTypes, returnType) = splitFunctionType fType
  argNames <- replicateM (length argTypes) (newName "arg")
  let x = show argNames
  let body = normalB [| $(varE name) $(listE (map varE argNames)) |]
  z <- funD (mkName "stub") [clause (map varP argNames) body []]
  return [z]

-- 関数の呼び出しを記録するためのラッパーを生成するTH関数
recordFunctionCall :: Name -> Q [Dec]
recordFunctionCall funcName = do
  -- 元の関数の情報を取得
  VarI _ funcType  _ <- reify funcName
  let funcString = nameBase funcName

  -- 新しい関数名を生成
  let newFuncName = mkName ("mock_" ++ funcString)

  -- 関数の引数と本体を生成
  (args, body) <- generateFunctionBody funcName funcType

  let sig = SigD newFuncName funcType

  -- 新しい関数の定義を生成
  let newFuncDec = FunD newFuncName [Clause args (NormalB body) []]

  return [sig, newFuncDec]

-- 関数の引数と本体を生成
generateFunctionBody :: Name -> Type -> Q ([Pat], Exp)
generateFunctionBody funcName (AppT (AppT ArrowT argType) rest) = do
  -- 引数のパターンと名前を生成
  argName <- newName "arg"
  let argPat = VarP argName
  let argExp = VarE argName


  -- 再帰的に残りの引数と本体を生成
  (args, body) <- generateFunctionBody funcName rest

  -- 引数を記録するコードを生成
  --let recordCode = AppE (VarE 'modifyIORef) (AppE (VarE 'callHistory) (LamE [WildP] (AppE (AppE (VarE '(:)) (TupE [Just (LitE (StringL (nameBase funcName))), Just (ListE [AppE (VarE 'show) argExp])])) (VarE '[]))))

  -- 新しい関数の本体を生成
  --let newBody = DoE [NoBindS recordCode, NoBindS (AppE (AppE (VarE '($)) (VarE funcName)) argExp)]
  --let newBody = DoE Nothing [NoBindS (AppE (AppE (VarE '($)) (VarE funcName)) argExp)]

  return (argPat : args, body)
generateFunctionBody _ resultType = do
  resultName <- newName "result"
  let resultPat = VarP resultName
  return ([], VarE resultName)

{-
genedFn_a5uI arg1_a5uJ arg2_a5uK = (arg1_a5uJ ++ arg2_a5uK)
-}
genFn :: Name -> Q [Dec]
genFn n = do
  name <- newName "genedFn" -- 関数名
  a1 <- newName "arg1" -- 引数名1
  a2 <- newName "arg2" -- 引数名2
  arg1 <- varP a1 -- パターン(単純な変数1)
  arg2 <- varP a2 -- パターン(単純な変数2)
  body <- normalB [|$(varE a1) ++ $(varE a2)|] -- 関数本体(ExpじゃないといけないのでvarE a1とかしてる)
  let f = FunD name [Clause [arg1, arg2] body []] -- 関数の定義
  return [f] -- 返す(リストじゃないといけない)

genFnWithSig :: Name -> Q [Dec]
genFnWithSig n = do
  name <- newName "genedFnWithSig" -- 関数名
  a1 <- newName "arg1" -- 引数名1
  a2 <- newName "arg2" -- 引数名2
  arg1 <- varP a1 -- パターン(単純な変数1)
  arg2 <- varP a2 -- パターン(単純な変数2)
  body <- normalB [|$(varE a1) <> $(varE a2)|] -- 関数本体(ExpじゃないといけないのでvarE a1とかしてる)
  let f = FunD name [Clause [arg1, arg2] body []] -- 関数の定義

  let typeA = varT (mkName "a") -- type
  -- シグニチャのtype
  signatureType <- [t|$(conT $ mkName "Monoid") $typeA => $typeA -> $typeA -> $typeA|]
  let sig = SigD name signatureType -- シグニチャを作る

  return [sig, f] -- 返す(リストじゃないといけない)

{-
  mock = 1 :> 2 :> 100
-}