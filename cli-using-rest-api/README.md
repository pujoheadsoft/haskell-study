# CLIのサンプル
実行すると REST Api (JSON Placeholder) から指定したユーザーIDに関連する投稿および投稿へのコメントを取得し、
指定したパスへJSONとして書き出す。

## 実行方法
`stack exec -- cli -u 1 -o test.json`

以下でヘルプが見られる。
`stack exec -- cli`

## テスト
`stack test`

## 設計
いわゆる Three Layer Haskell Cake となっている。  
以下の層が存在する。
- ドメイン層
- インフラ層
- アプリケーション層