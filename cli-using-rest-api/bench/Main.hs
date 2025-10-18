import Criterion.Main (defaultMain, bgroup, bench, nfIO) -- メイン関数とベンチマーク定義
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM)
import Control.Concurrent (threadDelay)

-- 💡 ベンチマーク対象の関数をここにインポート（例: yourModule.yourFunc）
-- import qualified MyProject.DataFetch as DF 

-- | 逐次（Sequential）でデータをフェッチする関数（IOアクション）
sequentialFetch :: Int -> IO [Int]
sequentialFetch n = forM [1..n] (\x -> threadDelay 1000 >> return x)

-- | 並列（Concurrent）でデータをフェッチする関数（IOアクション）
concurrentFetch :: Int -> IO [Int]
concurrentFetch n = mapConcurrently (\x -> threadDelay 1000 >> return x) [1..n]

-- | メイン関数
main :: IO ()
main = defaultMain
  [ bgroup "Data Fetch Performance (N=10)"
    [ -- 'nfIO' は、I/Oアクションを評価し、結果を完全に評価（nf）する時間を計測します
      bench "sequential" $ nfIO (sequentialFetch 10)
    , bench "concurrent" $ nfIO (concurrentFetch 10)
    ]
  , bgroup "Data Fetch Performance (N=100)"
    [ bench "sequential" $ nfIO (sequentialFetch 100)
    , bench "concurrent" $ nfIO (concurrentFetch 100)
    ]
  ]