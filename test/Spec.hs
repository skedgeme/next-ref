{-# LANGUAGE LambdaCase #-}
import Test.Hspec
import Control.Concurrent.NextRef
import GHC.Conc
import Control.Monad (void, replicateM_)

while :: IO Bool -> IO () -> IO ()
while test act = test >>= \case 
  True  -> act >> while test act
  False -> return ()
  

main :: IO ()
main = hspec $ describe "NextRef" $ do 
  it "newNextRef/takeNext/takeNext blocks" $ do
    ref <- newNextRef ()
    takeNextRef ref
    threadId <- forkIO $ void $ takeNextRef ref
    
    while ((== ThreadRunning) `fmap` threadStatus threadId) $ threadDelay 100000
    
    stat <- threadStatus threadId 
    stat `shouldBe` ThreadBlocked BlockedOnSTM
    
  it "writing never blocks" $ do
    ref <- newNextRef 1
    replicateM_ 10 $ writeNextRef ref 2
    True `shouldBe` True 
  
  it "readLast does not block" $ do
    ref <- newNextRef ()
    takeNextRef ref
    readLast ref
    
    True `shouldBe` True 
    
  it "takeNext/write/takeNext doesn't block, updates correctly" $ do
    ref <- newNextRef (1 :: Int)
    actual <- takeNextRef ref
    actual `shouldBe` Just 1
    
    writeNextRef ref 2
    
    actual1 <- takeNextRef ref
    actual1 `shouldBe` Just 2
    
  it "takeNext/modify/takeNext doesn't block, updates correctly" $ do
    ref <- newNextRef (1 :: Int)
    actual <- takeNextRef ref
    actual `shouldBe` Just 1
    
    modifyNextRef ref (\x -> (x+1, ()))
    
    actual1 <- takeNextRef ref
    actual1 `shouldBe` Just 2
    
  it "close/takeNext give nothing" $ do
    ref <- newNextRef ()
    close ref
    actual <- takeNextRef ref
    actual `shouldBe` Nothing
    
  it "close/open/takeNext gives value" $ do
    ref <- newNextRef ()
    close ref
    open  ref
    actual <- takeNextRef ref
    actual `shouldBe` Just ()
  
    
    