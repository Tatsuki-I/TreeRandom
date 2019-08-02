module System.Random.TreeRandom where

import Data.Word ( Word8
                 , Word32 )
import Data.Bits
import Control.Parallel.Strategies
import Data.Maybe (fromJust)
import System.IO
import Data.Function ((&))
import Data.List.Split

data BTree a
    = Leaf
    | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

a :: Word8
a =  17

b :: Word8
b =  13

umask :: Word8
umask =  0x55

lmask :: Word8
lmask =  0xAA

tr   :: Int      -- ^ Length
     -> Word8   -- ^ Seed
     -> BTree Word8 -- ^ Random numbers
tr l =  tr' (((floor . logBase 2 . fromIntegral) l) + 1)
        where tr'     :: Int
                      -> Word8
                      -> BTree Word8
              tr' 0 _ =  Leaf
              tr' l s =  Node s (tr' (l - 1)  r)
                                (tr' (l - 1) rr)
                         where r  = f s
                               rr = f $ maxBound - r
ptr   :: Int
      -> Word8
      -> BTree Word8
ptr l =  ptr' (((floor . logBase 2 . fromIntegral) l) + 1)
         where ptr'   :: Int
                      -> Word8
                      -> BTree Word8
               ptr' 0 _ =  Leaf
               ptr' l s =  runEval $ do n1 <- (rpar . ptr' (l - 1))  r
                                        n2 <- (rpar . ptr' (l - 1)) rr
                                        return $ Node s n1 n2
                           where r  = xor1 s
                                 rr = xor2 s

f   :: Word8 -> Word8
f s =  a * s + b

g s =  b * s + a

xor1 :: Word8 -> Word8
xor1 s =  s & (\v -> (v `shiftL` 1) `xor` v)
            & (\v -> (v `shiftR` 7) `xor` v)
            & (\v -> (v `shiftL` 3) `xor` v)



xor2 :: Word8 -> Word8
xor2 s =  s & (\v -> (v `shiftL` 5) `xor` v)
            & (\v -> (v `shiftR` 1) `xor` v)
            & (\v -> (v `shiftL` 3) `xor` v)

toList   :: BTree a
         -> Int
         -> [a]
toList t n =  take n $ map (fromJust) $ toList' [t] []
              where toList'        :: [BTree a] -- ^ Tree
                                   -> [Maybe a]       -- ^ Result state
                                   -> [Maybe a]
                    toList' [] es   = es
                    toList' ts es   =  toList' nts (es ++ nes)
                                       where nes = map (fst . getElem) ts
                                             nts = concatMap (snd . getElem) ts

getElem              :: BTree a
                     -> (Maybe a, [BTree a])
getElem Leaf           =  (Nothing, [])
getElem (Node e t1 t2) =  (Just e, [t1, t2])


xorshift :: Word8 -> Word8
xorshift = undefined

writeTR i s =  writeFile ("result_" ++ show i ++ ".csv") res
               where res = unlines $ splitOn "," $ init $ tail $ show $ toList (ptr i s) i
