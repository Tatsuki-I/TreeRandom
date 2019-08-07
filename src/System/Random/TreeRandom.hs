module System.Random.TreeRandom where

import Data.Word ( Word8
                 , Word32 )
import Data.Bits
import Control.Parallel.Strategies
import Data.Maybe (fromJust)
import System.IO
import Data.Function ((&))
import Data.List.Split
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable as STV
import Control.Monad.ST (runST)
import qualified Data.Array.IArray                as A
import qualified Data.Array.MArray                as M
import           Data.Array.Unboxed               as U  --TODO
import           Data.Array.ST                    ( writeArray
                                                  , runSTArray
                                                  , runSTUArray )
import qualified Data.Map.Lazy                    as LM

data BTree a
    = Leaf
    | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

data Gen = MkGen Seed
                 (V.Vector Seed)
                 Length
                 Pointer
           deriving ( Show
                    , Eq )

type Seed = Word8
type Length = Int
type Pointer = Int

mkGen   :: Word8 -> Gen
mkGen s =  MkGen s
                 (V.singleton s)
                 1
                 1

next :: Gen -> (Seed, Gen)
next (MkGen s v ln pt) = (ns, MkGen ns nv nln npt)
                         where (npt, nv, nln) | pt + 1 < ln = (pt + 1, v, ln)
                                              | otherwise   = (0, V.concatMap xorshift v, ln * 2)
                               ns             = nv V.! npt

xorshift   :: Seed -> V.Vector Seed
xorshift s =  V.singleton (xor1 s) `V.snoc` xor2 s

randoms     :: Int -> Seed -> [Seed]
randoms i s =  take i $ f (mkGen s)
               where f g = ns : f ng
                           where (ns, ng) = next g


a :: Word8
a =  17

b :: Word8
b =  13

umask :: Word8
umask =  0x55

lmask :: Word8
lmask =  0xAA

tr     :: Int      -- ^ Length
       -> Word8   -- ^ Seed
       -> BTree (Int, Word8) -- ^ Random numbers
tr l s =  tr' ((floor . logBase 2 . fromIntegral) l + 1) (1, s)
        where tr'          :: Int
                           -> (Int, Word8)
                           -> BTree (Int, Word8)
              tr' 0 _      =  Leaf
              tr' l e@(i, s) =  Node e (tr' (l - 1) (i * 2    , xor1 s))
                                       (tr' (l - 1) (i * 2 + 1, xor2 s))

trl     :: Int      -- ^ Length
        -> Word8   -- ^ Seed
        -> [(Int, Word8)] -- ^ Random numbers
trl l s =  trl' ((floor . logBase 2 . fromIntegral) l + 1) (1, s)
           where trl'            :: Int
                                 -> (Int, Word8)
                                 -> [(Int, Word8)]
                 trl' 0 _        =  []
                 trl' l e@(i, s) =  e :  trl' (l - 1) (i * 2    , xor1 s)
                                      ++ trl' (l - 1) (i * 2 + 1, xor2 s)


tra     :: Int      -- ^ Length
        -> Word8   -- ^ Seed
        -> A.Array Int Word8 -- ^ Random numbers
tra l s =  A.array (1, (2 ^ ((floor . logBase 2 . fromIntegral) l + 1)) - 1)
                   (tra' ((floor . logBase 2 . fromIntegral) l + 1) (1, s))
           where tra'            :: Int
                                 -> (Int, Word8)
                                 -> [(Int, Word8)]
                 tra' 0 _        =  []
                 tra' l e@(i, s) =  e :  tra' (l - 1) (i * 2    , xor1 s)
                                      ++ tra' (l - 1) (i * 2 + 1, xor2 s)

ptra     :: Int      -- ^ Length
         -> Word8   -- ^ Seed
         -> A.Array Int Word8 -- ^ Random numbers
ptra l s =  A.array (1, l)
                    (ptra' ((floor . logBase 2 . fromIntegral) l + 1) (1, s))
            where ptra'            :: Int
                                   -> (Int, Word8)
                                   -> [(Int, Word8)]
                  ptra' 0 _        =             []
                  ptra' r e@(i, s) | i > l     = []
                                   | otherwise = runEval $ do n1 <- (rpar . ptra' (r - 1)) (i * 2,     xor1 s)
                                                              n2 <- (rpar . ptra' (r - 1)) (i * 2 + 1, xor2 s)
                                                              return $ e : n1 ++ n2

ptrMap     :: Int      -- ^ Length
           -> Word8   -- ^ Seed
           -> LM.Map Int Word8 -- ^ Random numbers
ptrMap l s =  LM.fromAscList (ptrMap' ((floor . logBase 2 . fromIntegral) l + 1) (1, s))
              where ptrMap'            :: Int
                                       -> (Int, Word8)
                                       -> [(Int, Word8)]
                    ptrMap' 0 _        =             []
                    ptrMap' r e@(i, s) | i > l     = []
                                       | otherwise = runEval $ do n1 <- (rpar . ptrMap' (r - 1)) (i * 2,     xor1 s)
                                                                  n2 <- (rpar . ptrMap' (r - 1)) (i * 2 + 1, xor2 s)
                                                                  return $ e : n1 ++ n2


ptr   :: Int
      -> Word8
      -> BTree Word8
ptr l =  ptr' ((floor . logBase 2 . fromIntegral) l + 1)
         where ptr'     :: Int
                        -> Word8
                        -> BTree Word8
               ptr' 0 _ =  Leaf
               ptr' l s =  runEval $ do n1 <- (rpar . ptr' (l - 1)) (xor1 s)
                                        n2 <- (rpar . ptr' (l - 1)) (xor2 s)
                                        return $ Node s n1 n2

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

toVec :: BTree (Int, a)
      -> Int
      -> V.Vector a
toVec v ln = runST $ do res <- STV.new ln
                        V.freeze res

writeTR i s =  writeFile ("result_" ++ show i ++ ".csv") res
               where res = unlines $ splitOn "," $ init $ tail $ show $ toList (ptr i s) i
