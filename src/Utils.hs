module Utils where


i0 = 0 :: Int
i1 = 1 :: Int
i2 = 2 :: Int
i3 = 3 :: Int
i9 = 9 :: Int

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs
