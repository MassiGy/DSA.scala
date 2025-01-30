

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
		where 
		 ys = [a | a <- xs, a <= x]
		 zs = [a | a <- xs, a > x ]

