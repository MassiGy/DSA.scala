-- flatten
flatten :: [[a]] -> [a]
flatten []       = []
flatten [(x:xs)] = (x:xs)
flatten (xs:xss) = xs ++ flatten xss

