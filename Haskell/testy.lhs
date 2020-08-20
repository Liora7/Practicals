cp :: [[a]] -> [[a]]
cp xss = foldr h [[]] xss
    where h as xss = foldr (\a bs -> (map (a:) xss)++bs) [] ass
