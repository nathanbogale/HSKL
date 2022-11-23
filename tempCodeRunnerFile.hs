---replacing length with foldr
length' (_:xs) = 1 + (length' xs) --initial approach
length' (_:xs) = (+) 1 (length' xs) --prefixing the operator/function
length' (_:xs) = (\n -> 1 + n) x (length' xs)  --substituting it with lambda function
length' (_:xs) = foldr (\n -> 1 + n) 0  --replacing it with the foldr function
