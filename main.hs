{-
    ...
-}

type Vector = (Double, Double, Double)

main :: IO()
main = do 
    let vector1 = (1,1,2)
        vector2 = (1,1,-1)
        dotProd = dotProduct vector1 vector2
        crsProd = crossProduct vector1 vector2
        mixProd = mixedProduct vector1 vector2 crsProd
    putStrLn("")
    putStrLn("dot product:           " ++ show(dotProd))
    putStrLn("cross product:         " ++ show(crsProd))
    putStrLn("mixed product:         " ++ show(mixProd))
    putStrLn("parallelogram area:    " ++ show(norm crsProd))
    putStrLn("parallelepiped volume: " ++ show(abs mixProd))
    
norm :: Vector -> Double
norm (a,b,c) = sqrt(a*a + b*b + c*c)
    
dotProduct :: Vector -> Vector -> Double
dotProduct (a,b,c) (x,y,z) = a*x + b*y + c*z

crossProduct :: Vector -> Vector -> Vector 
crossProduct (a,b,c) (x,y,z) = (b*z - c*y, c*x - a*z, a*y - b*x)

mixedProduct :: Vector -> Vector -> Vector -> Double
mixedProduct (a,b,c) (x,y,z) (i,j,k) = dotProduct (a,b,c) (crossProduct (x,y,z) (i,j,k))