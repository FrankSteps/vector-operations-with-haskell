{-
    ...
-}
type Vector = (Double, Double, Double)

main :: IO()
main = do  
    let vector1     = (-1,1,-2)
        vector2     = (2,1,1)
        normU       = norm vector1
        normV       = norm vector2
        dotProd     = dotProduct vector1 vector2
        crsProd     = crossProduct vector1 vector2
        mixProd     = mixedProduct vector1 vector2 crsProd
        angleRad    = angleVector dotProd normU normV
        angleDegree = radToDegree angleRad
        
    putStrLn("")
    putStrLn("dot product:           " ++ show(dotProd))
    putStrLn("cross product:         " ++ show(crsProd))
    putStrLn("mixed product:         " ++ show(mixProd))
    putStrLn("rad:                   " ++ show(angleRad))
    putStrLn("degree:                " ++ show(angleDegree))
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

radToDegree :: Double -> Double 
radToDegree rad = rad * 180/pi

angleVector :: Double -> Double -> Double -> Double 
angleVector dotProd uNorm vNorm = acos(dotProd / (uNorm * vNorm)) 