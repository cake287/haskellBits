width = 200
height = 46
-- width = 100
-- height = 27

clamp x lower upper = min (max x lower) upper

fillVec len val = take len (repeat val)

vecAdd [x1, y1] [x2, y2] = [x1 + x2, y1 + y2]
vecAdd [x1, y1, z1] [x2, y2, z2] = [x1 + x2, y1 + y2, z1 + z2]

vecSub [x1, y1] [x2, y2] = [x1 - x2, y1 - y2]
vecSub [x1, y1, z1] [x2, y2, z2] = [x1 - x2, y1 - y2, z1 - z2]

vecMul [a] [x, y] = [a*x, a*y]
vecMul [a] [x, y, z] = [a*x, a*y, a*z]
vecMul [x1, y1] [x2, y2] = [x1*x2, y1*y2]
vecMul [x1, y1, z1] [x2, y2, z2] = [x1*x2, y1*y2, z1*z2]

vecDiv [x, y] [a] = [x / a, y / a]
vecDiv [x, y, z] [a] = [x / a, y / a, z / a]
vecDiv [x1, y1] [x2, y2] = [x1 / x2, y1 / y2]
vecDiv [x1, y1, z1] [x2, y2, z2] = [x1 / x2, y1 / y2, z1 / z2]

vecLength [x, y] = sqrt (x*x + y*y)
vecLength [x, y, z] = sqrt (x*x + y*y + z*z)

vecAbs [x, y] = [abs x, abs y]
vecAbs [x, y, z] = [abs x, abs y, abs z]

vecMax [x1, y1] [a] = [max x1 a, max y1 a]
vecMax [x1, y1, z1] [a] = [max x1 a, max y1 a, max z1 a]
vecMax [x1, y1] [x2, y2] = [max x1 x2, max y1 y2]
vecMax [x1, y1, z1] [x2, y2, z2] = [max x1 x2, max y1 y2, max z1 z2]

-- vecNormalise v = 
--     let len = vecLength v 
--     in v `vecDiv` [if len == 0 then 1 else len]
vecNormalise v = v `vecDiv` [vecLength v]

vecDot [x1, y1, z1] [x2, y2, z2] = x1*x2 + y1*y2 + z1*z2
vecCross [x1, y1, z1] [x2, y2, z2] = [y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2] 

rotY [x, y, z] angle = 
    let 
        s = sin angle
        c = cos angle
    in
        [c*x + s*z, y, (-s)*x + c*z]

rotZ [x, y, z] angle = 
    let 
        s = sin angle
        c = cos angle
    in
        [c*x - s*y, s*x + c*y, z]


sdfSphere p r = vecLength p - r -- r is single value
sdfBox p r = vecLength (vecMax ((vecAbs p) `vecSub` r) [0]) -- r is vector of same length as p

mapCoord p = 
    let 
        dist1 = sdfSphere (p `vecSub` [0.5, 0.4, 0.1]) 0.4
        dist2 = sdfBox (p `vecSub` [(-0.7), (-0.2), (-0.2)]) [0.6, 0.3, 0.4]
        dist3 = sdfBox (p `vecSub` [0, (-0.6), 0.5]) [3, 0.1, 3]
    in 
        min dist1 (min dist2 dist3)



softShadow rayStart rayDir t k res
        | dist < 0.001 = 0
        | t > 5 = res
        | True = softShadow rayStart rayDir (t+dist) k (min res (k*dist/t)) 
        where dist = mapCoord (rayStart `vecAdd` ([t] `vecMul` rayDir))


rayStep :: [Float] -> [Float] -> Int -> (Float, [Float])
rayStep rayPos rayDir step
        | dist < 0.001 = (dist, rayPos) -- surface found
        | step >= 32 = (dist, rayPos) -- ray march hasn't found a collision
        | True = rayStep (rayPos `vecAdd` ([dist] `vecMul` rayDir)) rayDir (step + 1)
        where dist = mapCoord rayPos 


-- https://iquilezles.org/articles/normalsSDF
calcNormal :: [Float] -> [Float]
calcNormal p =
    let 
        e = 0.5773
        eps = 0.005

        eX =   e
        eY = (-e)
        epX = e*eps
        epY = (-e)*eps

        m1 = [mapCoord (p `vecAdd` [epX, epY, epY])] `vecMul` [eX, eY, eY]
        m2 = [mapCoord (p `vecAdd` [epY, epY, epX])] `vecMul` [eY, eY, eX]
        m3 = [mapCoord (p `vecAdd` [epY, epX, epY])] `vecMul` [eY, eX, eY]
        m4 = [mapCoord (p `vecAdd` [epX, epX, epX])] `vecMul` [eX, eX, eX]
    
    in
        vecNormalise (m1 `vecAdd` m2 `vecAdd` m3 `vecAdd` m4)

surfaceCol p = 
    let
        baseCol = 1
        normal = calcNormal p

        light1 = vecNormalise [0.4, 0.5, (-0.05)]
        shadow1 = softShadow p light1 0.01 8 1
        diffuse1 = clamp (vecDot normal light1) 0 1

        ambient = 0.3 + 0.5*(normal !! 1)
    in
        0.1*ambient + diffuse1*shadow1*baseCol 


frag2UVcoord (fragX, fragY) = 
    (
        (fragX - 0.5*width ) / height, -- width is adjusted since pixels are ~2 times as tall as they are wide
        (2*fragY - height) / (-height)
    )
    
fragCol (x, y) = 
    let
        (uvX, uvY) = frag2UVcoord (x, y)

        camAngleX = 1
        camAngleY = 0.3
        rayStart = rotY (rotZ [2, 0, 0] camAngleY) camAngleX
        rayTarget = [0, 0, 0]

        -- camera matrix rows
        ww = vecNormalise (rayTarget `vecSub` rayStart)
        uu = vecNormalise (vecCross ww [0, 1, 0])
        vv = vecNormalise (vecCross uu ww)

        focalLength = 2
        rayDir = vecNormalise (([uvX] `vecMul` uu) `vecAdd` ([uvY] `vecMul` vv) `vecAdd` ([focalLength] `vecMul` ww))

        (dist, rayEnd) = rayStep rayStart rayDir 0
    in
        if dist <= 0.01
        then surfaceCol rayEnd
        else 0.05



charFromCol col =
    let 
        boundedCol = min (max col 0) 1
        asciiChars =  " `.-':_,^=;><+!rc*/z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@" -- chars in increasing brightness
        --charBrightnesses = [0, 0.0751, 0.0829, 0.0848, 0.1227, 0.1403, 0.1559, 0.185, 0.2183, 0.2417, 0.2571, 0.2852, 0.2902, 0.2919, 0.3099, 0.3192, 0.3232, 0.3294, 0.3384, 0.3609, 0.3619, 0.3667, 0.3737, 0.3747, 0.3838, 0.3921, 0.396, 0.3984, 0.3993, 0.4075, 0.4091, 0.4101, 0.42, 0.423, 0.4247, 0.4274, 0.4293, 0.4328, 0.4382, 0.4385, 0.442, 0.4473, 0.4477, 0.4503, 0.4562, 0.458, 0.461, 0.4638, 0.4667, 0.4686, 0.4693, 0.4703, 0.4833, 0.4881, 0.4944, 0.4953, 0.4992, 0.5509, 0.5567, 0.5569, 0.5591, 0.5602, 0.5602, 0.565, 0.5776, 0.5777, 0.5818, 0.587, 0.5972, 0.5999, 0.6043, 0.6049, 0.6093, 0.6099, 0.6465, 0.6561, 0.6595, 0.6631, 0.6714, 0.6759, 0.6809, 0.6816, 0.6925, 0.7039, 0.7086, 0.7235, 0.7302, 0.7332, 0.7602, 0.7834, 0.8037, 0.9999]
    in 
        asciiChars !! ( floor ( (1 - boundedCol) * fromIntegral (length asciiChars - 1) ) )

fragChar (x, y) = charFromCol (fragCol (x, y))

getRow thisX y
        | thisX == width = []
        | True           = fragChar (thisX, y) : getRow (thisX + 1) y

getRows thisY
        | thisY == height = []
        | True            = [getRow 0 thisY] ++ getRows (thisY + 1)


displayImage = mapM_ putStrLn (getRows 0)
