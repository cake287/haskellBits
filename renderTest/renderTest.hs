width = 100
height = 27

fillVec len val = take len (repeat val)

vecAdd [x1, y1] [x2, y2] = [x1 + x2, y1 + y2]
vecAdd [x1, y1, z1] [x2, y2, z2] = [x1 + x2, y1 + y2, z1 + z2]

vecSub [x1, y1] [x2, y2] = [x1 - x2, y1 - y2]
vecSub [x1, y1, z1] [x2, y2, z2] = [x1 - x2, y1 - y2, z1 - z2]

vecLength [x, y] = sqrt (x*x + y*y)
vecLength [x, y, z] = sqrt (x*x + y*y + z*z)

vecAbs [x, y] = [abs x, abs y]
vecAbs [x, y, z] = [abs x, abs y, abs z]

vecMax [x1, y1] [x2, y2] = [max x1 x2, max y1 y2]
vecMax [x1, y1, z1] [x2, y2, z2] = [max x1 x2, max y1 y2, max z1 z2]



sdfSphere p r = vecLength p - r -- r is single value
sdfBox p r =  -- r is vector of same length as p
    let 
        dist = (vecAbs p) `vecSub` r
        zeroes = fillVec (length p) 0
    in
        vecLength (vecMax dist zeroes)

mapCoord p = 
    let 
        dist1 = sdfSphere (p `vecSub` [0.5, 0.4]) 0.4
        dist2 = sdfBox (p `vecSub` [(-0.7), (-0.2)]) [0.6, 0.3]
    in 
        min dist1 dist2




frag2UVcoord (fragX, fragY) = 
    (
        (fragX - 0.5*width ) / height, -- width is adjusted since pixels are ~2 times as tall as they are wide
        (2*fragY - height) / (-height)
    )


fragCol (x, y) = 
    let
        (uvX, uvY) = frag2UVcoord (x, y)
        dist = mapCoord [uvX, uvY]
    in
        if dist <= 0
        then 0.5 * (cos uvX + sin uvY)
        else 0





charFromCol col =
    let 
        boundedCol = min (max col 0) 1
        asciiChars = " .'`^,-~+:;><Il!i?][}{1)(/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$" -- chars in increasing darkness
    in 
        asciiChars !! ( floor ( boundedCol * fromIntegral (length asciiChars - 1) ) )

fragChar (x, y) = charFromCol (fragCol (x, y))

getRow thisX y
        | thisX == width = []
        | True           = fragChar (thisX, y) : getRow (thisX + 1) y

getRows thisY
        | thisY == height = []
        | True            = [getRow 0 thisY] ++ getRows (thisY + 1)


displayImage = mapM_ putStrLn (getRows 0)