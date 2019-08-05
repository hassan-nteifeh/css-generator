type Link = Float
type Power = Float
type Location = (Float, Float)

data LinkStation = LinkStation {
    reach :: Float,
    loc :: Location
    power :: Maybe Float
}

data Device = Device {
   devLoc :: Location
} 

square x = x * x

calcHypotenuse x y = sqrt (square x + square y)

calcDistance (x1, y1) (x2, y2) = hypotenuse (abs (x1 - x2)) (abs (y1 - y2))

calcPower :: Device -> LinkStation -> Float
calcPower dvc lnks = 
    let loc1 = devLoc dvc
        loc2 = loc lnks
        r = reach lnks
        ds = calcDistance loc1 loc2
        in if ds > r then 0
            else square $ r - ds

calcLinkStationsPowers dvc xs = map (\x -> calcPower dvc x) xs

main = do
    let dvc = Device (3.0, 2.0)
        lnks = [LinkStation 10 (0, 0) Nothing, LinkStation 12 (10, 0) Nothing, LinkStation 5 (20, 20) Nothing]
        powers = calcLinkStationsPowers dvc lnks
        linkStationWithMostPower = foldl (\acc x -> if ) (head powers) powers