namespace GBM_1

module basic =
    let hello name =
        printfn "Hello %s" name


    let getRandom (seed:int) (count:int) :float list=
        let rnd = System.Random(seed)
        if not (count%2=0) then [] //COUNT MUST BE EVEN USING BOX MULLER TRANSFORM
        else List.init count (fun _ -> rnd.NextDouble())
        
    //for lists of lists
    let writeTestResultToFile filename source = 
        let path = __SOURCE_DIRECTORY__ + "/" + filename
        let writer = new System.IO.StreamWriter(path,true)
        for l in source do
            l |> List.map string |> String.concat "," |> writer.Write
            "\n" |> writer.Write
        writer.Close()

    let writeToFile (filename:string) (source) = 
        let path = __SOURCE_DIRECTORY__ + "/" + filename
        let writer = new System.IO.StreamWriter(path,true)
        source |> List.map string |> String.concat "\n" |> writer.Write
        writer.Close()

    let writeLineToFile (filename:string) (source) = 
        let path = __SOURCE_DIRECTORY__ + "/" + filename
        let writer = new System.IO.StreamWriter(path,true)
        "\n" |> writer.Write
        source |> List.map string |> String.concat "," |> writer.Write
        writer.Close()

 
    //BOX-MUELLER TRANSFORM
    let normalizeList (randomList:float list) =
        let rec normalize (newNormalList:float list) (iter:int) =
            if iter = randomList.Length then newNormalList
            else
                let U1 = randomList.[iter]
                let U2 = randomList.[iter+1]
                let N1 = sqrt(-2.*log(U1))*sin(System.Math.PI*2.*U2)
                let N2 = sqrt(-2.*log(U1))*cos(System.Math.PI*2.*U2)
                normalize (newNormalList@[N1;N2]) (iter+2)
        normalize [] 0
    
    //getRandom 777712 3000 |> normalizeList
    //getRandom 12345 10  

module GBM = 
    
    open basic

    let simulateGBMPath (count:int) (steps:int) (price:float) (drift:float) (vol:float) (years:int) (seed:int) =         
        let GBMPath = [price]
        let normalVars = getRandom seed steps |> normalizeList
        let rec buildGBMPath (priceList:float list) (iter:int) =
            if iter=steps-1 then priceList
            else
                let firstComp = float(years)/float(steps)*(drift - vol**2./2.)
                let secondComp = vol * sqrt(float(years)/float(steps)) * normalVars.[iter]
                let newPrice = priceList.[iter] * exp(firstComp+secondComp)
                buildGBMPath (priceList@[newPrice]) (iter+1)
        buildGBMPath GBMPath 0
        


    writeToFile "test.txt" (simulateGBMPath 1 100 10.0 0.5 0.5 2 100)



    let testGBMData (count:int) (steps:int) (price:float) (drift:float) (vol:float) (years:int) (seed:int) =
        let rec buildResult currentResult t =
           if t = count then currentResult
           else
               let newPath = simulateGBMPath count steps price drift vol years (seed+t)
               let finalPriceOfPath = newPath.[newPath.Length-1]
               let rec buildVolList volList iter =
                    if iter = steps-1 then volList
                    else
                        let currentVol = log(newPath.[iter+1]/newPath.[iter])
                        buildVolList (volList@[currentVol]) (iter+1)
               let volList = buildVolList [] 0
               let volAvg = List.average volList
               let squares = List.fold( fun acc elem -> acc + (elem - volAvg)**2.) 0. volList
               let histVolatility = (float(steps)/(float(years)* float(steps-1)))*squares
               let newResult = [finalPriceOfPath;histVolatility]
               buildResult (currentResult@[newResult]) (t+1)
        let result = buildResult [] 0
        result

    let test = testGBMData 1000 400 10. 0.5 0.5 1 12345

    writeTestResultToFile "output.txt" test 

module BlackSholesModel =
    open basic
    open System
    #r "nuget: FSharp.Stats"
    open FSharp.Stats.Distributions.Continuous

    type GBMParam = 
        {
            steps: int
            price: float
            drift: float
            vol: float
            years: int
            seed: int
        }

    type BSParam =
        {
            k: float    //strike
            m: float    //maturity
        }

    let cdf mean stdev point =
        Normal.CDF mean stdev point

    let simulateBSPutOptionPriceAndDelta (gbm:GBMParam) (bs:BSParam) =
        let GBMPath = [gbm.price]
        let normalVars = getRandom gbm.seed gbm.steps |> normalizeList
        let rec buildGBMPath (priceList:float list) (iter:int) =
            if iter=gbm.steps-1 then priceList
            else
                let firstComp = float(gbm.years)/float(gbm.steps)*(gbm.drift - gbm.vol**2./2.)
                let secondComp = gbm.vol * sqrt(float(gbm.years)/float(gbm.steps)) * normalVars.[iter]
                let newPrice = priceList.[iter] * exp(firstComp+secondComp)
                buildGBMPath (priceList@[newPrice]) (iter+1)
        let stockPricesList = buildGBMPath GBMPath 0
        let finalPrice = stockPricesList.[stockPricesList.Length-1]

        let d1 = (log(gbm.price/bs.k) + (gbm.drift + 0.5*(gbm.vol**2.)*bs.m)) / (gbm.vol*sqrt(bs.m))
        let d2 = d1 - gbm.vol*sqrt(bs.m)
        let BScall =             
            let BScallPrice = gbm.price * (cdf 0. 1. d1) - bs.k * exp(-gbm.drift*bs.m) * (cdf 0. 1. d2)
            BScallPrice
        
        let BScallDelta = cdf 0. 1. d1
        let BSputDelta = -1. * cdf 0. 1. -d2

        let BSput = bs.k*exp(-gbm.drift*bs.m) * (cdf 0. 1. -d2) - gbm.price * (cdf 0. 1. -d1)
        //System.Console.Write(BScallDelta-BSputDelta)
        //System.Console.Write("\n")
        //System.Console.Write(BSput-BScall)
        //System.Console.Write("\n")
        //System.Console.Write(gbm.price-bs.k)
        //[BScall; BScallDelta; BSput; BSputDelta]
        BScallDelta

    let g2 = {
           years=1
           steps=200
           price=4.2
           drift=0.14
           vol=0.1
           seed=5}

    let b2 = {
           k=8.
           m=1.}

    simulateBSPutOptionPriceAndDelta g2 b2

    let g = {
        years=1
        steps=200
        price=1.
        drift=0.14
        vol=0.2
        seed=5}
    
    let b = {
        k=25.
        m=0.5}

    let createList =
        let rec buildList currentResult g b step =
            if step = 100 then currentResult
            else
                let r = simulateBSPutOptionPriceAndDelta g b
                let nextPoint = [g.price; r]
                let newResult = currentResult@[nextPoint]
                let gNew = { years=g.years; steps=g.steps; price=g.price+1.; drift=g.drift; vol=g.vol; seed=g.seed}
                let stepNew = step + 1
                buildList newResult gNew b stepNew
        buildList [] g b 1


    let a = createList

    a |> writeTestResultToFile "calldelta2vol.csv"