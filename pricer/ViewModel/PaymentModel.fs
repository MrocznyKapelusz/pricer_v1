namespace ViewModel

open System
open DistributionFuncs
(* A type representing given amount of money in specific currency. Very bare bones, could be extended in various ways. Some examples:
1. Multiplication by float so that $1 * 100 = $100.
2. Addition to other Money instance so that $1 + $2 = $3, but 1 zl + $1 = <exception thrown> *)
type Money =
    {
        Value : float
        Currency : string
    }

    override this.ToString() = sprintf "%.2f (%s)" this.Value this.Currency

(* Model for Payment trade. *)
type PaymentRecord =
    {
        TradeName : string
        Expiry    : DateTime
        Currency  : string
        Principal : int64
    }
    
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(configuration : CalculationConfiguration) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN"; |]
        
        let knownCurrencies = if configuration.ContainsKey "valuation::knownCurrencies" 
                              then configuration.["valuation::knownCurrencies"].Split([|' '|])
                              else knownCurrenciesDefault
        
        {
            TradeName = sprintf "Payment%04d" (PaymentRecord.sysRandom.Next(9999))
            Expiry    = (DateTime.Now.AddMonths (PaymentRecord.sysRandom.Next(1, 6))).Date
            Currency  = knownCurrencies.[ PaymentRecord.sysRandom.Next(knownCurrencies.Length) ]
            Principal = int64 (PaymentRecord.sysRandom.Next())
        }

(* Complete set of data required for valuation *)
type PaymentValuationInputs = 
    {
        Trade : PaymentRecord
        Data : DataConfiguration
        CalculationsParameters: CalculationConfiguration
    }

(* The valuation model for Payment. We may have multiple valuation models implementations per given trade type, or have a valuation model that handles multiple trade types. *)
type PaymentValuationModel(inputs: PaymentValuationInputs) = 
    (* Calculate() method returns a value of given trade. This one is very simple, yet demonstrates some concepts.
    
    It will try to return the result in the global default currency as configured by valuation::baseCurrency key.

    If the valuation::baseCurrency is not defined or we are unable to obtain the FX rate FX::<targetCcy><tradeCcy>, 
    we simply return the value using the trade currency.

    *)
    member this.Calculate() : Money = 
        let tradeCcy = inputs.Trade.Currency

        let targetCcy = match inputs.CalculationsParameters.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let interestRate = match inputs.Data.TryFind "interestRate::percentage" with
                            | Some intRate -> double(intRate)
                            | None -> 0.

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else tradeCcy
        
        let expiryTime = 
            let now = DateTime.Now
            let expiry = inputs.Trade.Expiry
            double (int (expiry - now).Days)/365.

        System.Console.WriteLine(interestRate)
        { Value = ((float inputs.Trade.Principal) / fxRate) * exp(interestRate*expiryTime); Currency = finalCcy }



(* Model for Option trade. *)
type OptionRecord =
    {
        OptionName : string
        Type: string //call albo put
        Maturity    : DateTime
        Currency  : string
        Strike : float
    }
            
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(configuration : CalculationConfiguration) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN"; |]
                
        let knownCurrencies = if configuration.ContainsKey "valuation::knownCurrencies" 
                                then configuration.["valuation::knownCurrencies"].Split([|' '|])
                                else knownCurrenciesDefault

  

        let optionTypes = [|"Call"; "Put"|]
        {
            Type = optionTypes.[OptionRecord.sysRandom.Next(optionTypes.Length)]
            OptionName = sprintf "Option%04d" (OptionRecord.sysRandom.Next(9999))
            Maturity    = (DateTime.Now.AddMonths (OptionRecord.sysRandom.Next(2, 12))).Date
            Currency  = knownCurrencies.[ OptionRecord.sysRandom.Next(knownCurrencies.Length) ]
            Strike = OptionRecord.sysRandom.NextDouble() * ( - 6.51 + 10.51) + 6.51 //get strike from -2,2 of default price
            //Type = optionTypes.[OptionRecord.sysRandom.Next(optionTypes.Length)]
        }
        
(* Complete set of data required for valuation *)
type OptionValuationInputs = 
    {
        Option : OptionRecord
        Data : DataConfiguration
        CalculationsParameters: CalculationConfiguration
    }

type GBMParams = 
    {
        //count:int
        steps:int   //must be even
        price:float
        drift:float
        vol:float
        years:float
        seed:int
    }


type BSParams = 
    {
    k: float    //strike
    m: float    //maturity
    }

type OptionValuationModel(inputs: OptionValuationInputs) = 
    (* Calculate() method returns a value of given trade. This one is very simple, yet demonstrates some concepts.
    
    It will try to return the result in the global default currency as configured by valuation::baseCurrency key.

    If the valuation::baseCurrency is not defined or we are unable to obtain the FX rate FX::<targetCcy><tradeCcy>, 
    we simply return the value using the trade currency.

    *)
    member this.Calculate() : Money * float = 
        let tradeCcy = inputs.Option.Currency

        let targetCcy = match inputs.CalculationsParameters.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else tradeCcy
        
        let getRandom (seed:int) (count:int) :float list=
            let rnd = System.Random(seed)
            if not (count%2=0) then [] //COUNT MUST BE EVEN USING BOX MULLER TRANSFORM
            else List.init count (fun _ -> rnd.NextDouble())

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

        let simulateGBMPath (count:int) (steps:int) (price:float) (drift:float) (vol:float) (years:float) (seed:int) =         
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

        let cdf mean stdev point =
            //FSharp.Stats.Distributions.Continuous.Normal.CDF mean stdev point
            //MathNet.Numerics.Distributions.Normal.CDF(mean, stdev, point)
            DistributionFuncs.normcdf point
            //0.85 * point
        
        let simulateBSPutOptionPriceAndDelta (gbm:GBMParams) (bs:BSParams) =
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
            
            [BScall; BScallDelta; BSput; BSputDelta]
         
        let interestRateStr = match inputs.Data.TryFind "interestRate::percentage" with
                    | Some intRate -> intRate
                    | None -> "0"

        let interestRateFloat = 
            try float interestRateStr
            with
            | _ -> 0.

        let priceStr = 
            match inputs.Data.TryFind "stock::price" with
                | Some price -> price
                | None -> "8.52"

        let priceFloat = 
            try float priceStr
            with
            | _ -> 8.52

        let volStr = 
            match inputs.Data.TryFind "stock::volatility" with
                | Some volatility -> volatility
                | None -> "0.1"

        let volFloat = 
            let volTry = 
                try float volStr with
                | _ -> 0.1
            if volTry > 0. && volTry <1. then volTry
            else 0.1

        let stepsStr = 
            match inputs.CalculationsParameters.TryFind "option::steps" with
                  | Some steps -> steps
                  | None -> "200"
        
        let stepsInt =
            let stepsTry = 
                try int stepsStr with
                | _ -> 100
            if stepsTry > 0 then stepsTry
            else 100

        let seedInt =
            let seedS = match inputs.CalculationsParameters.TryFind "option::seed" with
                                | Some seed -> seed
                                | None -> "7"
            let seedTry = 
                try int seedS with
                | _ -> 77
            if seedTry>0 then seedTry
            else 77

        let strike = 
            match inputs.Option.Strike with
            | strike -> if strike > 0. then strike else 10.
            | _ -> 10.

        let maturity = 
            let yearInTicks = System.DateTime.Now.AddYears(1).Ticks - System.DateTime.Now.Ticks
            match inputs.Option.Maturity with
            | date -> float (date.Ticks - System.DateTime.Now.Ticks) * (double (1.))/(double yearInTicks)
            | _ -> 1.

        let g = {
            years=maturity
            steps=stepsInt
            price=priceFloat
            drift=interestRateFloat //from 5"%" do .05
            vol=volFloat
            seed=seedInt}

        let b = {
            k=strike
            m=maturity}

        let r = simulateBSPutOptionPriceAndDelta g b

        let callResult (r:float list) =
            let m1 = {Value = r.[0] / fxRate; Currency=finalCcy} //call option value
            let m2 = r.[1] //call option delta
            (m1, m2)

        let putResult (r:float list) =
            let m1 = {Value = r.[2] / fxRate; Currency=finalCcy} //call option value
            let m2 = r.[3] //call option delta
            (m1, m2)

        let constructOutput = 
            if inputs.Option.Type = "Put" then putResult r
            else callResult r

        //System.Console.Write("\nConstructed Model Output\n")
        constructOutput
        

