namespace ViewModel
 
//Representation of a Payment to the UI

//PAYMENT
type PaymentViewModel2(input : PaymentRecord) = 
   inherit ViewModelBase()

   let mutable userInput = input
   let mutable value : Money option = None

   member this.TradeName 
        with get() = userInput.TradeName
        and set(x) = 
            userInput <- {userInput with TradeName = x }
            base.Notify("TradeName")

    member this.Expiry 
        with get() = userInput.Expiry
        and set(x) = 
            userInput <- {userInput with Expiry = x }
            base.Notify("Expiry")

    member this.Currency 
        with get() = userInput.Currency
        and set(x) = 
            userInput <- {userInput with Currency = x }
            base.Notify("Currency")

    member this.Principal 
        with get() = userInput.Principal
        and set(x) = 
            userInput <- {userInput with Principal = x }
            base.Notify("Principal")
    
    member this.Value
        with get() = value
        and set(x) = 
            value <- x
            base.Notify("Value")

    // Invoke the valuation based on user input
    member this.Calculate(data : DataConfiguration, calculationParameters : CalculationConfiguration) = 
        
        //capture inputsu7
        let paymentInputs : PaymentValuationInputs = 
            {
                Trade = 
                         {
                             TradeName = this.TradeName
                             Expiry    = this.Expiry
                             Currency  = this.Currency
                             Principal = this.Principal
                         }
                Data = data
                CalculationsParameters = calculationParameters
            }
        //calculate
        let calc = PaymentValuationModel(paymentInputs).Calculate()

        //present to the user
        this.Value <- Option.Some (calc)



(* summary row. there is little functionality here, so this is very brief. *)
type SummaryRow = 
    {
        Currency: string
        Value : float
    }


//OPTION
type PaymentViewModel(input : OptionRecord) = 
    inherit ViewModelBase()

    let mutable userInput = input
    let mutable value : Money option = None
    let mutable delta : float option = None

    member this.OptionName 
        with get() = userInput.OptionName
        and set(x) = 
            userInput <- {userInput with OptionName = x }
            base.Notify("OptionName")


    member this.Type 
        with get() = userInput.Type
        and set(x) = 
            userInput <- {userInput with Type = x }
            base.Notify("Type")

    member this.Maturity 
        with get() = userInput.Maturity
        and set(x) = 
            userInput <- {userInput with Maturity = x }
            base.Notify("Maturity")

    member this.Currency 
        with get() = userInput.Currency
        and set(x) = 
            userInput <- {userInput with Currency = x }
            base.Notify("Currency")

    member this.Strike 
        with get() = userInput.Strike
        and set(x) = 
            userInput <- {userInput with Strike = x }
            base.Notify("Strike")
    
    member this.Value
        with get() = value
        and set(x) = 
            value <- x
            base.Notify("Value")

    member this.Delta
        with get() = delta
        and set(x) = 
            delta <- x
            base.Notify("Delta")

    // Invoke the valuation based on user input
    member this.Calculate(data : DataConfiguration, calculationParameters : CalculationConfiguration) = 
    
        //capture inputsu7
        let paymentInputs : OptionValuationInputs = 
            {
                Option = 
                         {
                             OptionName = this.OptionName
                             Type = this.Type
                             Maturity    = this.Maturity
                             Currency  = this.Currency
                             Strike = this.Strike
                         }
                Data = data
                CalculationsParameters = calculationParameters
            }
        //calculate
        let calc : Money*float = OptionValuationModel(paymentInputs).Calculate()
        System.Console.Write(calc)
        //present to the user
        this.Value <- Option.Some (fst calc)
        this.Delta <- Option.Some (snd calc)