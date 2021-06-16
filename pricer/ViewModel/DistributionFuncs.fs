module DistributionFuncs
    
    open System
    let sqr (x:float<'u>) = x * x 

    /// Computes the complementary error function. This function is defined 
    /// by 2/sqrt(pi) * integral from x to infinity of exp (-t^2) dt
    let erfc x =
        if (Double.IsNegativeInfinity x) then 2.0
        elif (Double.IsPositiveInfinity x) then 0.0
        else
            let z = abs x
            let t = 1.0 / (1.0 + 0.5 * z) 
            let res = t * exp (-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277))))))))) 
            if (x >= 0.0) then res else 2.0 - res


    /// Computes the cummulative Gaussian distribution at a specified point of interest
    let normcdf t = 
        let sqrt2 = 1.4142135623730951 
        (erfc (-t / sqrt2)) / 2.0