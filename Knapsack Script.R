## For confidentiality reasons, the source data cannot be provided, nor the defined MaxWeight & MaxVolume.
## We will assume that all variables are defined for this reason.

## BRUTE FORCE METHOD - only applicable if, say, number of items on FBA list is < 30 - at MOST 40:
  
  if (nrow(profitProductInfo) < 40) {
    
    ## Define constants:
    items <- profitProductInfo[,1]
    weights <- (profitProductInfo[,7]/453.592)
    volumes <- ceiling((profitProductInfo[,4] * profitProductInfo[,5] * profitProductInfo[,6])/16387.064)
    values <- profitProductInfo[,2]
    ## Get max shipment weight that we would like to aim for to get reasonable shipping cost charge:
    sack.weight <- MaxWeight
    ## Get max shipment volume that we would like to aim for to get reasonable shipping cost charge:
    sack.volume <- MaxVolume
    max.items <- floor(pmin(sack.weight/weights, sack.volume/volumes))
    
    ## Some utility functions:
    getTotalValue <- function(n) sum(n*values)
    getTotalWeight <- function(n) sum(n*weights)
    getTotalVolume <- function(n) sum(n*volumes)
    willFitInSack <- function(n) getTotalWeight(n) <= sack.weight && getTotalVolume(n) <= sack.volume
    
    ## Find all possible combinations, then eliminate those that won't fit in the sack:
    knapsack <- expand.grid(lapply(max.items, function(n) seq.int(0, n)))
    ok <- apply(knapsack, 1, willFitInSack)
    knapok <- knapsack[ok,]
    
    ## Find the solutions with the highest value of profitability:
    vals <- apply(knapok, 1, getTotalValue)
    highProfit <- knapok[vals == max(vals),]
    
    ## Multiply the number of each item (highProfit) by unit profit, weight and volume:
    totalItems <- rowSums(highProfit)
    totalProfit <- rowSums(highProfit * values)
    totalWeight <- ceiling(rowSums(highProfit * weights))
    totalVolume <- ceiling(rowSums(highProfit * volumes))
    highProfitTotal <- data.frame(highProfit, totalItems, totalProfit, totalWeight, totalVolume)
    names(highProfitTotal)[1:length(items)] <- as.character(profitProductInfo[,1])
    names(highProfitTotal)[(length(items)+1):ncol(highProfitTotal)] <- c('totalItems','totalProfit CAD', 'totalWeight LBS', 'totalVolume inches^3')
    
    return(highProfitTotal)
    
    }
