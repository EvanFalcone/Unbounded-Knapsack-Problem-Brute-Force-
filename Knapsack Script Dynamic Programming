## Unbounded Knapsack Problem using Dynamic Programming (more scalable than Brute Force, but still limited):

ShipmentQuantities <- function (FBAprofitability, AmazonProductData, MaxWeight, MaxVolume) {

## DYNAMIC PROGRAMMING METHOD - applicable for larger number of items on FBA list (still scales slowly so be careful!):
    ## Get the units right! With the divisions included, weight = lbs & volume = inches^3:
    
    Data_ <- as.data.frame(structure(list(item = as.character(profitProductInfo[,1]),
                                          value = as.numeric(profitProductInfo[,2]),
                                          weight = as.numeric((profitProductInfo[,7]/453.592)),
                                          volume = as.numeric((ceiling((profitProductInfo[,4] * profitProductInfo[,5] * profitProductInfo[,6])/16387.064)),
                                                              .Names = c("item", "value", "weight", "volume"),
                                                              row.names = c(NA, 3L),
                                                              class = "data.frame"))))
    
    knapsack_volume <- function(Data, W, Volume, full_K) {
      
      ## Data must have the columns with names: item, value, weight and volume.
      K <- list() ## highest values
      K_item <- list() ## items that reach the highest value
      K <- rep(0, W+1) ## The position '0'
      K_item <- rep('', W+1) ## The position '0'
      
      for(w in 1:W)
      {
        temp_w <- 0
        temp_item <- ''
        temp_value <- 0
        for (i in 1:dim(Data)[1]) ## each row
        {
          wi <- Data$weight[i] ## item i
          vi <- Data$value[i]
          item <- Data$item[i]
          volume_i <- Data$volume[i]
          if (wi <= w & volume_i <= Volume)
          {
            back <- full_K[[Volume - volume_i + 1]][w - wi + 1]
            temp_wi <- vi + back
            if (temp_w < temp_wi)
            {
              temp_value <- temp_wi
              temp_w <- temp_wi
              temp_item <- item
            }	
          }
        }
        K[[w+1]] <- temp_value
        K_item[[w+1]] <- temp_item
      }
      return(list(K = K, Item = K_item))
    }
    
    
    Un_knapsack <- function(Data, W, V)
    {
      K <- list();K_item <- list()
      K[[1]] <- rep(0,W+1) ## the line 0
      K_item[[1]] <- rep('', W+1) ## the line 0
      for(v in 1:V)
      {
        best_volume_v <- knapsack_volume(Data, W, v, K)
        K[[v+1]] <- best_volume_v$K
        K_item[[v+1]] <- best_volume_v$Item
      }
      
      return(list(K = data.frame(K), Item = data.frame(K_item, stringsAsFactors = F)))
      
    }
    
    retrieve_info <- function(knapsack, Data)
    {
      W <- dim(knapsack$K)[1]
      items <- c()
      col <- dim(knapsack$K)[2]
      selected_item <- knapsack$Item[W, col]
      while(selected_item != '')
      {
        selected_item <- knapsack$Item[W,col]
        if(selected_item != '')
        {
          selected_item_value <- Data[Data$item == selected_item]			
          W <- W - selected_item_value$weight
          items <- c(items,selected_item)			
          col <- col - selected_item_value$volume
        }
      }
      return(items)
    }
    
    main_knapsack <- function(Data, W, Volume)
    {
      knapsack_result <- Un_knapsack(Data,W,Volume)
      items <- table(retrieve_info(knapsack_result, Data))
      K <- knapsack_result$K[W+1, Volume+1]
      cat(paste('The Total profit is: ', K, '\n'))
      cat(paste('You must carry:', names(items), '(x',items, ') \n'))
    }
    
    ## Make sure you have all units right! For ex, for MaxWeight and MaxVolume! (be consistent)
    return(main_knapsack(Data_, MaxWeight, MaxVolume))
    
  }
