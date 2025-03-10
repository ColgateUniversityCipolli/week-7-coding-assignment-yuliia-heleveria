#where do we use size?

#Question 1 - pois.prob function
pois.prob <- function(x, lambda, type="<="){
  if (type == "="){ #compute P(X=x)
    return(dpois(x, lambda))
  }else if(type == "!="){  #compute P(X!=x)
    return(1 - dpois(x, lambda))
  }else if(type == "<"){  #compute P(X<x)
    return(ppois(x -1, lambda))
  }else if(type == "<="){ #compute P(X<=x)
    return(ppois(x, lambda))
  }else if(type == ">"){  #compute P(X>x)
    return(1 - ppois(x,lambda))
  }else if (type == ">="){  #compute P(X>=x)
    return(1 - ppois(x -1,lambda))
  }else{
    return("Invalid argument for 'type'")
  }
}