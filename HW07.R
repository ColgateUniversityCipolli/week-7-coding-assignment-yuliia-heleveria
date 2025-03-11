#where do we use size?
#will it be 0 and 1 for beta
#or should we use dbeta
#what do I put in the writeup

#Question 1 - pois.prob function
pois.prob <- function(x, lambda, type="<="){
  #check if x is valid for Poisson distribution
  if (x<0 | x != floor(x)){
    return("Invalid argument for 'x'")
  }
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

#Question 2 - beta.prob function
beta.prob <- function(x, alpha, beta, type="<="){
  #check if x is valid for Beta distribution
  if (x<0 | x>1){
    return("Invalid argument for 'x'")
  }
  if (type == "="){ #compute P(X=x)
    return(0) #continuous distribution
  }else if(type == "!="){  #compute P(X!=x)
    return(1) #continuous distribution
  }else if(type == "<"){  #compute P(X<x)
    return(pbeta(x, alpha, beta))
  }else if(type == "<="){ #compute P(X<=x)
    return(pbeta(x, alpha, beta))
  }else if(type == ">"){  #compute P(X>x)
    return(1 - pbeta(x, alpha, beta))
  }else if (type == ">="){  #compute P(X>=x)
    return(1 - pbeta(x, alpha, beta))
  }else{
    return("Invalid argument for 'type'")
  }
}

#Test the functions
#testing pois.prob for each possible return
print(pois.prob(5,2,"="))
print(pois.prob(5,2,"!="))
print(pois.prob(5,2,"<"))
print(pois.prob(5,2,"<="))
print(pois.prob(5,2,">"))
print(pois.prob(5,2,">="))
print(pois.prob(5,2,"<-"))
print(pois.prob(-5,2,"<"))
#testing beta.prob for each possible return
print(beta.prob(0.5,2,5,"="))
print(beta.prob(0.5,2,5,"!="))
print(beta.prob(0.5,2,5,"<"))
print(beta.prob(0.5,2,5,"<="))
print(beta.prob(0.5,2,5,">"))
print(beta.prob(0.5,2,5,">="))
print(beta.prob(0.5,2,5,">8"))
print(beta.prob(7,2,5,">="))