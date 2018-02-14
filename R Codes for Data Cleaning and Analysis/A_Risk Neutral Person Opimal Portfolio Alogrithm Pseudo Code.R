##### WARNING ########

"
In this code the author made the mistake of not putting the total
sum constraint = 1, thus the total weights assigned to all products
are coming more than 1. Due to paucity of time, this mistake could not
be corrected. 
but in E_Periodic Rebalancing Algorithm the mistake has been corrected

"





#### 1a. Optimization for Risk Neutral Person #####

#Pseudo Code Implementation


# Input Needed for Optimization Function is 

# 1. A named Return vector whose value will denote the return
#    whose name will denote out of which 19 product it comes



# Reason for creating a new algoithm

"
We were not able to find a appropriate package which optimizes 
based on the given diversification cosntraints.

All though the problem can be solved by hand for risk neutal person
but we thought of writing a alrogithm as it may be useful for
periodic rebalancing part as well. 


"



ret #It could be like as follows
    # Here mean daily return has been converted to average return by
    # ((1+r)^365-1) where r is mean daily return

"
Price.A1      Price.A2       AA.bond       BB.bond 
   0.14738664    0.37879091    0.11111923    0.16876164 
CC.bond          Gold        Silver        Copper 
0.25331388    0.01707079    0.05047818   -0.05420841 
Chy.INR       GBP.INR       USD.INR Mutual.Fund.J 
-0.02734379   -0.05154641    0.02057410    0.32633019 
Mutual.Fund.K Mutual.Fund.L       Stock.A       Stock.B 
0.21201122    0.43748939    0.87137471    0.35634272 
Stock.C       Stock.D          Cash 
0.78154283    0.13427779    0.03500000 

"


# Creating a type function which will return the type of 
# asset product as follows

type <- function(x){
  
  if(x == 'Price.A1' | x == 'Price.A2'){
    return('Alt_Inv')
  } else if( x == 'AA.bond' | x  == 'BB.bond'| x == 'CC.bond'){
    return('Bonds')
  } else if( x == 'Gold' | x == 'Silver' | x == 'Copper'){
    return('Commodities')
  } else if (x == 'Chy.INR' | x  == 'GBP.INR' | x == 'USD.INR'){
    return('Forex')
  } else if( x == 'Mutual.Fund.J' | x == 'Mutual.Fund.K' | x == 'Mutual.Fund.L'){
    return('Fund')
  } else if( x == 'Stock.A' | x == 'Stock.B'| x == 'Stock.C' | x == 'Stock.D' ){
    return('Stock')
  } else {
    return('Cash')
  }
}



"
Creating a Selection Indicator Vector(Choosen) which 
will denote out given 19 products which one is choosen or not

And similaryly doing this for Asset classs as well by using
choosen.asset.class vector

"

choosen <- rep(0,19)
names(choosen) <- c('Price.A1','Price.A2',
                    'AA.bond','BB.bond','CC.bond',
                    'Gold','Silver','Copper',
                    'Chy.INR','GBP.INR','USD.INR',
                    'Mutual.Fund.J','Mutual.Fund.K','Mutual.Fund.L',
                    'Stock.A','Stock.B','Stock.C','Stock.D',
                    'Cash')
choosen



choosen.asset.class <- rep(0,7)
names(choosen.asset.class) <- c('Alt_Inv','Bonds','Commodities','Forex','Fund','Stock','Cash')
choosen.asset.class

# Creating a weight function for Product s
weight <- rep(0,19)
names(weight) <- c('Price.A1','Price.A2',
            'AA.bond','BB.bond','CC.bond',
            'Gold','Silver','Copper',
            'Chy.INR','GBP.INR','USD.INR',
            'Mutual.Fund.J','Mutual.Fund.K','Mutual.Fund.L',
            'Stock.A','Stock.B','Stock.C','Stock.D',
            'Cash')
weight


# Creating a weight function for Product
weight.asset.class <- rep(0,7)
names(weight.asset.class) <- c('Alt_Inv','Bonds','Commodities','Forex','Fund','Stock','Cash')
weight.asset.class


# Constraints
max.prod.weight <- 0.35
min.prod.weight <- 0.05
max.asset.class.weight <- 0.6

################################################################

# TO satisfy the constraints that products choosen should belong
# to three different classes we first begin by identifying the best
# product from each class and give it the maximum weight of 0.35

##############################################################3


# Finding the Max return in an Asset Class

max.asset.ret <- rep(-9e10,7)
names(max.asset.ret) <- c('Alt_Inv','Bonds','Commodities','Forex','Fund','Stock','Cash')

for(i in 1:19){
  n = names(ret[i]) #finding the name of product
  t = type(n)       #finding the type of product
  x = ret[i]        # saving the value of return in x
  y = max.asset.ret[t]
  if(x >= y){
    y = x
  }
  max.asset.ret[t] = y
}
max.asset.ret

#Ordering Max.return in a Asset Class

order.max.asset.ret = sort(max.asset.ret,decreasing =  T)
order.max.asset.ret


# Picking the First 3 Best Assets from All asset classes

for(i in 1:3){
  x = order.max.asset.ret[i]
  class.name = names(x)
  product.name = names(ret[ret == x]) #finding the name of that asset
  choosen[names(choosen) == product.name] = 1 #selecting that asset
  choosen.asset.class[names(choosen.asset.class) == class.name] = 1
  
  
}

# Giving weights to best choosen assets in asset class

weight[choosen == 1] = max.prod.weight
weight


weight.asset.class[choosen.asset.class == 1] = max.prod.weight 
weight.asset.class

#####################################################

# j = Total number of assets choosen

# Now after selecting the best 3 asset classes product we work
# solving the rest of problem where depending on the j = c(5,6,7)
# we solve for best products in it

########################################################



# Selecting the rest of returns from return vector
reduced.ret <- ret[choosen != 1]
reduced.ret


#Creating a emptiy list to save the results
result = list()
library(lpSolve)

for(j in 5:7){
  k = j - 3 # since three products has already been choosen
  ordered.reduced.ret <- sort(reduced.ret,decreasing = T)
  ordered.reduced.ret
  int = 0
  while(int < k) {
    for(i in 1:length(ordered.reduced.ret)){
      product.return = ordered.reduced.ret[i]
      return = calculate.return(product.return) # calculates assigned returns
      if(return > 0){
        int = int + 1
        product.name = names(product.return)
        class.name = type(n)
        choosen[names(choosen) == product.name] = 1
        weight[names(choosen) == product.name] = return
        choosen.asset.class[names(choosen.asset.class) == class.name] = 1
        }
    }
  }
  # Saving the results in results vector list
  result[[j]] = list(sum(ret*weight),ret,weight,choosen,choosen.asset.class)
}
  
  
calculate.return <- function(product.return){
  n = names(product.return)
  t = type(n)
  choosen.asset.class.name = names(choosen.asset.class)[(choosen.asset.class == 1)]
  if(t %in% choosen.asset.class.name){
    a = max.asset.class.weight
    b = max.prod.weight
    c = min.prod.weight
    r = product.return
    cat.r = weight.asset.class[t]
    obj.fun = c(1)
    constraints = matrix(c(1,0,1,0,1,1),ncol = 2, byrow = T)
    constr.dir = c('<=','>=','<=')
    rhs = c(b,c,a)
    sol = lp('max',obj.fun,constraints,constr.dir,rhs)
    return(sol$objval)
    weight.asset.class[t] = weight.asset.class[t] + sol$objval
  } else{
    return(max.prod.weight)
  }
}

# Finding the Best Result for all j values

bestresults <- function(result){
  a = results[[5]]
  b = results[[6]]
  c = results[[7]]
  if(a == max(a,b,c)){
    return(5)
  } else if( b == max(a,b,c)){
    return(6)
  } else{
    return(7)
  }
}

optimal_j = bestresults(result)
optimal_result_j = result[[j]]



##### SOLVING THE PROBLME MANUALLy ######


ret
ordered.returns = sort(ret, decreasing = T)
ordered.returns

weight['Stock.A'] = 0.35
weight['Mutual.Fund.L'] = 0.35
weight['Price.A2'] = 0.35
weight['Stock.C'] = 0.6-0.35
weight['Mutual.Fund.J'] = 0.6-0.35
weight['CC.bond'] = 0.35
weight['BB.bond'] = 0.6-0.35

weight


optimalweight = weight
sum(ret*optimalweight)



ret[optimalweight> 0]
