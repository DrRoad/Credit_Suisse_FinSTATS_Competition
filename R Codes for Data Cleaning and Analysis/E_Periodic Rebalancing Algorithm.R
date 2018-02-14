

"
Instructions for Rebalancing Strategy

- The frequenc of periodic rebalancing would be months. As doing it daily
  or weekly may be too costly in terms of time and transaction costs
- Optimal weight for given set of assets would be decided monthly
  on the basis of price movements. Its like running optimization algorithm
  at the end of every month. And then allocating weights
- profit function
  previous weght and new weight
  daily optimization. 
- transaction cost
- ignoring the constraints given in appendix about stocks, bonds, mutual funds
  alternative investments


"


# Constraints


"
1. Need to rebalance within the already choosen
   asset products
2. Max.asset.class.weight = 0.6 and max.asset.product. weight = 0.35
3. No diversification constraints
4. No min.asset.product.weight constraints
5. No constraints on holding cash


"





##### PERIODIC REBALANCING FOR Risk Neutral Person #####

weightassigned #named vector
var = names(weightassigned)
var

"
weightassigned
     Price.A2       BB.bond       CC.bond Mutual.Fund.J 
0.35          0.25          0.35          0.25 
Mutual.Fund.L       Stock.A       Stock.C 
0.35          0.35          0.25 

"

# Calculating intial weights ######

# Calculating w_0

df3 = data[2:860,c(1,3,5,6,13,15,16,18)]
str(df3)

obj.fun = colMeans(df3[,-1])
obj.fun
constraints = matrix(0,ncol = 7,nrow = 11)
colnames(constraints) = names(weightassigned)

constraints

# Writing Constraints

# 1-7 Writing individual products constraints
for(i in 1:7){
  constraints[i,i] =1
}

#8 Bond asset class constraint
constraints[8,2] =1
constraints[8,3] =1

#9 Mutual Fund Constraints
constraints[9,4] =1
constraints[9,5] =1

#10 Stock Constraints
constraints[10,6] =1
constraints[10,7] =1

#11 All weights to be less than
for(i in 1:7){
  constraints[11,i] = 1
}

constraints

# Constriant Direction

constr.dir = rep('<=',11)
constr.dir

rhs = c(rep(0.35,7),rep(0.6,3),1)
rhs


prod.sol <- lp('max', obj.fun,constraints,constr.dir,rhs,compute.sens = T)
mean.daily.return = prod.sol$objval
mean.annual.return = dailytoannual(mean.daily.return)
mean.annual.return

sol = prod.sol$solution
names(sol) = var

w_0 = sol
w_0

#############



#loading information of Asset product choosen

df2 = data[861:length(data$Date),c(1,3,5,6,13,15,16,18)]
str(df2)

'
data.frame():	63 obs. of  8 variables:
 $ Date         : chr  "03-07-2017" "04-07-2017" "05-07-2017" "06-07-2017" ...
$ Price.A2     : num  -0.000369 0.000368 0.000418 -0.003942 0.001057 ...
$ BB.bond      : num  1.29e-03 3.51e-05 3.11e-04 4.25e-04 1.98e-04 ...
$ CC.Bond      : num  0.001104 0.000306 0.000543 0.000391 0.00056 ...
$ Mutual.Fund.J: num  0.00735 0 0.0073 0 0 ...
$ Mutual.Fund.L: num  0.01319 -0.01247 0.01043 -0.00652 0.02516 ...
$ Stock.A      : num  0.00468 -0.01177 0.00903 -0.01076 0.00118 ...
$ Stock.C      : num  0.0269 0.00303 0.00776 -0.01347 0.00589 ...

'

# Choosing 3 dates for running periodic rebalancing algorithm
d1 = df2[15,1] 
d1 #21-07-2017

d2 = df2[30,1]
d2 # 11-08-2017

d3 = df2[45,1]
d3 # 5-09-2017


w_0 = weightassigned
w_0 #intial weights

# Calculating returns vector for 3 dates using mean of returns between
# dates

ret_d1 = colMeans(df2[1:15,-1])
ret_d1

ret_d2 = colMeans(df2[16:30,-1])
ret_d2

ret_d3 = colMeans(df2[31:45,-1])
ret_d3

return_list = list(ret_d1,ret_d2,ret_d3)
return_list[1]

# Using Linear programming approach to calculate the weights w_d1,w_d2
# w_d3

library(lpSolve)

sol.list = list() # Empty list to save the results
mean.daily.return.list = list()
mean.annual.return.list = list()



##### For ret_d1, calculating w_d1
  
obj.fun = ret_d1


prod.sol <- lp('max', obj.fun,constraints,constr.dir,rhs,compute.sens = T)
mean.daily.return = prod.sol$objval
mean.annual.return = dailytoannual(mean.daily.return)
mean.annual.return

sol = prod.sol$solution
names(sol) = var
sol

sol.list[[1]] = sol
mean.daily.return.list[[1]] = mean.daily.return
mean.annual.return.list[[1]] = mean.annual.return




##### For ret_d2, calculating w_d2

obj.fun = ret_d2



prod.sol <- lp('max', obj.fun,constraints,constr.dir,rhs,compute.sens = T)
mean.daily.return = prod.sol$objval
mean.annual.return = dailytoannual(mean.daily.return)
mean.annual.return

sol = prod.sol$solution
names(sol) = var
sol

sol.list[[2]] = sol
mean.daily.return.list[[2]] = mean.daily.return
mean.annual.return.list[[2]] = mean.annual.return





##### For ret_d3, calculating w_d3

obj.fun = ret_d3


prod.sol <- lp('max', obj.fun,constraints,constr.dir,rhs,compute.sens = T)
mean.daily.return = prod.sol$objval
mean.annual.return = dailytoannual(mean.daily.return)
mean.annual.return

sol = prod.sol$solution
names(sol) = var
sol

sol.list[[3]] = sol
mean.daily.return.list[[3]] = mean.daily.return
mean.annual.return.list[[3]] = mean.annual.return



#Seeing the Results

sol.list
mean.annual.return.list
mean.daily.return.list

##### Transaction Cost ######################

# Profit Function incorporating the transaction cost

buyingcost = c(1,1.001,1.001,1,1,1.001,1.001)
names(buyingcost) = var

buyingcost

sellingcost = c(1,1.001,1.001,1.01,1.01,1.001,1.001)
names(sellingcost) = var

sellingcost


money_0 = 1e7
money_0
budget.alloc_0 = w_0*money_0
budget.alloc_0


rawdata <- read.csv('data_corrected.csv',na.strings = '#N/A')
str(rawdata)
colnames(rawdata)

rawdata$Date = as.character(rawdata$Date)
rawdata$Silver = as.numeric(rawdata$Silver)
rawdata$Mutual.Fund.J = as.numeric(rawdata$Mutual.Fund.J)

Price_0 = rawdata[860,c(3,5,6,13,15,16,18)]
Price_0

Price_1 = rawdata[rawdata$Date == d1,c(3,5,6,13,15,16,18)]
Price_1

Price_2 = rawdata[rawdata$Date == d2,c(3,5,6,13,15,16,18)]
Price_2

Price_3 = rawdata[rawdata$Date == d3,c(3,5,6,13,15,16,18)]
Price_3


portfolio.alloc_0 = round(budget.alloc_0/(Price_0*buyingcost),0) 
portfolio.alloc_0


#### Transaction at d1 date

# Selling all the assets
money_d1 = sum(portfolio.alloc_0*Price_1)
money_d1

w_1 = sol.list[[1]]
w_1
budget.alloc_1 = w_1*money_d1

portfolio.alloc_1 = round(budget.alloc_1/(Price_1*buyingcost),0) 
portfolio.alloc_1

sum((portfolio.alloc_1 - portfolio.alloc_0)*Price_1)



#### STOPPING HERE COULD NOT DO MORE THAN THIS 


##########################################################

#               FOR RISK AVERSE INDIVIDUAL

##################################################



