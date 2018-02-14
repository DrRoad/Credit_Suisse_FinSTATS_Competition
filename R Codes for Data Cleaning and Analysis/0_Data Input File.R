# Site



# Importing the Data

returns <- read.csv('return.csv')
data <- read.csv('data.csv',na.strings = '#N/A')
colnames(returns)

#Droping the first line


str(returns)
str(data)



# Correcting the type of data

data$Date = as.character(data$Date)
data$Silver = as.numeric(data$Silver)
data$Mutual.Fund.J = as.numeric(data$Mutual.Fund.J)




# To find columns with NA values
for (i in 1:20){
  vec[i] = any(is.na(data[,i]))
  #vec_row[i] = which(is.na(data[,i]))
}

any(is.na(data[,4])) #True
any(is.na(data[,14]))
which(is.na(data[,14]))

data[199,] # rows with na values
data[57,] 

# Imputing the NA Values 


#using the average 
colnames(data)
data[199,4] = mean(data[c(198,200),4])
data[199,5] = mean(data[c(198,200),5])
data[199,6] = mean(data[c(198,200),6])
data[57,14] = mean(data[c(56,58),14])


any(is.na(data))

write.csv(data, file = 'data_corrected.csv',row.names = F)

cor(data[,-1])

str(data)

data2 <- data



# Calculating the Daily Returns for all stock prices
library('quantmod')

return.data <- data2

for (i in 2:20){
  x = data[,i]
  y = as.numeric(Delt(x,type = 'arithmetic'))
  return.data[,i] = y
}

str(return.data)

data = return.data


df <- return.data[2:860,-20] # removing the first NA row and Index Column
df[859,]


# Finally I now have daily returns to use 




# Converting daily to annual return 

dailytoannual = function(x){
  return((1 + x)^365-1)
}

str(df)
y = colMeans(df[,-1])

y

AR = y

dailytoannual(0.0003767473)


for(i in 1:length(y)){
  AR[i] = dailytoannual(y[i])
}

AR
names(AR)
AR['Price.A1']


# Creating Variance Covariance Matrix

A = var(df[,-1])

A

vars = c('Price.A1','Price.A2',
         'AA.bond','BB.bond','CC.bond',
         'Gold','Silver','Copper',
         'Chy.INR','GBP.INR','USD.INR',
         'Mutual.Fund.J','Mutual.Fund.K','Mutual.Fund.L',
         'Stock.A','Stock.B','Stock.C','Stock.D')

var_ret = y

names(Var_ret)

for (z in vars){
  var_ret[z] = A[z,z]
}

names(AR)

ret = c(AR,0.035) # return
names(ret) = c('Price.A1','Price.A2',
               'AA.bond','BB.bond','CC.bond',
               'Gold','Silver','Copper',
               'Chy.INR','GBP.INR','USD.INR',
               'Mutual.Fund.J','Mutual.Fund.K','Mutual.Fund.L',
               'Stock.A','Stock.B','Stock.C','Stock.D',
               'Cash')
names(ret)
var_ret # Variance


