

# Loading the Data

str(data)
data[860,]
data[data$Date == '30-06-2017',]


future.data = data[861:length(data$Date),-20]
str(future.data)

future.data[1,]

df = future.data
y = colMeans(df[,-1])

y

AR = y

dailytoannual(0.0003767473)


for(i in 1:length(y)){
  AR[i] = dailytoannual(y[i])
}

AR
names(AR)

ret = c(AR,0.035) # return adding for cash
names(ret) = c('Price.A1','Price.A2',
               'AA.bond','BB.bond','CC.bond',
               'Gold','Silver','Copper',
               'Chy.INR','GBP.INR','USD.INR',
               'Mutual.Fund.J','Mutual.Fund.K','Mutual.Fund.L',
               'Stock.A','Stock.B','Stock.C','Stock.D',
               'Cash')

ret

##### For Risk Neutral Person 

# Return Calculation
Actual.Return.annual = sum(optimalweight*ret)
Actual.Return.annual

actual.return.daily = sum(weightassigned*colMeans(df2))
actual.return.daily

dailytoannual(actual.return.daily)


# Risk Calculation

str(df)
fil = names(optimalweight)[optimalweight > 0]
weightassigned = optimalweight[fil]
weightassigned

fil


colnames(df)
df.sub = subset(df,select = )


df2 = df[,c(3,5,6,13,15,16,18)]
str(df2)

vcov.mat = var(df2)
df2


portfolio.variance = t(weightassigned) %*% vcov.mat %*% weightassigned
portfolio.sd =sqrt(portfolio.variance)

portfolio.sd


t(weightassigned)
