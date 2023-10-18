###R code for demand modeling examples

rm(list=ls()) #clears workspace

## Set working directory. Make sure you use / instead of \ in the address. 
## CHANGE THE PATH BELOW ACCORDING TO WHERE YOU HAVE STORED/DOWNLOADED THE DATA FILES oj_data.csv and Catalog Data.csv
setwd("~/Dropbox/Teaching/BUS AN 579 Fall 2023")
#Can also do this manually by going to Session -> Set Working Directory -> Choose Directory in R Studio
#Choose the directory where you have downloaded the oj_data.csv file
#For Windows the path may be something like
#setwd("C:/Users/Shirsho/Teaching/BUS AN 579 2023")# set working directory 

## Load the data
df = read.csv("oj_data.csv") 
#This data set contains the weekly sales and prices of a few orange juice products at a few stores
head(df)
tail(df)

#Plot the data
plot(x = df$Sales,  ## x-coordinates
     y = df$Price,  ## y-coordinates
     type = "p",    ## type of the graph ("p"= points, "l" = line)
     cex=1,         ## Size of the point 
     col = "black", ## color of the point
     xlab = "Sales",  ##  label on x-axis
     ylab = "Price",       ##  label on y-axis
     main = "Raw Price-Sales plot")

## Add logged variables to the data frame 
df$logSales = log(df$Sales)
df$logPrice = log(df$Price)

##################### Regression Analysis ######################

# 1. How to do OLS estimation 
# 2. How to plot the data and fitted values  

# Here, we try 4 different models:
# Model 1: out_reg : logSales = intercept + b1*logPrice + error
# Model 2: out_reg2 : logSales = intercept + b1*logPrice + b2*DisplayDummy + error
# Model 3: out_reg3 : logSales = intercept + b1*logPrice + b2*ProductDummy + b3*StoreDummy +b4*DisplayDummy +  error
# Model 4: out_reg4 : logSales = intercept + b1*logPrice + b2*(logPrice*StoreDummy)+ b3*ProductDummy + b4*StoreDummy +b5*DisplayDummy  + error
################################################################


#Baseline regression
## run a regression of logSales on logPrice
out_reg = lm(logSales ~ logPrice, data = df) #Model 1
summary(out_reg)

#Show the Display Effect
df$Display_cat= factor(df$Display)
## Now play with colors - Color by product
plot(x = df$Sales, 
     y = df$Price,
     type = "p",    ## type of the graph ("p"= points, "l" = line)
     cex = 1,     ## shape of the point. filled circle 
     pch = 16, #filled circles
     col = df$Display_cat,  ## color will differ depending on whether there is a promotional display
     xlab = "Sales",  ##  label on x-axis
     ylab = "Price",       ##  label on y-axis
     main = "In-aisle Display Effect")
legend("topright", ### location of legend
       legend = unique(df$Display_cat),
       col=1:length(df$Display_cat),
       pch=16)

#Making a display dummy variable
df$DisplayDummy = 1*(df$Display == "Display")

## introduce a dummy variable for display
out_reg2 = lm(logSales ~ logPrice + DisplayDummy, data = df) #Model 2
summary(out_reg2)

#Show the Store Effect
df$Store_cat= factor(df$Store_id)
## Now play with colors - Color by product
plot(x = df$Sales, 
     y = df$Price,
     type = "p",    ## type of the graph ("p"= points, "l" = line)
     cex = 1,     ## shape of the point. filled circle 
     pch = 16, #filled circles
     col = df$Store_cat,  ## color will differ depending on whether there is a promotional display
     xlab = "Sales",  ##  label on x-axis
     ylab = "Price",       ##  label on y-axis
     main = "Store Effect")
legend("topright", ### location of legend
       legend = unique(df$Store_cat),
       col=1:length(df$Store_cat),
       pch=16)

#Show the Product Effect
df$Product_cat= factor(df$Product_id)
## Now play with colors - Color by product
plot(x = df$Sales, 
     y = df$Price,
     type = "p",    ## type of the graph ("p"= points, "l" = line)
     cex = 1,     ## shape of the point. filled circle 
     pch = 16, #filled circles
     col = df$Product_cat,  ## color will differ depending on whether there is a promotional display
     xlab = "Sales",  ##  label on x-axis
     ylab = "Price",       ##  label on y-axis
     main = "Product Effect")
legend("topright", ### location of legend
       legend = unique(df$Product_cat),
       col=1:length(df$Product_cat),
       pch=16)


## Make a dummy variable for store and product
df$Product1 = 1*(df$Product_id == 1)
df$Store2 = 1*(df$Store_id == 2)


#Model 3
out_reg3 = lm(logSales ~ logPrice + Product1+ Store2 + DisplayDummy, data=df)
summary(out_reg3)

#plot the fitted values
df$fitted_logSales_m3 = out_reg3$fitted.values

plot(x = df$fitted_logSales_m3,
     y = df$logPrice,
     xlab = "log(Sales)",
     ylab = "log(Price)",
     col = 'red',
     type = 'p',
     cex = 1,
     pch = 16)


points(x = df$logSales, 
       y = df$logPrice, 
       cex=0.5, 
       col='blue')
legend("bottomleft",
       legend = c("Fitted values", "Data"),
       col= c("red", "blue"),
       pch= c(16,1))


###### Plot the fitted values vs. actual data - exponentiating the log variables
plot(x = exp(df$fitted_logSales_m3),
     y = exp(df$logPrice),
     xlab = "Sales",
     ylab = "Price",
     col = 'red',
     type = 'p',
     cex = 1,
     pch = 16)
## Adding a layer of actual data points 
points(x = exp(df$logSales), 
       y = exp(df$logPrice), 
       cex=0.5, 
       col='blue')
legend("topright",
       legend = c("Fitted values", "Data"),
       col= c("red", "blue"),
       pch= c(16,1))


demand_results = out_reg3$coefficients # estimated coefficients from the model 
out_reg3$coefficients
demand_oj = function(Price, Product1, Store2) {
  ## demand function baesd on the regression results
  ## Which equation does this refer to?
  ## Which data variables is this function using? 
  Q = exp(demand_results[1] + demand_results[2]*log(Price) + demand_results[3]*Product1 + demand_results[4]*Store2)
  Q[which(Q < 0)] = 0 # to make sure that demand is not negative
  return(as.numeric(Q))
}

## plot predicted demand
price_grid = seq(
  from = 0.05,
  to = .4, 
  by = .001) ## create a grid of prices

## what is the demand for these prices for Product 3 at Store 2
demand_grid1 = demand_oj(price_grid, 0, 1) 
## what is the demand for these prices for Product 3 at Store 137
demand_grid2 = demand_oj(price_grid, 0, 0) 
## what is the demand for these prices for Product 1 at Store 2
demand_grid3 = demand_oj(price_grid, 1, 1) 
## what is the demand for these prices for Product 1 at Store 137
demand_grid4 = demand_oj(price_grid, 1, 0) 

plot(x = price_grid, 
     y = demand_grid1, 
     xlab = "Price", 
     ylab = "Quantity", 
     type ="l",# linetype. 1 = solid line. Default option is 1. 
     lty = 1, 
     main = "Demand functions") ## now type is line 

lines(x = price_grid, 
      y = demand_grid2,
      col = "red") ## add a layer of actual data points 

lines(x = price_grid, 
      y = demand_grid3,
      col = "green") ## add a layer of actual data points 
