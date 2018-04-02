data = read.csv("games.csv")
data = na.omit(data)

# All genre games global sale
action = data[data$Genre == "Action",]
actionsale = sum(action$Global_Sales)
adventure = data[data$Genre == "Adventure",]
adventuresale = sum(adventure$Global_Sales)
fighting = data[data$Genre == "Fighting",]
fightingsale = sum(fighting$Global_Sales)
misc = data[data$Genre == "Misc",]
miscsale = sum(fighting$Global_Sales)
platform = data[data$Genre == "Platform",]
platformsale = sum(platform$Global_Sales)
puzzle = data[data$Genre == "Puzzle",]
puzzlesale = sum(puzzle$Global_Sales)
racing = data[data$Genre == "Racing",]
racingsale = sum(data[data$Genre == "Racing",]$Global_Sales)
rpg = data[data$Genre == "Role-Playing",]
rpgsale = sum(rpg$Global_Sales)
shooter = data[data$Genre == "Shooter",]
shootersale = sum(shooter$Global_Sales)
simulation = data[data$Genre == "Simulation",]
simulationsale = sum(simulation$Global_Sales)
sports = data[data$Genre == "Sports",]
sportsale = sum(sports$Global_Sales)
strategy = data[data$Genre == "Strategy",]
strategysale = sum(strategy$Global_Sales)

Global_Sales_ALL = c(actionsale, adventuresale, fightingsale, miscsale, platformsale, puzzlesale, racingsale, rpgsale, shootersale, simulationsale, sportsale, strategysale)
barplot(height = c(Global_Sales_ALL[1], Global_Sales_ALL[11], Global_Sales_ALL[9], Global_Sales_ALL[8], Global_Sales_ALL[7], Global_Sales_ALL[5], Global_Sales_ALL[4], Global_Sales_ALL[3], Global_Sales_ALL[10], Global_Sales_ALL[2], Global_Sales_ALL[6], Global_Sales_ALL[12]),names = c("action", "sport", "shoot", "rpg", "race", "platf", "misc", "fight", "sim", "adven", "puzz", "strat"),main="Global sales vs. Genre")

# year trend of games
all_1996 = data[data$Year_of_Release == "1996",]
all_1997 = data[data$Year_of_Release == "1997",]
all_1998 = data[data$Year_of_Release == "1998",]
all_1999 = data[data$Year_of_Release == "1999",]
all_2000 = data[data$Year_of_Release == "2000",]
all_2001 = data[data$Year_of_Release == "2001",]
all_2002 = data[data$Year_of_Release == "2002",]
all_2003 = data[data$Year_of_Release == "2003",]
all_2004 = data[data$Year_of_Release == "2004",]
all_2005 = data[data$Year_of_Release == "2005",]
all_2006 = data[data$Year_of_Release == "2006",]
all_2007 = data[data$Year_of_Release == "2007",]
all_2008 = data[data$Year_of_Release == "2008",]
all_2009 = data[data$Year_of_Release == "2009",]
all_2010 = data[data$Year_of_Release == "2010",]
all_2011 = data[data$Year_of_Release == "2011",]
all_2012 = data[data$Year_of_Release == "2012",]
all_2013 = data[data$Year_of_Release == "2013",]
all_2014 = data[data$Year_of_Release == "2014",]
all_2015 = data[data$Year_of_Release == "2015",]
all_2016 = data[data$Year_of_Release == "2016",]
all_2017 = data[data$Year_of_Release == "2017",]
year = c(1996:2017)
global_sale_year = c(sum(all_1996$Global_Sales), sum(all_1997$Global_Sales), sum(all_1998$Global_Sales), sum(all_1999$Global_Sales), sum(all_2000$Global_Sales), sum(all_2001$Global_Sales), sum(all_2002$Global_Sales), sum(all_2003$Global_Sales), sum(all_2004$Global_Sales), sum(all_2005$Global_Sales), sum(all_2006$Global_Sales), sum(all_2007$Global_Sales), sum(all_2008$Global_Sales), sum(all_2009$Global_Sales), sum(all_2010$Global_Sales), sum(all_2011$Global_Sales), sum(all_2012$Global_Sales), sum(all_2013$Global_Sales), sum(all_2014$Global_Sales), sum(all_2015$Global_Sales), sum(all_2016$Global_Sales), sum(all_2017$Global_Sales))
plot(year, global_sale_year, type = 'b', lwd = 2,main="Global sales vs. Year")

#in order to predict sales:
# 80% training data, 20% test data
set.seed(12345)
trainid = sample(1:nrow(data), size = trunc(nrow(data) * 0.8))
trainData = data[trainid,]
testData = data[-trainid,]
#Predict sale
# first basic method: lasso regression
library(glmnet)
set.seed(12345)
trainx = model.matrix(Global_Sales ~ . - NA_Sales - EU_Sales - JP_Sales - Other_Sales - Global_Sales - Name-Publisher-Year_of_Release, data = trainData)[, -1]
trainy = trainData$Global_Sales
testx = model.matrix(Global_Sales ~ . - NA_Sales - EU_Sales - JP_Sales - Other_Sales - Global_Sales - Name-Publisher-Year_of_Release, data = testData)[, -1]
testy = testData$Global_Sales
#use cross validation to choose the best lambda in lasso regression
cv.out = cv.glmnet(trainx, trainy, alpha = 1)
bestlambda = cv.out$lambda.min

#using the best lambda to do lasso regression:
model.lasso = glmnet(trainx, trainy, lambda = bestlambda,alpha = 1)
lasso_train_pred = predict(model.lasso, s = bestlambda, newx = trainx)
lasso_train_mse = mean((lasso_train_pred - trainy) ^ 2)
lasso_test_pred = predict(model.lasso, s = bestlambda, newx = testx)
lasso_test_mse = mean((lasso_test_pred - testy) ^ 2)
lasso_train_mse
lasso_test_mse
lasso_coef = predict(model.lasso, s = bestlambda, type = "coefficient")[1:200,]

# knn regression on user_score, critic_score, critic_count and user_count
k_range = c(10,20,30,40,50,60,70,80,90,100,110,150,200,500)
testMse_knn = c()
trainData$User_Score = as.numeric(trainData$User_Score)
testData$User_Score = as.numeric(testData$User_Score)
for (i in 1:length(k_range)) {
    knn_model = knn.reg(train=scale(trainData[, c(11, 12, 13, 14)]), test = scale(testData[, c(11, 12, 13, 14)]), y = trainData$Global_Sales, k = k_range[i])
    testMse_knn[i]=mean((knn_model$pred-testData$Global_Sales)^2)
}
testMse_knn
# The best k=100
knn_best_model = knn.reg(train = scale(trainData[, c(11, 12, 13, 14)]), test = scale(testData[, c(11, 12, 13, 14)]), y = trainData$Global_Sales, k = 100)
