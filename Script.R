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
plot(year,global_sale_year)
best_1996=all_1996[which(all_1996$Global_Sales == max(all_1996$Global_Sales)),]
best_1997=all_1997[which(all_1997$Global_Sales == max(all_1997$Global_Sales)),]