# Clear console (Can be done by pressing ctrl/cmd + L)
cat("\014")
# Clear environment
rm(list=ls())
# Get working directory
getwd()

source("allFunctions.R")



# Get unnecessary columns to be removed:
unnecessaryColumns <- c("sofifa_id","player_url","dob","short_name",
                        "real_face","nation_position",
                        "nation_jersey_number","mentality_composure","loaned_from",
                        "team_position","team_jersey_number","joined","contact_valid_until")


##################################### FIFA 20 #####################################
f20_complete <- read.csv("Dataset/players_20.csv")
f20 <- f20_complete[,!(names(f20_complete) %in% unnecessaryColumns)]
f20 <- handleNonNumericAttributes(f20)
f20 <- addPositionColumn(f20)


logisticRegression_Position(f20)


# Age Graphs:

g_age <- ggplot(data = f20, aes(age))

g_age + geom_histogram(bins = 25,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 20:Distribution based on Age")

g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 20:Distribution based on Age and Position")

ggplot(f20, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 20:Distribution based on General Playing Position")


# Top 10 countries info:

graphTopCountries(f20)


# Draw Top 10 Countries on worldmap:

countries_count <- count(f20, nationality)
top10 <- top_n(countries_count, 10, n)
top10 <- as.data.frame(top10)
# Work-Around to graph UK
levels(top10$nationality) <- c(levels(top10$nationality), "UK") 
top10$nationality[top10$nationality == "England"]  <- "UK"

fr <- joinCountryData2Map(dF = top10,joinCode = "NAME",nameJoinColumn = "nationality",verbose=F) # Prepare data to plot

mapCountryData(mapToPlot = fr,nameColumnToPlot = "nationality",
               catMethod = "fixedWidth",
               oceanCol = "steelblue1",
               missingCountryCol = "white",
               mapTitle = "Number of players per Country",
               colourPalette = "heat",
               aspect = "variable",
               addLegend = F)


# Wage & Value Statistics:

f20 <- addWageandValueLevels(f20)
plotWagesMoreThan100k(f20)
plotValueAbove30M(f20)


# Overall & Age relation:

plotAgevsOverall(f20)


# Club Value:

plotTopClubValue(f20)


# SVM Position Prediction:

CMSVM_F20 <- predictSVM(f20)
CMSVM_F20


# Random Forest Position Prediction:

CMRF_F20 <- predictRandomForrest(f20)
CMRF_F20


# Attribtes Correlation HeatMap:

plotCorrelationHeatMap(f20)


# Best team in FIFA based on overall:

plotBest442("20")
plotBest352("20")
plotBest433("20")

F20_442 <- getBestTeamForFormation("20", "4-4-2")
F20_352 <- getBestTeamForFormation("20", "3-5-2")
F20_433 <- getBestTeamForFormation("20", "4-3-3")

bestTeams20 <- list(F20_442, F20_352, F20_433)
bestTeams20[getBestTeamByOverall(F20_442, F20_352, F20_433)]


###################################### FIFA 16 --> FIFA 19 ######################################
# Read datasets
f16_complete <- read.csv("Dataset/players_16.csv")
f17_complete <- read.csv("Dataset/players_17.csv")
f18_complete <- read.csv("Dataset/players_18.csv")
f19_complete <- read.csv("Dataset/players_19.csv")


# Remove unnecessary columns
f16 <- f16_complete[,!(names(f16_complete) %in% unnecessaryColumns)]
f17 <- f17_complete[,!(names(f17_complete) %in% unnecessaryColumns)]
f18 <- f18_complete[,!(names(f18_complete) %in% unnecessaryColumns)]
f19 <- f19_complete[,!(names(f19_complete) %in% unnecessaryColumns)]

# Handle String attributes that caused errors:
# For Example: attacking_crossing, ls and similar attributes.
f16 <- handleNonNumericAttributes(f16)
f17 <- handleNonNumericAttributes(f17)
f18 <- handleNonNumericAttributes(f18)
f19 <- handleNonNumericAttributes(f19)

# Factorise player positions:
f16 <- addPositionColumn(f16)
f17 <- addPositionColumn(f17)
f18 <- addPositionColumn(f18)
f19 <- addPositionColumn(f19)

# Logistic Regression For Defense:
logisticRegression_Position(f16)
logisticRegression_Position(f17)
logisticRegression_Position(f18)
logisticRegression_Position(f19)

#Age Statistics:

# Fifa 16:
g_age <- ggplot(data = f16, aes(age))

g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 16:Distribution based on Age")

g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 16:Distribution based on Age and Position")

ggplot(f16, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 16:Distribution based on General Playing Position")


# Fifa 17:
g_age <- ggplot(data = f17, aes(age))

g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 17:Distribution based on Age")

g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 17:Distribution based on Age and Position")

ggplot(f17, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 17:Distribution based on General Playing Position")

# Fifa 18:
g_age <- ggplot(data = f18, aes(age))

g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 18:Distribution based on Age")

g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 18:Distribution based on Age and Position")

ggplot(f18, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 18:Distribution based on General Playing Position")

# Fifa 19:
g_age <- ggplot(data = f19, aes(age))

g_age + geom_histogram(bins = 25,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 19:Distribution based on Age")

g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 19:Distribution based on Age and Position")

ggplot(f19, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 19:Distribution based on General Playing Position")

# Top 10 Countries:

graphTopCountries(f16)
graphTopCountries(f17)
graphTopCountries(f18)
graphTopCountries(f19)

# Add Wage and value Labels:

f16 <- addWageandValueLevels(f16)
f17 <- addWageandValueLevels(f17)
f18 <- addWageandValueLevels(f18)
f19 <- addWageandValueLevels(f19)

# Wage Above 100k:

plotWagesMoreThan100k(f16)
plotWagesMoreThan100k(f17)
plotWagesMoreThan100k(f18)
plotWagesMoreThan100k(f19)


# Age vs Overall:

plotAgevsOverall(f16)
plotAgevsOverall(f17)
plotAgevsOverall(f18)
plotAgevsOverall(f19)

# Market Value:

plotValueAbove30M(f16)
plotValueAbove30M(f17)
plotValueAbove30M(f18)
plotValueAbove30M(f19)

# Club Values:

plotTopClubValue(f16)
plotTopClubValue(f17)
plotTopClubValue(f18)
plotTopClubValue(f19)

# Predict Position using SVM:

CMSVM_F16 <- predictSVM(f16)
CMSVM_F17 <- predictSVM(f17)
CMSVM_F18 <- predictSVM(f18)
CMSVM_F19 <- predictSVM(f19)


CMSVM_F16
CMSVM_F17
CMSVM_F18
CMSVM_F19

# Predict Position using Random Forrest:

CMRF_F16 <- predictRandomForrest(f16)
CMRF_F17 <- predictRandomForrest(f17)
CMRF_F18 <- predictRandomForrest(f18)
CMRF_F19 <- predictRandomForrest(f19)


CMRF_F16
CMRF_F17
CMRF_F18
CMRF_F19

# Correlation:

plotCorrelationHeatMap(f16)
plotCorrelationHeatMap(f17)
plotCorrelationHeatMap(f18)
plotCorrelationHeatMap(f19)


# Best Team In Each Year:

# FIFA 19:
plotBest442("19")
plotBest352("19")
plotBest433("19")

F19_442 <- getBestTeamForFormation("19", "4-4-2")
F19_352 <- getBestTeamForFormation("19", "3-5-2")
F19_433 <- getBestTeamForFormation("19", "4-3-3")

bestTeams19 <- list(F19_442, F19_352, F19_433)
bestTeams19[getBestTeamByOverall(F19_442, F19_352, F19_433)]

# FIFA 18:
plotBest442("18")
plotBest352("18")
plotBest433("18")

F18_442 <- getBestTeamForFormation("18", "4-4-2")
F18_352 <- getBestTeamForFormation("18", "3-5-2")
F18_433 <- getBestTeamForFormation("18", "4-3-3")

bestTeams18 <- list(F18_442, F18_352, F18_433)
bestTeams18[getBestTeamByOverall(F18_442, F18_352, F18_433)]

# It can be observed that teams with messi and cristiano are always the best formation:
# F20 Team is better than that of other years:
getBestTeamByOverall(F20_433, F19_433, F18_433)


######################### Lionel Messi VS Cristiano Ronaldo ###############################
# Lionel Messi VS Cristiano Ronaldo:

plotMessiVsCristiano()
