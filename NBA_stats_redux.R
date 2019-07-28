library(dplyr)
library(ggplot2)
library(caTools)
library(reshape2)
seasonStats <- read.csv("Seasons_Stats.csv")

seasonStats2018 <- read.csv("nbastats2018CSV.csv")
seasonStats2018 <- seasonStats2018 %>%
  mutate(PPG = PTS/G)

seasonStats2017 <- seasonStats %>% filter(Year == 2017)
head(seasonStats2017)

#getting rid of useless variables + creating a Points Per Game Variable
seasonStats2017 <- seasonStats2017 %>% dplyr::select(-blanl, -blank2)

seasonStats2017 <- seasonStats2017 %>% mutate(PPG = PTS/G)

seasonStats2017 <- seasonStats2017 %>% dplyr::select(-X, -Year)

#players who were traded have values for each of the teams they played
#for and their total values, and all i want is their total values.
#so I deleted the values of each specific team they played for
indicesOfDuplicated2017 <- which(duplicated(seasonStats2017$Player))
seasonStats2017 <- seasonStats2017[-c(indicesOfDuplicated2017), ]


colSums(is.na(seasonStats2017))



seasonStats2017<- seasonStats2017 %>% filter(PPG > 0)

#splitting the data into training/test Set
set.seed(121)
sample = sample.split(seasonStats2017, SplitRatio = 0.8)

trainingSetFor2017 <- subset(seasonStats2017, sample == TRUE)
testingSetFor2017 <- subset(seasonStats2017, sample == FALSE)


#I tried looking for outliers using normal numbers, but that failed.
#So now let's try with statistics that are percentage-based

#the numbers for all of these are < Q1-1.5IQR or > Q3 + 1.5IQR, if they exist


#first up is TS.
summary(trainingSetFor2017$TS.)

trainingSetFor2017OutliersForTS. <- trainingSetFor2017 %>%
  filter(TS. < .38995 | TS. > .69195)

#there are 21 players who are outliers here, but 2 of the 21 players played 
#>250 minutes. Remember, I arbitrarily chose 250 minutes as the cutoff for players
#who are actually good and ones who got unlucky/lucky with their outlier numbers,
#so I'll get rid of the <250 players

which(trainingSetFor2017$TS. < .38995 | trainingSetFor2017$TS. > .69195)
trainingSetFor2017 <- trainingSetFor2017[-c(67, 118, 142, 144, 170, 178, 241, 257, 
                                            262, 277, 290, 314, 329, 330, 332, 341, 
                                            352, 368, 385), ]

#next is X3PAr -- these are only upper outliers here, so let's check them out
#this guy played 44 minutes, so he's out
summary(trainingSetFor2017$X3PAr)
trainingSetFor2017OutliersForX3PAr <- trainingSetFor2017 %>%
  filter(X3PAr > .9125)

which(trainingSetFor2017$X3PAr > .9125)
trainingSetFor2017 <- trainingSetFor2017[-c(356),]

#let's check Free Throw Rate -- only high outliers -- 7/11 of the outliers
#have <250 minutes, so we can get rid of them
summary(seasonStats2017$FTr)
hist(trainingSetFor2017$FTr)
trainingSetFor2017OutliersForFTr <- trainingSetFor2017 %>%
  filter(FTr > .59875)
which(trainingSetFor2017$FTr > .59875)
trainingSetFor2017 <- trainingSetFor2017[-c(64, 117, 121,272, 317,335,357),]


#ORB. -- only high outliers

summary(trainingSetFor2017$ORB.)
trainingSetFor2017OutliersForORB. <- trainingSetFor2017 %>%
  filter(ORB. > 16.2)
which(trainingSetFor2017$ORB. > 16.2)
trainingSetFor2017 <- trainingSetFor2017[-c(148, 295),]

#DRB. -- all these outliers played >250 minutes, so I will keep all of them in
summary(trainingSetFor2017$DRB.)
trainingSetFor2017OutliersForDRB. <- trainingSetFor2017 %>%
  filter(DRB. > 33.3)

##now Total Rebound Percentage -- higher outliers but not lower ones
#again, like I figured, all these outliers have > 250 minutes. These guys
#were also some of the guys who had outlier DRB. numbers, which is what you would
#expect given that it's much easier to get DRB than ORB

summary(trainingSetFor2017$TRB.)
trainingSetFor2017HighTRB. <- trainingSetFor2017 %>%
  filter(TRB. > 23.7)

#next is Assist Percentage -- only high outliers, but it's interesting that out of the 16
#outliers, 14 of them played big minutes, meaning these guys are, for lack of a more sophisticated 
#response, really really good at creating assists

summary(trainingSetFor2017$AST.)
trainingSetFor2017HighAST. <- trainingSetFor2017 %>%
  filter(AST. > 33.9)
which(trainingSetFor2017$AST. > 33.9)
trainingSetFor2017 <- trainingSetFor2017[-c(102, 162),]

#next is Steal% -- 5/13 both low and high steal outliers who played
#under <250
summary(trainingSetFor2017$STL.)
trainingSetFor2017OutliersForSTL. <- trainingSetFor2017 %>%
  filter(STL. < .05 | STL. > 2.85)
which(trainingSetFor2017$STL. < .05 | trainingSetFor2017$STL. > 2.85)
trainingSetFor2017 <- trainingSetFor2017[-c(27, 131, 154, 173, 255),]

#Block% -- 10 high outliers, but all of them played >250 minutes
summary(trainingSetFor2017$BLK.)
trainingSetFor2017OutliersForBLK. <- trainingSetFor2017 %>%
  filter(BLK. > 5.6)

#Turnover % -- 6/7 guys played < 250 min
summary(trainingSetFor2017$TOV.)
trainingSetFor2017OutliersForTOV.. <- trainingSetFor2017 %>%
  filter(TOV. < 1.48 | TOV. > 23.4)
which(trainingSetFor2017$TOV. < 1.48 | trainingSetFor2017$TOV. > 23.4)
trainingSetFor2017 <- trainingSetFor2017[-c(183, 223, 260, 268, 313, 339),]

#USG. -- Westbrook was the lone outlier, go figure haha

summary(trainingSetFor2017$USG.)
trainingSetFor2017OutliersForUSG. <- trainingSetFor2017 %>%
  filter(USG. > 36.5)

#ok that's it for percentages, let's check advanced stats (though I do not think
#most of the outliers will be the best players)

#WS -- all high outliers of good players
summary(trainingSetFor2017$WS)
trainingSetForHighWS <- trainingSetFor2017 %>%
    filter(WS > 9.05)

#DWS -- all high outliers for good players, but interestingly, Westbrook
#was one of these outliers... but he's a bad defender? 
summary(trainingSetFor2017$DWS)
trainingSetForHighDWS <- trainingSetFor2017 %>%
  filter(DWS > 4.1)

#OWS -- 22 outliers, but they all played big minutes/were actually good
#tho Gallinari was on here -- he played 63 games this season, which was actually
#more than what KD played (62)! I know KD got hurt that year and missed a portion of 
#the season, but damn son
summary(trainingSetFor2017$OWS)
trainingSetForHighOWS <- trainingSetFor2017 %>%
  filter(OWS > 5.6625)

#WS.48 -- not expecting any new info here -- surprisingly, Henry Ellenson recorded a high WS.48 
#playing < 250 minutes -- he must've been super efficient in his limited time on the court
summary(trainingSetFor2017$WS.48)

trainingSet2017OutlierWS.48 <- trainingSetFor2017 %>%
  filter(WS.48 < -.055875 | WS.48 > .229125)
which(trainingSetFor2017$Player == "Henry Ellenson")
trainingSetFor2017 <- trainingSetFor2017[-c(93),]

#OBPM -- all outliers >250 min. except 2 of them
summary(trainingSetFor2017$OBPM)

trainingSet2017OBPM <- trainingSetFor2017 %>%
    filter(OBPM < -6.3 | OBPM > 4.1)
which(trainingSetFor2017$Player == "A.J. Hammons" | trainingSetFor2017$Player == "Daniel Ochefu")
trainingSetFor2017 <- trainingSetFor2017[-c(127, 245),]

#DBPM -- one outlier who played < 250 min
summary(trainingSetFor2017$DBPM)
trainingSet2017OutlierDBPM <- trainingSetFor2017 %>%
  filter(DBPM < -4.75 | DBPM > 4.45)
which(trainingSetFor2017$Player == "Bobby Brown")
trainingSetFor2017 <- trainingSetFor2017[-c(47),]

#BPM -- all among the best ballers in the league
summary(trainingSetFor2017$BPM)
trainingSet2017ForHighBPM <- trainingSetFor2017 %>%
  filter(BPM > 6.2375)

#VORP -- all good players
summary(trainingSetFor2017$VORP)
trainingSet2017ForHighVORP <- trainingSetFor2017 %>%
  filter (VORP > 3.05)



#for the most part, all of the advanced statistics favored guys who were actually
#the best players in the game -- there were far fewer outliers with <250 minutes 
#than there were for the Percentages

#ok now on to feature selection -- let's try backward selection first
#but first, let's get rid of some obviously irrevalent variables like Pos, Age and Team

trainingSet2017Condensed <- trainingSetFor2017 %>% 
  dplyr::select(-c(Pos, Age, Tm))


#let's split up the dataset into one DS with percentages and one with their respective numbers

trainingSet2017Percentages <- trainingSet2017Condensed %>%
  dplyr::select(-c(FG, FGA, X3P, X3PA, X2P, X2PA, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV))

trainingSet2017Numbers <- trainingSet2017Condensed %>%
  dplyr::select(-c(TS., X3PAr, FTr, ORB., DRB., TRB., AST., STL., BLK., TOV., USG., eFG., FT.))

#let's feature select with forward and backward selection -- let's first
#do it with the Percentages Dataset
FitAll <- lm (formula = PER ~. -Player, data = trainingSet2017Percentages)
summary(FitAll)


FitAllPercentages <- lm (formula = PER ~. -Player, data = trainingSet2017Percentages)
summary(FitAllPercentages)

step(FitAllPercentages, direction = "backward")

#AIC for backward selection -- -450.87, so this is a better selection of variables
#than the forward selection
formulaTrainingSet2017Backward <- PER ~ G + MP + TS. + X3PAr + FTr + ORB. + DRB. + 
  TRB. + AST. + STL. + BLK. + TOV. + USG. + WS.48 + OBPM + 
  BPM + VORP + X2P. + PF + PTS



FitForwardPercentages = lm(PER ~ 1,data = trainingSet2017Percentages)
step(FitForwardPercentages, direction = "forward", scope = formula(FitAll))
#so the AIC for the forward selection is -445.74

formulaTrainingSet2017Forward <- PER ~ WS.48 + USG. + ORB. + FG. + AST. + STL. + 
  PPG + TOV. + DRB. + DWS + BLK. + DBPM + MP + PF + OBPM + 
  PTS + X3PAr + TRB. + TS. + FTr + X2P.

#let's try linear regression for the backward selection variables first:

modelForBackwardSelectionPercentages <- lm(formula = formulaTrainingSet2017Backward, trainingSetFor2017)
summary(modelForBackwardSelectionPercentages)

sd(modelForBackwardSelectionPercentages$residuals)
fortify(modelForBackwardSelectionPercentages)
sd(modelForBackwardSelectionPercentages.resid)


ggplot(modelForBackwardSelectionPercentages, aes(.fitted, mean(.stdresid))) +
  geom_point() 

#note -- finding the variance of the fitted values was the last thing I did
#after realizing that this model was the best of the 4 -- hence, this is not out 
#of place
fort <- fortify(modelForBackwardSelectionPercentages)
max(fort$.fitted)

fort$bins <- cut(fort$.fitted, breaks=c(5, 10, 15, 20, 25))


fort2 <- fort %>% 
  group_by(bins) %>% 
  summarise(varResiduals=var(.resid))

fort2 = fort2 %>% mutate_if(is.factor,
                      forcats::fct_explicit_na,
                      na_level = "(25, max]")


ggplot(fort2, aes(x=bins, y=varResiduals, group=bins, fill=factor(bins))) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=varResiduals), position=position_dodge(width=0.9), vjust=-0.25)



par(mfrow=c(1,1))
plot(modelForBackwardSelectionPercentages)

#calculates the RMSE for backward selected percentages
differenceForBackwardSelectionPercentages <- sum(modelForBackwardSelectionPercentages$residuals^2)
RMSEBackwardSelectionPercentages <- sqrt(differenceForBackwardSelectionPercentages/nrow(trainingSetFor2017))
RMSEBackwardSelectionPercentages




PERPredictionsBackwardSelectionPercentages <- predict(modelForBackwardSelectionPercentages, testingSetFor2017)
SSE <- sum((PERPredictionsBackwardSelectionPercentages - testingSetFor2017$PER)^2)
RMSEBackwardSelectionPercentagesTestingSet <- sqrt(SSE/nrow(testingSetFor2017))
RMSEBackwardSelectionPercentagesTestingSet

#The RMSE for the backward selection on percentages for the training/testing set is 
#0.4844163 and 0.8274524 respectively

#calculate the MAE for backward selection on percentages
MAEBackwardSelectionPercentages <- mean(abs(modelForBackwardSelectionPercentages$residuals))
MAEBackwardSelectionPercentages

MAEBackwardSelectionPercentagesTestingSet <- mean(abs(PERPredictionsBackwardSelectionPercentages - testingSetFor2017$PER))
MAEBackwardSelectionPercentagesTestingSet



#now let's try it for forward selection variables
modelForForwardSelectionPercentages <- lm(formula = formulaTrainingSet2017Forward, trainingSetFor2017)
summary(modelForForwardSelectionPercentages)
par(mfrow=c(2,2))
plot(modelForForwardSelectionPercentages)


#RMSE for forward selected percentages

differenceForForwardSelectionPercentages <- sum(modelForForwardSelectionPercentages$residuals^2)
RMSEForwardSelectionPercentages <- sqrt(differenceForForwardSelectionPercentages/nrow(trainingSetFor2017))
RMSEForwardSelectionPercentages

PERPredictionsForwardSelectionPercentages <- predict(modelForForwardSelectionPercentages, testingSetFor2017)
SSE1 <- sum((PERPredictionsForwardSelectionPercentages - testingSetFor2017$PER)^2)
RMSEForwardSelectionPercentagesTestingSet <- sqrt(SSE1/nrow(testingSetFor2017))
RMSEForwardSelectionPercentagesTestingSet

#The RMSE for the backward selection on percentages for the training/testing set is 
#0.4866507 and 0.8726221 respectively



#the RMSE difference between the training/testing set is lower for the backward selected variables
#hence, this is a better model than using the forward selected variables

#MAE for forward selected percentages

MAEForwardSelectionPercentages <- mean(abs(modelForForwardSelectionPercentages$residuals))
MAEForwardSelectionPercentages

MAEForwardSelectionPercentagesTestingSet <- mean(abs(PERPredictionsForwardSelectionPercentages - testingSetFor2017$PER))
MAEForwardSelectionPercentagesTestingSet



#I wanna try it for numbers instead of percentages now -- let's start with Forward/Backward
#selection

FitAllNumbers <- lm (formula = PER ~. -Player, data = trainingSet2017Numbers)
summary(FitAll)

step(FitAllNumbers, direction = "backward")

#AIC for backward selection -- 45.614, so this is a better selection of variables
#than the forward selection
formulaTrainingSet2017BackwardNumbers <- PER ~ GS + MP + DWS + WS + WS.48 + OBPM + BPM + 
  FG + FGA + X3P + X3PA + X3P. + FT + FTA + ORB + DRB + AST + 
  STL + BLK + TOV + PF + PPG

FitForwardNumbers = lm(PER ~ 1,data = trainingSet2017Numbers)
step(FitForwardNumbers, direction = "forward", scope = formula(FitAllNumbers))
#so the AIC for the forward selection is 46.44

formulaTrainingSet2017ForwardNumbers <- PER ~ WS.48 + PPG + X3P + MP + FGA + TOV + FT + 
  DBPM + TRB + STL + WS + BLK + PF + OWS + AST + X3P. + X2PA + 
  OBPM + FG + GS + FTA + ORB + BPM

#these AIC numbers are wayyyyyyyyyy higher than their percentage counterparts

#but let's check out linear regression for the backward model just in case
modelForBackwardSelectionNumbers <- lm(formula = formulaTrainingSet2017BackwardNumbers, trainingSetFor2017)
summary(modelForBackwardSelectionNumbers)
par(mfrow=c(2,2))
plot(modelForBackwardSelectionNumbers)



differenceForBackwardSelectionNumbers <- sum(modelForBackwardSelectionNumbers$residuals^2)
RMSEBackwardSelectionNumbers <- sqrt(differenceForBackwardSelectionNumbers/nrow(trainingSetFor2017))
RMSEBackwardSelectionNumbers

PERPredictionsBackwardSelectionNumbers <- predict(modelForBackwardSelectionNumbers, testingSetFor2017)
SSENumbers <- sum((PERPredictionsBackwardSelectionNumbers - testingSetFor2017$PER)^2)
RMSEBackwardSelectionNumbersTestingSet <- sqrt(SSENumbers/nrow(testingSetFor2017))
RMSEBackwardSelectionNumbersTestingSet

#the RMSE difference b/t the training set and the testing set is .9994329 to 1.507918  

#MAE for backward selected numbers

MAEBackwardSelectedNumbers <- mean(abs(modelForBackwardSelectionNumbers$residuals))
MAEBackwardSelectedNumbers

MAEBackwardSelectedNumbersTestingSet <- mean(abs(PERPredictionsBackwardSelectionNumbers - testingSetFor2017$PER))
MAEBackwardSelectedNumbersTestingSet





#now let's try the forward selection linear regression
modelForForwardSelectionNumbers <- lm(formula = formulaTrainingSet2017ForwardNumbers, trainingSetFor2017)
summary(modelForForwardSelectionNumbers)
par(mfrow=c(2,2))
plot(modelForForwardSelectionNumbers)



differenceForForwardSelectionNumbers <- sum(modelForForwardSelectionNumbers$residuals^2)
RMSEForwardSelectionNumbers <- sqrt(differenceForForwardSelectionNumbers/nrow(trainingSetFor2017))
RMSEForwardSelectionNumbers

PERPredictionsForwardSelectionNumbers <- predict(modelForForwardSelectionNumbers, testingSetFor2017)
SSE1ForwardSelectionNumbers <- sum((PERPredictionsForwardSelectionNumbers - testingSetFor2017$PER)^2)
RMSEForwardSelectionNumbersTestingSet <- sqrt(SSE1ForwardSelectionNumbers/nrow(testingSetFor2017))
RMSEForwardSelectionNumbersTestingSet

#the RMSE difference is 1.500097 to .9977033, which is pretty equivalent to the backward selection numbers

#MAE forward selected numbers

MAEForwardSelectedNumbers <- mean(abs(modelForForwardSelectionNumbers$residuals))
MAEForwardSelectedNumbers

MAEForwardSelectedNumbersTestingSet <- mean(abs(PERPredictionsForwardSelectionNumbers - testingSetFor2017$PER))
MAEForwardSelectedNumbersTestingSet



#overall, the backward selection percentages regression has the least different RMSE's between the testing and 
#training set and both of these numbers are lower than all the other RMSE combinations




#now, finally, out of the four models we tried out, we can use the backward selection percentage-based model
#to predict the PERs of players from the 2017-2018 season.

seasonStats2018$PERPredictions <- predict(modelForBackwardSelectionPercentages, seasonStats2018)
seasonStats2018 <- seasonStats2018 %>% 
    arrange(desc(PERPredictions)) 
head(seasonStats2018)

seasonStats2018PERDesc <- seasonStats2018 %>%
  arrange(desc(PER))

head(seasonStats2018PERDesc)

seasonStats2018Top10PlayersBasedOnPrediction <- seasonStats2018[1:10, ]

seasonStats2018Top10PlayersBasedOnPrediction <- seasonStats2018Top10PlayersBasedOnPrediction %>%
  dplyr::select(Player, PER, PERPredictions)

seasonStats2018Top10PlayersBasedOnPrediction <- melt(seasonStats2018Top10PlayersBasedOnPrediction, id.vars = "Player")


ggplot(seasonStats2018Top10PlayersBasedOnPrediction, aes(x = Player, y = value)) + geom_bar(aes(fill = variable), 
                                                                                           width = 0.4, position = position_dodge(width=0.4),stat="identity") + 
  scale_x_discrete(limits = c("Edmond Sumner", "Vince Hunter", "James Harden",
                              "LeBron James", "Stephen Curry", "Monte Morris", "Anthony Davis", "Giannis Antetokounmpo",
                              "Russell Westbrook", "Boban Marjanovic"))




