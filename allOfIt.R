library(tidyverse)
library(beepr)
set.seed(31)


## first we want to get the data ready to fit the yards gained model

### Read in some data
setwd("/Users/aaronfoote/COURSES/Projects/With Greg/nfl-big-data-bowl-2024")

games <- read_csv("games.csv")
plays <- read_csv("plays.csv")
players <- read_csv("players.csv")

tracking <- list.files(pattern = 'tracking_week') %>% map(read_csv) %>% list_rbind()

      # Gotta have all plays going one direction
tracking <- tracking %>% 
  mutate(x = ifelse(playDirection == 'right',120-x,x),
         y = ifelse(playDirection == 'right',160/3 - y,y),
         dir = ifelse(playDirection == 'right',dir + 180,dir),
         dir = ifelse(dir > 360,dir - 360,dir),
         o = ifelse(playDirection == 'right',o + 180,o),
         o = ifelse(o > 360,o - 360,o)) %>%
  select(-c(time,jerseyNumber,playDirection))

### Filter Down Data for CDE Modeling


    # Don't care about tracking the position of the ball, plus it messes 
    # up stuff later down if I don't remove it
tracking <- tracking %>% filter(!is.na(nflId))


    # Only the plays in which a running back was the ball 
    # carrier (and no penalty)
rbIDs <- players %>% filter(position == "RB") %>% pull(nflId)
rbCarriesNoPenalties <- plays %>% 
  filter(ballCarrierId %in% rbIDs & is.na(penaltyYards)) %>%
  select(gameId,playId,ballCarrierId,defensiveTeam)

    # Only the frames for plays in which a handoff occurs
justHandoffs <- tracking %>% group_by(gameId,playId) %>% 
  mutate(hasHandoff = ifelse("handoff" %in% event,1,0)) %>%
  ungroup() %>%
  filter(hasHandoff == 1)

    # Take the intersection!
rbHandoffTracking <- inner_join(justHandoffs,
                                rbCarriesNoPenalties,
                                by = join_by(gameId,playId))

rm(tracking,justHandoffs,rbCarriesNoPenalties) # Clean up after yourself...


    # Only want the plays for which the final event is:
    # out_of_bounds, safety, tackle, or touchdown
finalEvents <- rbHandoffTracking %>% 
  filter(!is.na(event)) %>%
  group_by(gameId,playId) %>%
  distinct(frameId,event) %>% 
  mutate(maxEventFrame = max(frameId)) %>% 
  filter(maxEventFrame == frameId) %>% 
  mutate(finalEvent = event) %>%
  select(gameId,playId,maxEventFrame,finalEvent)

rbHandoffTracking <- left_join(rbHandoffTracking,finalEvents,
                               by = join_by(gameId,playId)) %>%
  filter(finalEvent %in% c("out_of_bounds","safety","tackle","touchdown"))

    # Now we have the plays on which the density estimates will be trained. 
    # Next up is to filter for only the relevant frames from each play.

processGame <- function(trackingData,f){
  return(trackingData %>% group_by(playId) %>% group_modify(~f(.x)))
} # Some light currying :)

relevantFrames <- function(playData){
  handoffFrame <- playData %>% 
    filter(event == "handoff") %>% 
    distinct(frameId) %>% 
    pull(frameId)
  return(playData %>% 
           filter(frameId >= handoffFrame, # Handoff and later
                  frameId <= maxEventFrame)) # But not after the play ends
}

rbHandoffTrackingTrimmed <- rbHandoffTracking %>% 
  group_by(gameId) %>%
  group_modify(~processGame(.x,relevantFrames))

rm(rbHandoffTracking, rbIDs) # Clean up after yourself...

### Features for CDE Modeling

justDistances <- function(d){
  rbId <- d %>% pull(ballCarrierId) %>% unique()
  rbX <- pull(d[d$nflId == rbId,'x'])
  rbY <- pull(d[d$nflId == rbId,'y'])
  return(d %>% mutate(distFromRusher = sqrt((x - rbX)*(x - rbX) + (y - rbY)*(y - rbY))))
}

distanceFeatures <- function(trackingData){
  return(trackingData %>% group_by(frameId) %>% group_modify(~justDistances(.x)))
}

defensePlusRB <- rbHandoffTrackingTrimmed %>% 
  filter(club == defensiveTeam | nflId == ballCarrierId)

    # Get distance from each defensive player to the running back
defensePlusRB <- defensePlusRB %>% 
  group_by(gameId) %>% 
  group_modify(~processGame(.x,distanceFeatures))

defenseWide <- defensePlusRB %>% filter(nflId != ballCarrierId) %>%
  select(gameId,playId,frameId,distFromRusher,s,x,y) %>%
  group_by(gameId,playId,frameId) %>%
  arrange(distFromRusher,.by_group = T) %>% 
  mutate(distRank = row_number()) %>%
  pivot_wider(id_cols = c(gameId,playId,frameId),
              values_from = c(distFromRusher,s,x,y),
              names_from = distRank,
              names_prefix = "defense")

    # Lastly we want the speed and xy-coordinates 
    # of the running back at every frame
rbFeatures <- rbHandoffTrackingTrimmed %>% filter(ballCarrierId == nflId) %>%
  select(gameId,playId,frameId,x,y,s)

featuresBase <- left_join(rbFeatures,defenseWide, 
                          by = join_by(gameId,playId,frameId))

featuresWithOutcomes <- featuresBase %>% group_by(gameId,playId) %>% 
  mutate(lastFrame = max(frameId)) %>% filter(frameId == lastFrame) %>%
  mutate(finalX = x) %>% select(gameId,playId,finalX) %>%
  right_join(featuresBase,by = join_by(gameId,playId))

featuresMinusWeek <- left_join(featuresWithOutcomes, 
                               finalEvents %>% ungroup() %>% select(-maxEventFrame),
                               by = join_by(gameId,playId)) %>%
  mutate(finalX = ifelse(finalEvent == 'touchdown',10,finalX),
         finalX = ifelse(finalEvent == 'safety',110,finalX)) %>%
  ungroup() %>%
  select(-finalEvent)

    # Last step is to add in the weeks of the games for the 
    # leave-one-week-out cross-fitting

left_join(featuresMinusWeek,
          games %>% select(gameId,week),
          by = join_by(gameId)) %>%
  ungroup() %>%
  mutate(yardsGained = x - finalX) -> readyForModeling  # gotta ungroup 
                                                # so the selects/filters work
                                                # in the LOWO cross-fitting
write_csv(readyForModeling,
          "/Users/aaronfoote/COURSES/Projects/With Greg/allOfRFCDEfitting.csv")

rm(finalEvents, defenseWide, rbFeatures, featuresBase, featuresMinusWeek, featuresWithOutcomes) # Clean up after yourself...


## Fit the RFCDE model
library(RFCDE)
lowo <- function(m,data,modelVars,response,cvVal){
  trainMatrix <- data %>% filter(week != cvVal) %>%
    select(modelVars) %>% as.matrix()
  responses <- data %>% filter(week != cvVal) %>%
    pull(response)
  return(m(trainMatrix,responses))
}

parsimoniousFeatures <- c("s","distFromRusher_defense1",
                          "distFromRusher_defense2","distFromRusher_defense3",
                          "distFromRusher_defense4","distFromRusher_defense5",
                          "distFromRusher_defense6","distFromRusher_defense7",
                          "distFromRusher_defense8")
weeks <- 1:9
t1 <- Sys.time()
parsimoniousCrossFits <- lapply(weeks,lowo,
                                m = RFCDE,
                                data = readyForModeling,
                                modelVars = parsimoniousFeatures,
                                response = 'yardsGained')
t2 <- Sys.time()
print(t2 - t1)
beep("complete")


    # Now we want to get the pmfs for every frame in the tracking data set, writing
    # them to file.

densityEstimates <- function(i,data){
  dfRow <- data[i,]
  # Density Estimate
  cde <- predict(parsimoniousCrossFits[[dfRow %>% pull(week)]],
                 as.matrix.data.frame(dfRow[parsimoniousFeatures]),
                 "CDE",
                 seq(max(-10,dfRow %>% pull(x) - 110), dfRow %>% pull(x) - 10,1),
                 bandwidth = 0.1)
  
  return(tibble(gameId = dfRow %>% pull(gameId),
                playId = dfRow %>% pull(playId),
                frameId = dfRow %>% pull(frameId),
                p = cde[1,],
                gain = seq(max(-10,dfRow %>% pull(x) - 110), dfRow %>% pull(x) - 10,1)))
}

walk(1:9,function(w){
  t1 <- Sys.time()
  sub <- readyForModeling %>% filter(week == w)
  write_csv(map_dfr(1:nrow(sub),densityEstimates,data = sub),
            paste0("/Users/aaronfoote/COURSES/Projects/With Greg/adjustedGainDensities/pmfY_",w,".csv"))
  t2 <- Sys.time()
  print(w)
  print(t2-t1)
})

beep("complete")


# Ranking defenders by yards saved

setwd("/Users/aaronfoote/COURSES/Projects/With Greg/adjustedGainDensities/")
pmfs <- list.files(pattern = "pmfY_",recursive = T) %>% 
  map(read_csv) %>% list_rbind()

    # re-scale the PMFs so that they sum to one
pmfs <- pmfs %>% group_by(gameId,playId,frameId) %>% 
  mutate(sumOfCDE = sum(p)) %>%
  ungroup() %>%
  mutate(pAdj = p/sumOfCDE) %>%
  select(gameId,playId,frameId,gain,pAdj)

    # get the expected yards gained for the RB at every frame
expGains <- pmfs %>% mutate(expGcomponent = gain*pAdj) %>% 
  group_by(gameId,playId,frameId) %>%
  summarize(expG = sum(expGcomponent)) %>% ungroup()
    
    # join in the change in expected yards gained varaible
expGains %>% group_by(gameId,playId) %>%
  mutate(prevExpectedG = lag(expG,n = 1, order_by = frameId)) %>% ungroup() %>%
  mutate(dExpectedG = prevExpectedG - expG) %>%
  select(gameId,playId,frameId,dExpectedG) %>%
  right_join(defensePlusRB, by = join_by(gameId,playId,frameId)) -> withExpGain

    #' Okay this next block looks intimidating. We first determine which rows 
    #' are frames of defensive players with contact opportunities, then only
    #' including those frames. Then we take the inverse distance from the rusher
    #' to be used in our kernel credit attribution (if you're on top of the rusher,
    #' the inverse of your distance is set to be really big but still defined
    #' because you can't actually inhabit the same coordinates of the rusher).
    #' 
    #' Then, for each frame the proportion of credit each player deserves is
    #' calculated (in the uniform attribution and kernel attribution manners). 
    #' 
    #' The final mutate actually computes the expected yards that are saved
    #' under each attribution strategy.  

withExpGain %>% ungroup() %>%
  mutate(contactOpportunity = ifelse(club == defensiveTeam & distFromRusher <= 1.5,1,0)) %>%
  filter(contactOpportunity == 1) %>%
  mutate(inverseDistance = 1/distFromRusher,
         inverseDistance = ifelse(inverseDistance == Inf, 100, inverseDistance)) %>%
  group_by(gameId,playId,frameId) %>% 
  mutate(sumInverseDist = sum(inverseDistance),
         contactOpportunityCount = sum(contactOpportunity)) %>%
  ungroup() %>%
  mutate(proportionOfContact = contactOpportunity/contactOpportunityCount) %>% 
  mutate(inverseSavedWeight = inverseDistance/sumInverseDist,
         uniformWeight = 1/proportionOfContact) %>%
  mutate(distanceWeighted = dExpectedG*inverseSavedWeight,
         uniformWeighted = dExpectedG*uniformWeight) -> rankMeYards1.5

withExpGain %>% ungroup() %>%
  mutate(contactOpportunity = ifelse(club == defensiveTeam & distFromRusher <= 2.5,1,0)) %>%
  filter(contactOpportunity == 1) %>%
  mutate(inverseDistance = 1/distFromRusher,
         inverseDistance = ifelse(inverseDistance == Inf, 100, inverseDistance)) %>%
  group_by(gameId,playId,frameId) %>% 
  mutate(sumInverseDist = sum(inverseDistance),
         contactOpportunityCount = sum(contactOpportunity)) %>%
  ungroup() %>%
  mutate(proportionOfContact = contactOpportunity/contactOpportunityCount) %>% 
  mutate(inverseSavedWeight = inverseDistance/sumInverseDist,
         uniformWeight = 1/proportionOfContact) %>%
  mutate(distanceWeighted = dExpectedG*inverseSavedWeight,
         uniformWeighted = dExpectedG*uniformWeight) -> rankMeYards2.5

    #' Now the actual rankings are determined (for total expected yards saved). 
    #' For each credit attribution strategy, the total expected yards saved is
    #' calculated and their ranking among their peers determined.
netRankings <- rankMeYards1.5 %>% 
  group_by(nflId) %>% 
  summarize(dwYardsSaved = sum(distanceWeighted),
            uwYardsSaved = sum(uniformWeighted)) %>%
  arrange(desc(dwYardsSaved)) %>% mutate(dwRank = row_number()) %>%
  arrange(desc(uwYardsSaved)) %>% mutate(uwRank = row_number())

write_csv(netRankings, "/Users/aaronfoote/COURSES/Projects/With Greg/yardsSavedNetRanking.csv")

    #' An accumulated stat isn't informative as a rate-based quality stat most
    #' of the time, so that's what we do next. For this, we rank players based
    #' on the average expected yards saved per contact opportunity.

withExpGain %>% 
  left_join(rankMeYards1.5 %>% select(gameId,playId,frameId,nflId,distanceWeighted,uniformWeighted),
            by = join_by(gameId,playId,frameId,nflId)) %>%
  select(gameId,playId,frameId,nflId,club,defensiveTeam,distFromRusher,distanceWeighted,uniformWeighted) %>% 
  filter(club == defensiveTeam) %>% # Only want defenders
  mutate(contactOpportunity = ifelse(distFromRusher <= 1.5,1,0)) %>% # Need to be within 1.5 yards to have CO
  select(-club,-defensiveTeam) -> rankingSub1.5 # Don't need these variables anymore

withExpGain %>% 
  left_join(rankMeYards2.5 %>% select(gameId,playId,frameId,nflId,distanceWeighted,uniformWeighted),
            by = join_by(gameId,playId,frameId,nflId)) %>%
  select(gameId,playId,frameId,nflId,club,defensiveTeam,distFromRusher,distanceWeighted,uniformWeighted) %>% 
  filter(club == defensiveTeam) %>% # Only want defenders
  mutate(contactOpportunity = ifelse(distFromRusher <= 2.5,1,0)) %>% # Need to be within 1.5 yards to have CO
  select(-club,-defensiveTeam) -> rankingSub2.5

rankingSub1.5 %>% 
  group_by(gameId,playId,nflId) %>% # For each player in each play of each game...
  mutate(contactOppPrevFrame = lag(contactOpportunity,n = 1, order_by = frameId)) %>%
  ungroup() %>% mutate(startOfContactOpportunity = 
                         ifelse(contactOpportunity == 1 & contactOppPrevFrame == 0,1,0)) %>%
  filter(startOfContactOpportunity == 1) %>%
  mutate(contactOpportunityStartIndex = row_number()) %>%
  select(gameId,playId,frameId,nflId,contactOpportunityStartIndex) -> startIndices1.5

rankingSub2.5 %>% 
  group_by(gameId,playId,nflId) %>% # For each player in each play of each game...
  mutate(contactOppPrevFrame = lag(contactOpportunity,n = 1, order_by = frameId)) %>%
  ungroup() %>% mutate(startOfContactOpportunity = 
                         ifelse(contactOpportunity == 1 & contactOppPrevFrame == 0,1,0)) %>%
  filter(startOfContactOpportunity == 1) %>%
  mutate(contactOpportunityStartIndex = row_number()) %>%
  select(gameId,playId,frameId,nflId,contactOpportunityStartIndex) -> startIndices2.5

left_join(rankingSub1.5,startIndices1.5, 
          by = join_by(gameId,playId,frameId,nflId)) %>%
  filter(contactOpportunity == 1) %>%
  group_by(gameId,playId,nflId) %>%
  arrange(frameId) %>%
  tidyr::fill(contactOpportunityStartIndex,.direction = "down") %>%
  group_by(nflId,contactOpportunityStartIndex) %>%
  summarize(dwYardsSaved = sum(distanceWeighted),
            uwYardsSaved = sum(uniformWeighted)) %>%
  summarize(dwAvgYardsSavedPerOpportunity = mean(dwYardsSaved),
            uwAvgYardsSavedPerOpportunity = mean(uwYardsSaved)) -> qualityRankings1.5

left_join(rankingSub2.5,startIndices2.5, 
          by = join_by(gameId,playId,frameId,nflId)) %>%
  filter(contactOpportunity == 1) %>%
  group_by(gameId,playId,nflId) %>%
  arrange(frameId) %>%
  tidyr::fill(contactOpportunityStartIndex,.direction = "down") %>%
  group_by(nflId,contactOpportunityStartIndex) %>%
  summarize(dwYardsSaved = sum(distanceWeighted),
            uwYardsSaved = sum(uniformWeighted)) %>%
  summarize(dwAvgYardsSavedPerOpportunity = mean(dwYardsSaved),
            uwAvgYardsSavedPerOpportunity = mean(uwYardsSaved)) -> qualityRankings2.5


    # With a quality-based metric, we gotta make sure to filter out players who
    # do not have very many opportunities and may have just performed well by
    # chance. I use 50 snaps against the run in nine weeks as a starting point.

defensePlusRB %>% 
  ungroup() %>% 
  filter(club == defensiveTeam) %>% 
  distinct(gameId,playId,nflId) %>% 
  count(nflId) %>% 
  filter(n >= 90) %>% 
  left_join(qualityRankings1.5, by = join_by(nflId)) -> finalQualityRankings1.5

write_csv(finalQualityRankings1.5, "/Users/aaronfoote/COURSES/Projects/With Greg/yardsSavedRateRanking1.5.csv")

defensePlusRB %>% 
  ungroup() %>% 
  filter(club == defensiveTeam) %>% 
  distinct(gameId,playId,nflId) %>% 
  count(nflId) %>% 
  filter(n >= 50) %>% 
  left_join(qualityRankings2.5, by = join_by(nflId)) -> finalQualityRankings2.5

write_csv(finalQualityRankings2.5, "/Users/aaronfoote/COURSES/Projects/With Greg/yardsSavedRateRanking2.5.csv")




##' An assumption that this approach makes is that all yards saved are of equal
##' value. In other words, saving a yard at the two yard line on 4th and goal
##' is of the same value as saving a yard 65 yards from the end zone on 2nd and
##' 8. Clearly this isn't the case, and the context in which yards are saved
##' taken into account. Let's do it!

# Calculating Instantaneous Value of the RB at every frame
##' This uses a slightly modified version of the expected points model provided
##' by the nflfastR package. 

library(xgboost)
#######################
#Functions for the model
#######################
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_cp_cpoe.R")

#Loading in the Data
pbp_data <- readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/models/cal_data.rds?raw=true"))

model_data <- pbp_data %>%
  # in 'R/helper_add_nflscrapr_mutations.R'
  make_model_mutations() %>%
  mutate(
    label = case_when(
      Next_Score_Half == "Touchdown" ~ 0,
      Next_Score_Half == "Opp_Touchdown" ~ 1,
      Next_Score_Half == "Field_Goal" ~ 2,
      Next_Score_Half == "Opp_Field_Goal" ~ 3,
      Next_Score_Half == "Safety" ~ 4,
      Next_Score_Half == "Opp_Safety" ~ 5,
      Next_Score_Half == "No_Score" ~ 6
    ),
    label = as.factor(label),
    # use nflscrapR weights
    Drive_Score_Dist = Drive_Score_Half - drive,
    Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
      (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
    ScoreDiff_W = (max(abs(score_differential), na.rm = T) - abs(score_differential)) /
      (max(abs(score_differential), na.rm = T) - min(abs(score_differential), na.rm = T)),
    Total_W = Drive_Score_Dist_W + ScoreDiff_W,
    Total_W_Scaled = (Total_W - min(Total_W, na.rm = T)) /
      (max(Total_W, na.rm = T) - min(Total_W, na.rm = T))
  ) %>%
  filter(
    !is.na(defteam_timeouts_remaining), !is.na(posteam_timeouts_remaining),
    !is.na(yardline_100)
  ) %>%
  select(
    label,
    season,
    half_seconds_remaining,
    yardline_100,
    home,
    ydstogo,
    era0, era1, era2, era3, era4,
    down1, down2, down3, down4,
    Total_W_Scaled
  )

# For the xgboost
model_data <- model_data %>%
  mutate(
    label = as.numeric(label),
    label = label - 1
  )

nrounds <- 525
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 7,
    eta = 0.025,
    gamma = 1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 5,
    min_child_weight = 1
  )

t1 <- Sys.time()
trainData <- model_data %>% select(-season)
fullTrain <- xgboost::xgb.DMatrix(model.matrix(~ . + 0,data = trainData %>% select(-label,-Total_W_Scaled)),
                                  label = trainData$label,
                                  weight = trainData$Total_W_Scaled)
epModel <- xgboost::xgboost(params = params, data = fullTrain, nrounds = nrounds, verbose = 2)
beep("complete")
t2 <- Sys.time()
print(t2 - t1)

rm(model_data,pbp_data,trainData)

#' The next part is to properly get the features read in to actually estimate
#' player value at every frame. Thankfully joining onto `pmfs` will 
#' automatically only consider the plays involving RB rushes and only frames
#' between the handoff and end of the play.


plays <- plays %>% 
  left_join(games %>% select(gameId, homeTeamAbbr), 
            by = join_by(gameId)) %>%
  mutate(
    loS = ifelse(possessionTeam == yardlineSide,
                 50 + 50 - yardlineNumber, yardlineNumber)) %>%
  replace_na(list(loS = 50)) %>%
  mutate(secondsInHalfSnap = ifelse(
    quarter %% 2 == 0,
    60*as.numeric(substr(gameClock,1,2)) + as.numeric(substr(gameClock,4,5)),
    900 + 60*as.numeric(substr(gameClock,1,2)) + as.numeric(substr(gameClock,4,5))),
    possessHome = ifelse(possessionTeam == homeTeamAbbr, 1, 0))

xp <- left_join(pmfs, plays %>% select(gameId, playId, loS, secondsInHalfSnap,
                                       down, yardsToGo, possessHome),
                by = join_by(gameId,playId)) %>%
  rename(downSnap = down, yTGsnap = yardsToGo) %>%
  left_join(readyForModeling %>% select(gameId,playId,frameId,x),
            by = join_by(gameId,playId,frameId)) %>%
  mutate(rbPosition = x - 10) %>% select(-x)

xpFeatures <- xp %>% mutate(
  ydstogo = rbPosition - gain - loS + yTGsnap,
  firstDown = ifelse(ydstogo <= 0, 1, 0),
  ydstogo = ifelse(firstDown, 10, ydstogo),
  ydstogo = ifelse(rbPosition - gain < 10, rbPosition - gain, ydstogo),
  down = ifelse(
    firstDown,
    1,
    ifelse(downSnap == 4, 1, downSnap + 1)),
  home = ifelse(
    !firstDown & downSnap == 4,
    (possessHome + 1) %% 2,
    possessHome),
  yardline_100 = ifelse(
    !firstDown & downSnap == 4,
    100 - rbPosition + gain,
    rbPosition - gain),
  turnoverOnDowns = ifelse(!firstDown & downSnap == 4, -1, 1),
  half_seconds_remaining = secondsInHalfSnap - frameId/10 - 30,
  half_seconds_remaining = ifelse(
    half_seconds_remaining <= 0,
    ifelse(half_seconds_remaining <= -10, 0, 1),
    half_seconds_remaining),
  era0 = 0, era1 = 0, era2 = 0, era3 = 0, era4 = 1,
  down1 = ifelse(down == 1, 1, 0),down2 = ifelse(down == 2, 1, 0),
  down3 = ifelse(down == 3, 1, 0),down4 = ifelse(down == 4, 1, 0))

as.data.frame(
  matrix(
    predict(
      epModel,
      as.matrix(xpFeatures %>% ungroup() %>% 
                  select(half_seconds_remaining,yardline_100,home,ydstogo,era0,
                         era1,era2,era3,era4,down1,down2,down3,down4))),
    ncol = 7, byrow = T)) %>%
  rename("Touchdown" = "V1", "Opp_Touchdown" = "V2", "Field_Goal" = "V3",
         "Opp_Field_Goal" = "V4", "Safety" = "V5", "Opp_Safety" = "V6", "No_Score" = "V7") %>%
  mutate(ep = 7*Touchdown + 3*Field_Goal + 2*Safety - 2*Opp_Safety - 3*Opp_Field_Goal - 7*Opp_Touchdown) %>%
  cbind(xpFeatures %>% ungroup() %>% 
          select(gameId,playId,frameId,pAdj,turnoverOnDowns)) %>%
  mutate(ivComponent = pAdj*ep*turnoverOnDowns) %>% group_by(gameId,playId,frameId) %>%
  summarize(iv = sum(ivComponent)) %>% ungroup() -> rbIV

beep('complete')

# Ranking Defenders by Points Saved

rbIV %>% group_by(gameId,playId) %>%
  mutate(prevIV = lag(iv,n = 1, order_by = frameId)) %>% ungroup() %>%
  mutate(dIV = prevIV - iv) %>%
  select(gameId,playId,frameId,dIV) %>%
  right_join(defensePlusRB, by = join_by(gameId,playId,frameId)) -> withIV

withIV %>% ungroup() %>%
  mutate(contactOpportunity = ifelse(club == defensiveTeam & distFromRusher <= 1.5,1,0)) %>%
  filter(contactOpportunity == 1) %>%
  mutate(inverseDistance = 1/distFromRusher,
         inverseDistance = ifelse(inverseDistance == Inf, 100, inverseDistance)) %>%
  group_by(gameId,playId,frameId) %>% 
  mutate(sumInverseDist = sum(inverseDistance),
         contactOpportunityCount = sum(contactOpportunity)) %>%
  ungroup() %>%
  mutate(proportionOfContact = contactOpportunity/contactOpportunityCount) %>% 
  mutate(inverseSavedWeight = inverseDistance/sumInverseDist,
         uniformWeight = 1/proportionOfContact) %>%
  mutate(distanceWeighted = dIV*inverseSavedWeight,
         uniformWeighted = dIV*uniformWeight) -> rankMePoints1.5

withIV %>% ungroup() %>%
  mutate(contactOpportunity = ifelse(club == defensiveTeam & distFromRusher <= 2.5,1,0)) %>%
  filter(contactOpportunity == 1) %>%
  mutate(inverseDistance = 1/distFromRusher,
         inverseDistance = ifelse(inverseDistance == Inf, 100, inverseDistance)) %>%
  group_by(gameId,playId,frameId) %>% 
  mutate(sumInverseDist = sum(inverseDistance),
         contactOpportunityCount = sum(contactOpportunity)) %>%
  ungroup() %>%
  mutate(proportionOfContact = contactOpportunity/contactOpportunityCount) %>% 
  mutate(inverseSavedWeight = inverseDistance/sumInverseDist,
         uniformWeight = 1/proportionOfContact) %>%
  mutate(distanceWeighted = dIV*inverseSavedWeight,
         uniformWeighted = dIV*uniformWeight) -> rankMePoints2.5

netRankingsPoints <- rankMePoints1.5 %>% 
  group_by(nflId) %>% 
  summarize(dwPointsSaved = sum(distanceWeighted),
            uwPointsSaved = sum(uniformWeighted)) %>%
  arrange(desc(dwPointsSaved)) %>% mutate(dwRank = row_number()) %>%
  arrange(desc(uwPointsSaved)) %>% mutate(uwRank = row_number())

write_csv(netRankingsPoints, "/Users/aaronfoote/COURSES/Projects/With Greg/pointsSavedNetRanking.csv")

withIV %>% 
  left_join(rankMePoints1.5 %>% select(gameId,playId,frameId,nflId,distanceWeighted,uniformWeighted),
            by = join_by(gameId,playId,frameId,nflId)) %>%
  select(gameId,playId,frameId,nflId,club,defensiveTeam,distFromRusher,distanceWeighted,uniformWeighted) %>% 
  filter(club == defensiveTeam) %>% # Only want defenders
  mutate(contactOpportunity = ifelse(distFromRusher <= 1.5,1,0)) %>% # Need to be within 1.5 yards to have CO
  select(-club,-defensiveTeam) -> rankingSub1.5 # Don't need these variables anymore

withIV %>% 
  left_join(rankMePoints2.5 %>% select(gameId,playId,frameId,nflId,distanceWeighted,uniformWeighted),
            by = join_by(gameId,playId,frameId,nflId)) %>%
  select(gameId,playId,frameId,nflId,club,defensiveTeam,distFromRusher,distanceWeighted,uniformWeighted) %>% 
  filter(club == defensiveTeam) %>% # Only want defenders
  mutate(contactOpportunity = ifelse(distFromRusher <= 2.5,1,0)) %>% # Need to be within 1.5 yards to have CO
  select(-club,-defensiveTeam) -> rankingSub2.5 # Don't need these variables anymore

rankingSub1.5 %>% 
  group_by(gameId,playId,nflId) %>% # For each player in each play of each game...
  mutate(contactOppPrevFrame = lag(contactOpportunity,n = 1, order_by = frameId)) %>%
  ungroup() %>% mutate(startOfContactOpportunity = 
                         ifelse(contactOpportunity == 1 & contactOppPrevFrame == 0,1,0)) %>%
  filter(startOfContactOpportunity == 1) %>%
  mutate(contactOpportunityStartIndex = row_number()) %>%
  select(gameId,playId,frameId,nflId,contactOpportunityStartIndex) -> startIndices1.5

rankingSub2.5 %>% 
  group_by(gameId,playId,nflId) %>% # For each player in each play of each game...
  mutate(contactOppPrevFrame = lag(contactOpportunity,n = 1, order_by = frameId)) %>%
  ungroup() %>% mutate(startOfContactOpportunity = 
                         ifelse(contactOpportunity == 1 & contactOppPrevFrame == 0,1,0)) %>%
  filter(startOfContactOpportunity == 1) %>%
  mutate(contactOpportunityStartIndex = row_number()) %>%
  select(gameId,playId,frameId,nflId,contactOpportunityStartIndex) -> startIndices2.5

left_join(rankingSub1.5,startIndices1.5, 
          by = join_by(gameId,playId,frameId,nflId)) %>%
  filter(contactOpportunity == 1) %>%
  group_by(gameId,playId,nflId) %>%
  arrange(frameId) %>%
  tidyr::fill(contactOpportunityStartIndex,.direction = "down") %>%
  group_by(nflId,contactOpportunityStartIndex) %>%
  summarize(dwPointsSaved = sum(distanceWeighted),
            uwPointsSaved = sum(uniformWeighted)) %>%
  summarize(dwAvgPointsSavedPerOpportunity = mean(dwPointsSaved),
            uwAvgPointsSavedPerOpportunity = mean(uwPointsSaved)) -> qualityRankings1.5

left_join(rankingSub2.5,startIndices2.5, 
          by = join_by(gameId,playId,frameId,nflId)) %>%
  filter(contactOpportunity == 1) %>%
  group_by(gameId,playId,nflId) %>%
  arrange(frameId) %>%
  tidyr::fill(contactOpportunityStartIndex,.direction = "down") %>%
  group_by(nflId,contactOpportunityStartIndex) %>%
  summarize(dwPointsSaved = sum(distanceWeighted),
            uwPointsSaved = sum(uniformWeighted)) %>%
  summarize(dwAvgPointsSavedPerOpportunity = mean(dwPointsSaved),
            uwAvgPointsSavedPerOpportunity = mean(uwPointsSaved)) -> qualityRankings2.5

# With a quality-based metric, we gotta make sure to filter out players who
# do not have very many opportunities and may have just performed well by
# chance. I use 50 snaps against the run in nine weeks as a starting point.

defensePlusRB %>% 
  ungroup() %>% 
  filter(club == defensiveTeam) %>% 
  distinct(gameId,playId,nflId) %>% 
  count(nflId) %>% 
  filter(n >= 90) %>% 
  left_join(qualityRankings1.5, by = join_by(nflId)) -> finalQualityRankingsPoints1.5

write_csv(finalQualityRankingsPoints1.5, "/Users/aaronfoote/COURSES/Projects/With Greg/pointsSavedRateRanking1.5.csv")

defensePlusRB %>% 
  ungroup() %>% 
  filter(club == defensiveTeam) %>% 
  distinct(gameId,playId,nflId) %>% 
  count(nflId) %>% 
  filter(n >= 50) %>% 
  left_join(qualityRankings2.5, by = join_by(nflId)) -> finalQualityRankingsPoints2.5

write_csv(finalQualityRankingsPoints2.5, "/Users/aaronfoote/COURSES/Projects/With Greg/pointsSavedRateRanking2.5.csv")


# Contact Opportunity Distance Threshold Plot
library(plotly)

contactThresholdPlotData <- defensePlusRB %>% 
  filter(!is.na(event),event %in% c("first_contact","tackle"),club == defensiveTeam) %>%
  group_by(gameId,playId,frameId) %>% 
  summarize(minDefenderDist = min(distFromRusher), event = unique(event)) %>% 
  ungroup() %>% 
  filter(minDefenderDist <= 4)
ggplotly(ggplot(data = contactThresholdPlotData, aes(x = minDefenderDist, fill = event)) + 
  geom_histogram(alpha = 0.75, color = "black", bins = 50, size = 0.25) +
  scale_fill_manual(values = c("#D72121","#000000"),
                    labels = c('First Contact', 'Tackle')) +
  geom_vline(xintercept = 1.5, size=1.5, linetype = "dashed") +
  labs(x = "Minimum Distance from Defender to RB", y = "Frequency", fill = "Event") + 
  scale_x_continuous(n.breaks = 7, limits = c(0,3)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,linewidth = 2),
        text = element_text(size = 30,family="Lato"),
        legend.position= c(0.75,0.75),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35))) %>% 
  layout(legend = list(x = 0.75, xanchor = 'center', y = 0.75))

# Table of Top Players by Avg. Yards/Points Saved per Opportunity

  # I want to see how similar the two quantities are
  # (ideally they're pretty similar)

pointsSaved <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/pointsSavedRateRanking.csv")
ggplot(pointsSaved, aes(x = dwAvgPointsSavedPerOpportunity, y = uwAvgPointsSavedPerOpportunity)) + 
  geom_point()

yardsSaved <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/yardsSavedRateRanking.csv")
ggplot(yardsSaved, aes(x = dwAvgYardsSavedPerOpportunity, y = uwAvgYardsSavedPerOpportunity)) + 
  geom_point()

inner_join(pointsSaved %>% select(nflId, dwAvgPointsSavedPerOpportunity),
           yardsSaved %>% select(nflId, dwAvgYardsSavedPerOpportunity),
           by = join_by(nflId)) -> combinedRankings
left_join(combinedRankings, 
          players %>% select(nflId,displayName,position),
          by = join_by(nflId)) %>% 
  rename(points = dwAvgPointsSavedPerOpportunity,
         yards = dwAvgYardsSavedPerOpportunity) -> combinedRankings2

combinedRankings2[combinedRankings2$position %in% c("NT","DE","DT"),"genPos"] <- "D-Line"
combinedRankings2[combinedRankings2$position %in% c("OLB","MLB","ILB"),"genPos"] <- "Linebacker"
combinedRankings2[combinedRankings2$position %in% c("SS","FS","CB","DB"),"genPos"] <- "Secondary"
ggplot(combinedRankings2, aes(x = points, fill = genPos)) + geom_boxplot()

library(reactablefmtr)
reactable(combinedRankings2 %>%
            mutate(points = round(points,3), yards = round(yards,3)) %>%
            select(-position,-nflId) %>%
            rename('Player' = displayName, 'Position' = genPos,
                   'Avg. Pts. Saved per Opportunity' = points,
                   'Avg. Yards Saved per Opportunity' = yards),
          defaultSorted = 'Avg. Pts. Saved per Opportunity',
          defaultSortOrder = 'desc',
          columns = list(
            'Player' = colDef(width = 150),
            'Position' = colDef(width = 90),
            'Avg. Pts. Saved per Opportunity' = colDef(width = 150),
            'Avg. Yards Saved per Opportunity' = colDef(width = 150)),
          defaultColDef = colDef(align = 'center'))

# Plots of Expected Yards Gained and Instantaneous Value of the RB over a play
#' A play that I really like for this is Tony Pollard's TD run against the Rams
#' in week 5 of the 2022 season. Here is a link if you want to watch it:
#' 
#' https://www.youtube.com/watch?v=EtYKNN37lp8
#' 
#' playId -- 1323 gameId -- 2022100912
library(ggrepel)
tracking5 <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/nfl-big-data-bowl-2024/tracking_week_5.csv") %>%
  filter(playId == 1323, gameId == 2022100912, nflId == 47911) %>% 
  select(gameId,frameId,playId,event)
  
expGains %>% 
  filter(playId == 1323, gameId == 2022100912) %>%
  rbind(tibble(playId = 1323, gameId = 2022100912, frameId = c(1:20,116:119),
               expG = rep(c(10,0),times = c(20,4)))) %>%
  left_join(tracking5, by = join_by(frameId)) %>%
  select(frameId,expG,event) %>%
  ggplot(aes(x = frameId,y = expG, label = event)) + 
  geom_point() + geom_line() + 
  geom_label_repel() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,linewidth = 2),
        text = element_text(size = 30,family="Lato")) + 
  labs(x = "Frame", y = "Expected Gain")

# Final Rankings Tables
library(ggcorrplot)

  # Let's get all of the tables read in so that we can join them
yards1.5 <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/yardsSavedRateRanking1.5.csv")
yards2.5 <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/yardsSavedRateRanking2.5.csv")
  
points1.5 <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/pointsSavedRateRanking1.5.csv")
points2.5 <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/pointsSavedRateRanking2.5.csv")

fullTable <- full_join(full_join(yards1.5 %>% select(-n), yards2.5 %>% select(-n), by = join_by(nflId)),
                       full_join(points1.5 %>% select(-n), points2.5 %>% select(-n), by = join_by(nflId)),
                       by = join_by(nflId)) %>%
  mutate(uniformYards1.5 = round(uwAvgYardsSavedPerOpportunity.x,3),
         invDistYards1.5 = round(dwAvgYardsSavedPerOpportunity.x,3),
         uniformYards2.5 = round(uwAvgYardsSavedPerOpportunity.y,3),
         invDistYards2.5 = round(dwAvgYardsSavedPerOpportunity.y,3),
         uniformPoints1.5 = round(uwAvgPointsSavedPerOpportunity.x,3),
         invDistPoints1.5 = round(dwAvgPointsSavedPerOpportunity.x,3),
         uniformPoints2.5 = round(uwAvgPointsSavedPerOpportunity.y,3),
         invDistPoints2.5 = round(dwAvgPointsSavedPerOpportunity.y,3)) %>%
  select(nflId,uniformYards1.5,invDistYards1.5,uniformYards2.5,invDistYards2.5,
         uniformPoints1.5,invDistPoints1.5,uniformPoints2.5,invDistPoints2.5) %>%
  left_join(players %>% select(nflId,displayName,position), by = join_by(nflId)) %>%
  select(-nflId)

fullTable %>% 
  select(uniformYards1.5,invDistYards1.5,uniformYards2.5,invDistYards2.5) %>%
  arrange(desc(uniformYards1.5)) %>% mutate(`Uniform 1.5` = row_number()) %>% 
  arrange(desc(invDistYards1.5)) %>% mutate(`Inv. Dist. 1.5` = row_number()) %>% 
  arrange(desc(uniformYards2.5)) %>% mutate(`Uniform 2.5` = row_number()) %>% 
  arrange(desc(invDistYards2.5)) %>% mutate(`Inv. Dist. 2.5` = row_number()) %>%
  select(`Uniform 1.5`,`Inv. Dist. 1.5`,`Uniform 2.5`,`Inv. Dist. 2.5`) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(type = "upper", show.legend = F) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 30,family="Lato"),
        axis.text.x = element_text(angle = 30))
ggsave(filename = '/Users/aaronfoote/COURSES/Projects/With Greg/CASSIS Poster Visuals/yardsHeatmap.tiff',
       device='tiff', dpi=1600)

fullTable %>%
  select(uniformPoints1.5,invDistPoints1.5,uniformPoints2.5,invDistPoints2.5) %>%
  arrange(desc(uniformPoints1.5)) %>% mutate(`Uniform 1.5` = row_number()) %>% 
  arrange(desc(invDistPoints1.5)) %>% mutate(`Inv. Dist. 1.5` = row_number()) %>% 
  arrange(desc(uniformPoints2.5)) %>% mutate(`Uniform 2.5` = row_number()) %>% 
  arrange(desc(invDistPoints2.5)) %>% mutate(`Inv. Dist. 2.5` = row_number()) %>%
  select(`Uniform 1.5`,`Inv. Dist. 1.5`,`Uniform 2.5`,`Inv. Dist. 2.5`) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(type = "upper", show.legend = F, lab = T) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 30,family="Lato"),
        axis.text.x = element_text(angle = 30))
ggsave(filename = '/Users/aaronfoote/COURSES/Projects/With Greg/CASSIS Poster Visuals/pointsHeatmap.tiff',
       device='tiff', dpi=1600)

yardsRate <- fullTable %>% 
  select(displayName,position,uniformYards1.5,invDistYards1.5) %>% 
  arrange(desc(invDistYards1.5)) %>% head(10) %>%
  rename(Player = displayName, Position = position, Uniform = uniformYards1.5, `Inverse Dist.` = invDistYards1.5)
library(reactablefmtr)
colorPalette <- c("#FFFFFF","#EB9090","#D72121")
yardsRateTable <- reactable(
  yardsRate, defaultSorted = 'Inverse Dist.', defaultSortOrder = 'desc',
  columns = list(
    'Uniform' = colDef(style = color_scales(yardsRate, colors = colorPalette),width = 80),
    'Inverse Dist.' = colDef(style = color_scales(yardsRate,colors = colorPalette),width = 105),
    'Player' = colDef(width = 135), 'Position' = colDef(width = 75)
  ),
  theme = reactableTheme(style = list(fontFamily = "Lato")
  )
)
save_reactable(yardsRateTable,"/Users/aaronfoote/COURSES/Projects/With Greg/CASSIS Poster Visuals/yardsRateTable.html")


pointsRate <- fullTable %>% 
  select(displayName,position,uniformPoints1.5,invDistPoints1.5) %>% 
  arrange(desc(invDistPoints1.5)) %>% head(10) %>%
  rename(Player = displayName, Position = position, Uniform = uniformPoints1.5, `Inverse Dist.` = invDistPoints1.5)
pointsRateTable <- reactable(
  pointsRate, defaultSorted = 'Inverse Dist.', defaultSortOrder = 'desc',
  columns = list(
    'Uniform' = colDef(style = color_scales(pointsRate, colors = colorPalette),width = 80),
    'Inverse Dist.' = colDef(style = color_scales(pointsRate,colors = colorPalette),width = 105),
    'Player' = colDef(width = 135), 'Position' = colDef(width = 75)
  ),
  theme = reactableTheme(style = list(fontFamily = "Lato")
  )
)
save_reactable(pointsRateTable,"/Users/aaronfoote/COURSES/Projects/With Greg/CASSIS Poster Visuals/pointsRateTable.html")


yardsNet <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/yardsSavedNetRanking.csv")
pointsNet <- read_csv("/Users/aaronfoote/COURSES/Projects/With Greg/pointsSavedNetRanking.csv")

yardsNet <- left_join(yardsNet %>% select(nflId,uwYardsSaved,dwYardsSaved),
                      players %>% select(nflId,displayName,position),
                      by = join_by(nflId)) %>%
  select(-nflId) %>%
  rename(Player = displayName, Position = position,
         Uniform = uwYardsSaved, `Inverse Dist.` = dwYardsSaved) %>%
  arrange(desc(`Inverse Dist.`)) %>% head(10) %>% 
  select(Player,Position,Uniform,`Inverse Dist.`) %>%
  mutate(Uniform = round(Uniform,1), `Inverse Dist.` = round(`Inverse Dist.`,1))

pointsNet <- left_join(pointsNet %>% select(nflId,uwPointsSaved,dwPointsSaved),
                       players %>% select(nflId,displayName,position),
                       by = join_by(nflId)) %>%
  select(-nflId) %>%
  rename(Player = displayName, Position = position,
         Uniform = uwPointsSaved, `Inverse Dist.` = dwPointsSaved) %>%
  arrange(desc(`Inverse Dist.`)) %>% head(10) %>% 
  select(Player,Position,Uniform,`Inverse Dist.`) %>%
  mutate(Uniform = round(Uniform,2), `Inverse Dist.` = round(`Inverse Dist.`,2))

yardsNetTable <- reactable(
  yardsNet, defaultSorted = 'Inverse Dist.', defaultSortOrder = 'desc',
  columns = list(
    'Uniform' = colDef(style = color_scales(yardsNet, colors = colorPalette),width = 80),
    'Inverse Dist.' = colDef(style = color_scales(yardsNet,colors = colorPalette),width = 105),
    'Player' = colDef(width = 150), 'Position' = colDef(width = 75)
  ),
  theme = reactableTheme(style = list(fontFamily = "Lato")
  )
)

save_reactable(yardsNetTable,"/Users/aaronfoote/COURSES/Projects/With Greg/CASSIS Poster Visuals/yardsNetTable.html")

pointsNetTable <- reactable(
  pointsNet, defaultSorted = 'Inverse Dist.', defaultSortOrder = 'desc',
  columns = list(
    'Uniform' = colDef(style = color_scales(pointsNet, colors = colorPalette),width = 80),
    'Inverse Dist.' = colDef(style = color_scales(pointsNet,colors = colorPalette),width = 105),
    'Player' = colDef(width = 150), 'Position' = colDef(width = 75)
  ),
  theme = reactableTheme(style = list(fontFamily = "Lato")
  )
)

save_reactable(pointsNetTable,"/Users/aaronfoote/COURSES/Projects/With Greg/CASSIS Poster Visuals/pointsNetTable.html")
