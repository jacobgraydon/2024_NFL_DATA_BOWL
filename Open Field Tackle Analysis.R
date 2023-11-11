library(ggplot2)
library(dplyr)
library(nflfastR)
library(tidyr)

Games <- read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/games.csv")
Players <- read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/players.csv")
Plays <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/plays.csv")
Tackles <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tackles.csv")
Tackles <- Tackles %>% 
  left_join(Players %>% select(nflId,displayName), by = "nflId")
Tackles <- Tackles %>% 
  rename(tackleDisplayName = displayName) %>% 
  mutate(nflId = as.character(nflId))
Week_1 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_1.csv")
# Tack on player positions to week 1 set
Week_1 <- Week_1 %>% 
  left_join(Players %>% select(nflId,position), by = "nflId")

# The goal of this final code will be to identify our metrics for an open field
# tackle in the NFL. I think a realistic metric that can be created is some sort
# of open field tackle %. We can compile every time an open field tackle opportunity
# is there for a defensive player for Weeks 1-9. The tackles data set will be useful
# to tying back those tackles and missed tackles.This example is using the df made
# from the Calculating Players Distance code for Week 1.

# Tackles %>% 
#   group_by(substr(gameId,1,4)) %>% 
#   summarise(tackles = sum(tackle),
#             assists = sum(assist),
#             missed_tackles = sum(pff_missedTackle))
# OUTPUT:
# `substr(gameId, 1, 4)` tackles assists missed_tackles
# <chr>                    <int>   <int>          <int>
#   1 2022                 9919    5494       2090

# We can enter in open field tackle requirements here when decided on 
# (not all may be applicable)
# x:
# y:
# o:
# dir:


# Open Field Tackle
# gameid = 2022091103
# playid = 1565
# Link to video: https://www.youtube.com/watch?v=XkgLk02XC4M&t=370s
# Skip to 6:00

Tackles %>% 
  filter(gameId == 2022091103, 
         playId == 1565)
# reference game
gameid <- 2022091103
playid <- 1565
PlayDF <- Plays %>% 
  filter(gameId == gameid) %>% 
  mutate(ballCarrierId = as.character(ballCarrierId))
# Join on ball carrier and tackle information
newdf <- df %>% 
  left_join(PlayDF %>% 
              select(gameId,playId,ballCarrierId,ballCarrierDisplayName),
            by = c("gameId","playId","nflId" = "ballCarrierId")) %>% 
  left_join(Tackles %>% 
              filter(tackle == 1 | assist == 1) %>% 
              select(gameId,playId,nflId,tackleDisplayName,tackle,assist),
            by = c("gameId","playId","nflId")) %>% 
  select(gameId,playId,frameId,nflId,displayName,ballCarrierDisplayName,
         tackleDisplayName,tackle,assist,everything())
# reference play
df_ex <- newdf %>% 
  filter(playId == playid)

# Getting ball carrier info for play
ballCarrierPos <- df_ex %>% 
  distinct(ballCarrierDisplayName,pos_unique) %>% 
  filter(!is.na(ballCarrierDisplayName),!is.na(pos_unique)) 
# Assigning position of ball carrier for reference
unique_ball_Carrier_Pos <- unique(ballCarrierPos$pos_unique)

# Getting all frames of tackler and the column that matches the ball carrier's
# unique position
mydata <- df_ex %>% 
  select(tackleDisplayName,nflId,frameId,gameId,playId,pos_unique,
         all_of(unique_ball_Carrier_Pos)) %>% 
  filter(!is.na(tackleDisplayName)) 

# Overwrites mydata data frame by getting the minimum value of ball carrier column
# This in theory should give us the frame where the ball carrier and tackler
# are closest to each other.
mydata <- mydata %>% 
  filter(mydata[7] == min(mydata[7]))

# Getting frame of where the ball carrier and tackler
# are closest to each other.
unique_tackle_frame <- unique(mydata$frameId)

# Goes back to data frame to get all player distances at that frame
Closest_Frame <- df_ex %>% 
  filter(frameId == unique_tackle_frame, nflId == unique(mydata$nflId))

def_pos <- c('CB', 'SS', 'NT', 'DE', 'FS', 'DT', 'OLB', 'ILB','MLB')

# Finds closest defender at that frame 
pwtest <- Closest_Frame %>%
  select(starts_with(def_pos)) %>% 
  pivot_longer(everything()) %>%
  filter(name != Closest_Frame$pos_unique, !is.na(value)) %>% 
  filter(value == min(value))
# OUTPUT
#   name  value
# <chr> <dbl>
#1  CB3    3.15
# At this point, the closest defender was at least 3 yards away from the tackler

# Getting position of closest defender
closest_def_position <- unique(pwtest$name)

# Looking at the frame with the closest defender tacked on
Closest_Frame %>% 
  select(gameId,playId,frameId,nflId,displayName,club,playDirection,x,y,s,a,dis,
         o, dir, position,
         all_of(closest_def_position))
