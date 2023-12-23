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

# Uses output from Calculating Players Distance code
# Identifying Missed Tackles from the game from the calculated distance output
# Will probably put inside while loop once the distance loop is using for multiple
# games
Missed_Tackles <- Tackles %>% 
  filter(pff_missedTackle == 1,
         gameId == unique(df$gameId)) %>% 
  mutate(helper = paste0(gameId,playId))
# Identifying Made Tackles from the game from the calculated distance output
Made_Tackles <- Tackles %>% 
  filter(tackle == 1,
         gameId == unique(df$gameId)) %>% 
  mutate(helper = paste0(gameId,playId))

# Unique plays from game
plays_game <- unique(df$playId)

# Create empty df
Open_Field_DF_3 <- data.frame()
# Set i
i <- 1

# Run while loop
while (i <= length(plays_game)) {
  Play2 <- plays_game[i]
  
  if(Play2 %in% Made_Tackles$playId){
    testdf <- df %>% 
      #filter(event != c("touchdown","qb_slide","out_of_bounds")) %>% 
      left_join(PlayDF %>% 
                  select(gameId,playId,ballCarrierId,ballCarrierDisplayName),
                by = c("gameId","playId","nflId" = "ballCarrierId")) %>% 
      left_join(Tackles %>% 
                  filter(tackle == 1 | assist == 1 | pff_missedTackle == 1) %>% 
                  select(gameId,playId,nflId,tackleDisplayName,tackle,assist,pff_missedTackle),
                by = c("gameId","playId","nflId")) %>% 
      select(gameId,playId,frameId,nflId,displayName,ballCarrierDisplayName,
             tackleDisplayName,tackle,assist,pff_missedTackle,everything())
    
    testdf <- testdf %>% 
      mutate(
        tackler_name = case_when(tackle == 1 ~ tackleDisplayName),
        missed_tackler_name = case_when(pff_missedTackle == 1 ~ tackleDisplayName)
      )
    
    df_ex2 <- testdf %>% 
      filter(playId == Play2)
    
    ballCarrierPos2 <- df_ex2 %>% 
      distinct(ballCarrierDisplayName,pos_unique) %>% 
      filter(!is.na(ballCarrierDisplayName),!is.na(pos_unique)) 
    # Assigning position of ball carrier for reference
    unique_ball_Carrier_Pos2 <- unique(ballCarrierPos2$pos_unique)
    
    # Getting all frames of tackler and the column that matches the ball carrier's
    # unique position
    mydata_tackler <- df_ex2 %>% 
      select(tackler_name,nflId,frameId,gameId,playId,pos_unique,
             all_of(unique_ball_Carrier_Pos2)) %>% 
      filter(!is.na(tackler_name)) 
    
    # Overwrites mydata data frame by getting the minimum value of ball carrier column
    # This in theory should give us the frame where the ball carrier and tackler
    # are closest to each other.
    mydata_tackler <- mydata_tackler %>% 
      filter(mydata_tackler[7] == min(mydata_tackler[7]))
    
    # Getting frame of where the ball carrier and tackler
    # are closest to each other.
    unique_made_tackle_frame <- unique(mydata_tackler$frameId)
    
    # Goes back to data frame to get all player distances at that frame
    Closest_Frame_Made_Tackle <- df_ex2 %>% 
      filter(frameId == unique_made_tackle_frame, nflId == unique(mydata_tackler$nflId))
    
    def_pos <- c('CB', 'SS', 'NT', 'DE', 'FS', 'DT', 'OLB', 'ILB','MLB')
    
    Closest_Defender_Made <- Closest_Frame_Made_Tackle %>%
      select(starts_with(def_pos)) %>% 
      pivot_longer(everything()) %>%
      filter(name != Closest_Frame_Made_Tackle$pos_unique, !is.na(value)) %>% 
      filter(value == min(value))
    
    # Getting position of closest defender of made tackle
    closest_def_position_made <- unique(Closest_Defender_Made$name)
    
    Output_Made <- Closest_Frame_Made_Tackle %>% 
      select(gameId,playId,frameId,nflId,displayName,club,playDirection,x,y,s,a,dis,
             o, dir, position,
             all_of(closest_def_position_made))
    Output_Made <- Output_Made %>% 
      mutate(Open_Field_Tackle_IND = 
               case_when(Output_Made[16] > 2 & 
                           x > 10 &
                           x < 110
                         ~ 1,
                         TRUE ~ 0),
             Open_Field_Missed_Tackle_IND = 0
      ) 
    Output_Made <- Output_Made %>% 
      rename(Closest_Defender = colnames(Output_Made[16]))
    Open_Field_DF_3 <- bind_rows(Open_Field_DF_3,Output_Made)
    
  }
  
  if(Play2 %in% Missed_Tackles$playId){
    testdf <- df %>% 
      #filter(event != c("touchdown","qb_slide","out_of_bounds")) %>% 
      left_join(PlayDF %>% 
                  select(gameId,playId,ballCarrierId,ballCarrierDisplayName),
                by = c("gameId","playId","nflId" = "ballCarrierId")) %>% 
      left_join(Tackles %>% 
                  filter(tackle == 1 | assist == 1 | pff_missedTackle == 1) %>% 
                  select(gameId,playId,nflId,tackleDisplayName,tackle,assist,pff_missedTackle),
                by = c("gameId","playId","nflId")) %>% 
      select(gameId,playId,frameId,nflId,displayName,ballCarrierDisplayName,
             tackleDisplayName,tackle,assist,pff_missedTackle,everything())
    
    testdf <- testdf %>% 
      mutate(
        tackler_name = case_when(tackle == 1 ~ tackleDisplayName),
        missed_tackler_name = case_when(pff_missedTackle == 1 ~ tackleDisplayName)
      )
    
    
    df_ex2 <- testdf %>% 
      filter(playId == Play2)
    
    ballCarrierPos2 <- df_ex2 %>% 
      distinct(ballCarrierDisplayName,pos_unique) %>% 
      filter(!is.na(ballCarrierDisplayName),!is.na(pos_unique)) 
    # Assigning position of ball carrier for reference
    unique_ball_Carrier_Pos2 <- unique(ballCarrierPos2$pos_unique)
    
    # Getting all frames of missed tackler and the column that matches the ball carrier's
    # unique position
    mydata_missed_tackler <- df_ex2 %>% 
      select(missed_tackler_name,nflId,frameId,gameId,playId,pos_unique,
             all_of(unique_ball_Carrier_Pos2)) %>% 
      filter(!is.na(missed_tackler_name)) 
    
    mydata_missed_tackler <- mydata_missed_tackler %>% 
      filter(mydata_missed_tackler[7] == min(mydata_missed_tackler[7]))
    
    unique_missed_tackle_frame <- unique(mydata_missed_tackler$frameId)
    
    # Goes back to data frame to get all player distances at that frame
    Closest_Frame_Missed_Tackle <- df_ex2 %>% 
      filter(frameId == unique_missed_tackle_frame, nflId == unique(mydata_missed_tackler$nflId))
    
    def_pos <- c('CB', 'SS', 'NT', 'DE', 'FS', 'DT', 'OLB', 'ILB','MLB')
    
    Closest_Defender_Missed <- Closest_Frame_Missed_Tackle %>%
      select(starts_with(def_pos)) %>% 
      pivot_longer(everything()) %>%
      filter(name != Closest_Frame_Missed_Tackle$pos_unique, !is.na(value)) %>% 
      filter(value == min(value))
    
    # Getting position of closest defender of missed tackle
    closest_def_position_miss <- unique(Closest_Defender_Missed$name)
    
    # Looking at the frame with the closest defender tacked on for missed tackle
    Output_Missed <- Closest_Frame_Missed_Tackle %>% 
      select(gameId,playId,frameId,nflId,displayName,club,playDirection,x,y,s,a,dis,
             o, dir, position,
             all_of(closest_def_position_miss))
    Output_Missed <- Output_Missed %>% 
      mutate(Open_Field_Tackle_IND = 0,
             Open_Field_Missed_Tackle_IND = 
               case_when(Output_Missed[16] > 2 & 
                           x > 10 &
                           x < 110
                         ~ 1,
                         TRUE ~ 0)
      ) 
    Output_Missed <- Output_Missed %>% 
      rename(Closest_Defender = colnames(Output_Missed[16]))
    
    Open_Field_DF_3 <- bind_rows(Open_Field_DF_3,Output_Missed)
  }
  # keeps loop going
  i <- i + 1
}



colnames(Open_Field_DF_3)
# Assessing output
Open_Field_DF_3 %>% 
  group_by(displayName, position,club) %>% 
  summarise(Tackles = sum(Open_Field_Tackle_IND),
            Open_Field_MT = sum(Open_Field_Missed_Tackle_IND),
            OFT_Opps = Tackles + Open_Field_MT) %>% 
  arrange(-OFT_Opps)
  
# displayName         club  Tackles Open_Field_MT OFT_Opps
# <chr>               <chr>   <dbl>         <dbl>    <dbl>
# 1 Chidobe Awuzie      CIN         6             0        6
# 2 Terrell Edmunds     PIT         3             3        6
# 3 Minkah Fitzpatrick  PIT         5             0        5
# 4 Ahkello Witherspoon PIT         4             0        4
# 5 Germaine Pratt      CIN         2             2        4
# 6 Mike Hilton         CIN         4             0        4
# 7 Logan Wilson        CIN         2             0        2
# 8 Akeem Davis-Gaither CIN         1             0        1
# 9 Alex Highsmith      PIT         1             0        1
# 10 Cameron Sutton     PIT         1             0        1
  
  
  
  