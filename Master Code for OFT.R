# Load Packages
library(dplyr)
library(nflfastR)
library(tidyr)



Games <- read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/games.csv")
Players <- read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/players.csv")
Plays <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/plays.csv")
Tackles <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tackles.csv")
Week_1 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_1.csv")
# Tack on player positions to week 1 set
Week_1 <- Week_1 %>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_2 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_2.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_3 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_3.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_4 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_4.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_5 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_5.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_6 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_6.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_7 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_7.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_8 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_8.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")
Week_9 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_9.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")


#AllWeeks <- rbind(Week_1,Week_2,Week_3,Week_4,Week_5,Week_6,Week_7,Week_8,Week_9)

Week_1 <- Week_1 %>%
  left_join(Players %>% select(nflId,position), by = "nflId")



#rm(list = setdiff(ls(),c("Games","Players","Plays","Tackles","AllWeeks")))

Games01 <- Games %>% filter(week == 1)

#AllWeeks <- AllWeeks %>% filter(gameId %in% c(Games01$gameId))


# Tack on player positions to week 1 set

# To loop through entire week apply same for loop logic that is being used for
# frames. The code below is isolating just one game

# Create Empty DF (Not used)
df <- data.frame()

# Assign gameIds (Change for Week)
GameIds <- unique(Week_1$gameId)
GameIds_Week_2 <- unique(Week_2$gameId)
GameIds_Week_3 <- unique(Week_3$gameId)

# Distance Loop
PlayLoop <- function(x) {
  
  
  
  gid <- GameIds[[x]]
  
  game <- Week_1 %>%
    filter(gameId == gid) %>%
    select(everything())
  
  # Loop through unique playIds
  
  unique_playIds <- unique(game$playId)
  
  
  for (pid in unique_playIds) {
    #for (pid in 1126) {
    play <- filter(game, playId == pid)
    
    # Loop through unique frameIds within the play
    unique_frameIds <- unique(play$frameId)
    for (fid in unique_frameIds) {
      
      frame <- filter(play, frameId == fid) %>%
        mutate(nflId = ifelse(is.na(nflId),"football",nflId))
      
      
      # Calculate distqnce
      dist_matrix <- as.matrix(dist(frame %>% select(x, y)))
      dist_matrix <- as.data.frame(dist_matrix)
      dist_matrix$nflId <- frame$nflId
      dist_matrix <- dist_matrix %>%
        select(nflId,everything())
      # Create unique positions to avoid duplicating columns
      frame2 <- frame %>%
        group_by(position) %>%
        mutate(
          position = ifelse(is.na(position),"football",position),
          pos_unique = ifelse(position == "football", "football",paste0(
            position,
            row_number() %>%
              as.character() %>%
              sub("\\.0", "", .) %>%
              sub("0", "", .)
          )
          )
        )
      colnames(dist_matrix) <-c("nflId",frame2$pos_unique)
      Final_Frame <- frame2 %>% left_join(dist_matrix, by = "nflId")
      # Concatenate new results into the output dataframe
      df <- bind_rows(df, Final_Frame)
      
    }
  }
  return(df)
}


length(GameIds)
library(parallel)
# Applying function
DF <- mclapply(1:length(GameIds),function(x) PlayLoop(x),mc.cores = 2)

DF[[length(GameIds)]]



#attach tackle info
Tackles <- Tackles %>%
  left_join(Players %>% select(nflId,displayName), by = "nflId")

Tackles <- Tackles %>%
  rename(tackleDisplayName = displayName) %>%
  mutate(nflId = as.character(nflId))

# Plays_2022 <- load_pbp(2022)
# colnames(Plays_2022)
# Plays_2022_1 <- Plays_2022 %>% select(play_id,old_game_id,touchdown)
#
# colnames(Plays_2022)
#
# Game3Tds <- Plays_2022_1 %>% filter(old_game_id == 2022091100, touchdown ==1)

# Create empty df (Not Used)
Open_Field_DF_3 <- data.frame()


# Run function for getting missed and open field tackles
GetOpenField <- function(x) {
  
  Missed_Tackles <- Tackles %>%
    filter(pff_missedTackle == 1,
           gameId == GameIds[x] ) %>%
    mutate(helper = paste0(gameId,playId))
  
  # Identifying Made Tackles from the game from the calculated distance output
  Made_Tackles <- Tackles %>%
    filter(tackle == 1,
           gameId == GameIds[x]) %>%
    mutate(helper = paste0(gameId,playId))
  
  
  PlayDF <- Plays %>% filter(gameId == GameIds[x])
  i <- 1
  
  df <- DF[[x]] %>%
    filter((playId %in% c(Made_Tackles$playId))|(playId %in% c(Missed_Tackles$playId)))
  plays_game <- unique(DF[[x]]$playId)
  
  while (i <= length(plays_game)) {
    
    Play2 <- plays_game[i]
    
    
    if(Play2 %in% Made_Tackles$playId){
      
      testdf <- df %>%
        #filter(event != c("touchdown","qb_slide","out_of_bounds")) %>%
        left_join(PlayDF %>% mutate(ballCarrierId = as.character(ballCarrierId)) %>%
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
        left_join(PlayDF %>% mutate(ballCarrierId = as.character(ballCarrierId)) %>%
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
  return(Open_Field_DF_3)
}

DF2 <- mclapply(1:length(GameIds),function(x) GetOpenField(x),mc.cores = 4)


# Creating DF for Output
Week_1_Output <- data.frame(do.call(rbind, DF2))

# Writing to CSV to have backup for when function reruns
write.csv(Week_1_Output, file = "/Users/staci/Downloads/2024 NFL Data Bowl/Week_1_Output.csv",row.names = FALSE)


# This step can all be added on once we have all weeks
extended_pbp <- nflreadr::load_participation(2022, include_pbp = T)

colnames(extended_pbp)

extended_pbp <- extended_pbp %>%
  filter(week <= 9) %>%
  select(old_game_id,play_id,everything())

# Tacking on More potential attributes at play level
FTN_2022 <- nflreadr::load_ftn_charting(2022)
colnames(FTN_2022)


extended_pbp <- extended_pbp %>%
  left_join(FTN_2022, by = c("nflverse_game_id","play_id" = "nflverse_play_id","week")) %>%
  select(old_game_id,week,play_id,desc,play_type,is_motion,is_play_action, is_rpo,
         solo_tackle,defense_personnel,was_pressure,posteam,defteam,defense_man_zone_type,
         defense_coverage_type,n_defense_box,n_blitzers,success,epa,air_epa,yac_epa,xyac_epa,surface,touchdown,touchback)

extended_pbp$old_game_id <- as.integer(extended_pbp$old_game_id)
str(extended_pbp)
Master_File <- Week_1_Output %>%
  left_join(extended_pbp, by = c("gameId" = "old_game_id", "playId" = "play_id"))

Master_File <- Master_File %>%
  mutate(
    Open_Field_Tackle_Opportunity = Open_Field_Tackle_IND + Open_Field_Missed_Tackle_IND
  )


Master_File %>%
  filter(Open_Field_Tackle_IND == 1 |Open_Field_Missed_Tackle_IND == 1) %>%
  group_by(club) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp,
            success_rate = mean(success),
            avgepa = mean(epa)) %>%
  arrange(-OFT_Opp)



dissect <- extended_pbp %>%
  left_join(Week_1_Output, by = c("old_game_id" = "gameId", "play_id" = "playId")) %>%
  filter(week == 1) %>%
  filter(play_type %in% c('pass','run')) %>%
  select(old_game_id,week,play_id,desc,play_type,is_motion,is_play_action, is_rpo,
         Open_Field_Tackle_IND,solo_tackle,defense_personnel,was_pressure,posteam,defteam,defense_man_zone_type,
         defense_coverage_type,n_defense_box,n_blitzers,success,epa,air_epa,yac_epa,xyac_epa,surface,touchdown,touchback)


