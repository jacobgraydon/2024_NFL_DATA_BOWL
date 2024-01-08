library(dplyr)
library(nflfastR)



Week_1_Pass_Caught <- read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_1.csv") %>% 
  left_join(Players %>% select(nflId,position), by = "nflId") %>% 
  filter(event == 'pass_outcome_caught')
Week_2_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_2.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId") %>% 
  filter(event == 'pass_outcome_caught')
Week_3_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_3.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")%>% 
  filter(event == 'pass_outcome_caught')
Week_4_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_4.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")%>% 
  filter(event == 'pass_outcome_caught')
Week_5_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_5.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")%>% 
  filter(event == 'pass_outcome_caught')
Week_6_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_6.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")%>% 
  filter(event == 'pass_outcome_caught')
Week_7_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_7.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")%>% 
  filter(event == 'pass_outcome_caught')
Week_8_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_8.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")%>% 
  filter(event == 'pass_outcome_caught')
Week_9_Pass_Caught <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_9.csv")%>% 
  left_join(Players %>% select(nflId,position), by = "nflId")%>% 
  filter(event == 'pass_outcome_caught')


All_Weeks_Caught <- rbind(Week_1_Pass_Caught,Week_2_Pass_Caught,Week_3_Pass_Caught,Week_4_Pass_Caught,
                          Week_5_Pass_Caught, Week_6_Pass_Caught,Week_7_Pass_Caught,Week_8_Pass_Caught,
                          Week_9_Pass_Caught)

colnames(All_Weeks_Caught)
colnames(Plays)
All_Weeks_Caught_Frames <-All_Weeks_Caught %>% 
  inner_join(Plays %>%
             select(gameId,playId,ballCarrierId,ballCarrierDisplayName),
           by = c("gameId","playId","nflId" = "ballCarrierId")) %>% 
  rename(pass_caught_frame = frameId,
         pass_catcher_nflId = nflId,
         pass_catcher_displayName = displayName,
         pass_catcher_position = position,
         pass_catcher_club = club) %>% 
  select(gameId,playId,pass_catcher_nflId,pass_catcher_displayName,
         pass_catcher_position,pass_catcher_club,pass_caught_frame)


unique(All_Weeks_Caught_Frames$pass_caught_frame)


dataframe <- Master_File %>% 
  filter(play_type == 'pass') %>% 
  inner_join(All_Weeks_Caught_Frames, by = c('gameId','playId')) %>% 
  select(gameId,playId,frameId,club,position,displayName,Open_Field_Tackle_IND,Open_Field_Missed_Tackle_IND,
         Open_Field_Tackle_Opportunity,week,desc,posteam,defteam,yac_epa,xyac_epa,
         pass_catcher_nflId,pass_catcher_displayName,pass_catcher_position,
         pass_catcher_club,pass_caught_frame) %>% 
  select(frameId,displayName,pass_caught_frame,pass_catcher_displayName,desc)


colnames(Master_File)
########
dataframe_test <- All_Weeks_Caught %>% 
  left_join(Plays %>%
              select(gameId,playId,ballCarrierId,ballCarrierDisplayName),
            by = c("gameId","playId","nflId" = "ballCarrierId")) %>% 
  left_join(Master_File %>% 
              rename(tackler_display_name = displayName) %>% 
              select(gameId,playId,nflId,tackler_display_name,
                     Open_Field_Tackle_IND,Open_Field_Missed_Tackle_IND,
                     Open_Field_Tackle_Opportunity), 
            by = c("gameId","playId","nflId")) %>% 
  filter(displayName == ballCarrierDisplayName | displayName == tackler_display_name) %>% 
  select(gameId,playId,displayName,ballCarrierDisplayName,tackler_display_name,
         Open_Field_Tackle_IND,Open_Field_Missed_Tackle_IND,Open_Field_Tackle_Opportunity,
         everything())



unique_gameIds_yac <- unique(dataframe_test$gameId)

df_yac <- data.frame()
gid <- 2022090800
pid <- 56
fid <- 6


for (gid in unique_gameIds_yac) {
  game <- dataframe_test %>% 
    filter(gameId == gid)
  unique_playIds = unique(game$playId)
  for (pid in unique_playIds) {
    #for (pid in 1126) {
    play <- filter(game, playId == pid)
    
    # Loop through unique frameIds within the play
    unique_frameIds <- unique(play$frameId)
    for (fid in unique_frameIds) {
      frame <- filter(play, frameId == fid)
      
      # Calculate distqnce
      dist_matrix <- as.matrix(dist(frame %>% select(x, y)))
      dist_matrix <- as.data.frame(dist_matrix)
      dist_matrix$nflId <- frame$nflId
      dist_matrix$displayName <- frame$displayName
      dist_matrix <- dist_matrix %>% 
        select(nflId,displayName,everything())
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
      colnames(dist_matrix) <-c("nflId","displayName",frame2$pos_unique)
      Final_Frame <- frame2 %>% left_join(dist_matrix, by = c("nflId","displayName"))
      # Concatenate new results into the output dataframe
      df_yac <- bind_rows(df_yac, Final_Frame)
    }
  }
}



df_yac <- df_yac %>% 
  mutate(Yards_Away = rowSums(df_yac[, 25:49], na.rm = T))
#write.csv(df, file = "/Users/staci/Downloads/2024 NFL Data Bowl/df_yards_away2.csv",row.names = FALSE)

colnames(df_yac)
df_yac %>% 
  distinct() %>% 
  mutate(IND = case_when(
    Yards_Away <= 5 ~ 1,
    TRUE ~ 0
  ))  %>% 
  filter(!is.na(tackler_display_name)) %>% 
  left_join(Master_File %>% 
              select(gameId,playId,displayName,xyac_epa,yac_epa,air_epa),
            by = c("gameId","playId","tackler_display_name"="displayName")) %>% 
  filter(Open_Field_Tackle_Opportunity == 1) %>% 
  summarise(OFT = sum(Open_Field_Tackle_IND, na.rm = T),
            OFT_M = sum(Open_Field_Missed_Tackle_IND, na.rm = T),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity, na.rm = T),
            Plays_for_YAC = sum(IND))




dissect <- df_yac %>% 
  distinct() %>% 
  select(-pos_unique) %>% 
  mutate(IND = case_when(
    Yards_Away <= 5 ~ 1,
    TRUE ~ 0
  )) %>%
  arrange(gameId,playId) %>% 
  filter(Open_Field_Tackle_Opportunity == 1) %>% 
  #filter (gameId == 2022091500) %>% 
  distinct() %>% 
  left_join(
    Master_File %>% 
      select(gameId,playId,displayName,xyac_epa,yac_epa,air_epa,frameId,play_type,
             Open_Field_Tackle_IND,Open_Field_Missed_Tackle_IND,
             Open_Field_Tackle_Opportunity) %>% 
      arrange(gameId,playId),
    by = c("gameId","playId","tackler_display_name"="displayName",#"frameId",
           'Open_Field_Tackle_IND','Open_Field_Missed_Tackle_IND',
           'Open_Field_Tackle_Opportunity')
  ) %>% 
  distinct() %>% 
  # filter(gameId == 2022092507,
  #        playId == 3091) %>% 
  group_by(gameId,playId,play_type,tackler_display_name) %>% 
  mutate(
    Open_Field_Tackle_IND = first(Open_Field_Tackle_IND),
    Open_Field_Missed_Tackle_IND = last(Open_Field_Missed_Tackle_IND)
  ) %>% 
  select(gameId,playId,play_type,tackler_display_name,xyac_epa,yac_epa,Yards_Away,IND,everything()) %>% 
    distinct()


League_EPA <- dissect %>% 
  filter(play_type == 'pass') %>% 
  #filter(club == 'LAC') %>% 
  group_by(Open_Field_Tackle_IND) %>% 
  summarise(Open_Field_Tackle_Opportunities = sum(Open_Field_Tackle_Opportunity, na.rm = T),
            total_xyac_epa = sum(xyac_epa, na.rm = T),
            total_yac_epa = sum(yac_epa, na.rm = T),
            total_yac_epa_saved = total_xyac_epa - total_yac_epa)


Chargers_EPA <- dissect %>% 
  filter(play_type == 'pass') %>% 
  filter(club == 'LAC') %>% 
  group_by(Open_Field_Tackle_IND) %>% 
  summarise(OFT_Opp = sum(Open_Field_Tackle_Opportunity, na.rm = T),
            total_xyac_epa = sum(xyac_epa, na.rm = T),
            total_yac_epa = sum(yac_epa, na.rm = T),
            total_yac_epa_saved = total_xyac_epa - total_yac_epa)

YAC_EPA_DT <- rbind(League_EPA,Chargers_EPA)

YAC_EPA_DT <- YAC_EPA_DT %>% 
  mutate(
    Team = case_when(
    OFT_Opp > 100 ~ 'NFL',
    TRUE ~ 'LAC'
  )) %>% 
  select(Team, everything())


YAC_EPA_DT %>% 
gt() %>%
  tab_header(
    title = md("**YAC Stoppers**"),
    subtitle = md("*min 10 Open Field Tackle Opportunities")
  ) %>% 
  opt_row_striping() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    team_logo_espn = "Team",
    #team_wordmark = "",
    tackler_display_name = "Name",
    headshot = "",
    position = "Position",
    OFT_Opp = "Open Field Tackle Opportunities",
    total_xyac_epa = "Total Expected YAC_EPA Allowed",
    total_yac_epa = "Total Actual YAC_EPA Allowed",
    total_yac_epa_saved = "Total YAC EPA Saved"
  ) %>%
  cols_align(align = "center") %>%
  #gtExtras::gt_img_rows(team_wordmark) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>%
  gtExtras::gt_img_rows(headshot, height = 60) %>%
  gtExtras::gt_color_rows(total_yac_epa_saved, palette = "ggsci::green_material") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "center"
  ) %>% 
  fmt_number(columns = c(total_xyac_epa,total_yac_epa,total_yac_epa_saved), decimals = 1)













yac_vs_xyac_epa_data <- dissect %>% 
  filter(IND == 1,
         Open_Field_Tackle_Opportunity == 1) %>% 
  group_by(club,tackler_display_name,nflId,position) %>% 
  summarise(OFT = sum(Open_Field_Tackle_IND, na.rm = T),
            OFT_M = sum(Open_Field_Missed_Tackle_IND, na.rm = T),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity, na.rm = T),
            Converted_OFT = OFT/OFT_Opp,
            Plays_for_YAC = sum(IND),
            total_xyac_epa = sum(xyac_epa, na.rm = T),
            total_yac_epa = sum(yac_epa, na.rm = T),
            total_yac_epa_saved = total_xyac_epa - total_yac_epa
            ) %>% 
  #filter(OFT_Opp >= 10) %>% 
  arrange(-total_yac_epa_saved) %>% 
  left_join(player_info %>%
              distinct(gsis_it_id,headshot) %>%
              filter(gsis_it_id != 47786)
            , by = c('nflId'='gsis_it_id')) %>%
  left_join(teams_colors_logos,by = c('club'='team_abbr'))  %>% 
  ungroup() %>%
  select(team_logo_espn,club,tackler_display_name,headshot,position,OFT_Opp,total_xyac_epa,total_yac_epa,
         total_yac_epa_saved)

yac_vs_xyac_epa_data %>% 
  filter(OFT_Opp >= 10) %>% 
  head(10) %>% 
  gt() %>%
  cols_hide(club) %>% 
  tab_header(
    title = md("**YAC Stoppers**"),
    subtitle = md("*min 10 Open Field Tackle Opportunities")
  ) %>% 
  opt_row_striping() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    team_logo_espn = "Team",
    #team_wordmark = "",
    tackler_display_name = "Name",
    headshot = "",
    position = "Position",
    OFT_Opp = "Open Field Tackle Opportunities",
    total_xyac_epa = "Total Expected YAC_EPA Allowed",
    total_yac_epa = "Total Actual YAC_EPA Allowed",
    total_yac_epa_saved = "Total YAC EPA Saved"
  ) %>%
  cols_align(align = "center") %>%
  #gtExtras::gt_img_rows(team_wordmark) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>%
  gtExtras::gt_img_rows(headshot, height = 60) %>%
  gtExtras::gt_color_rows(total_yac_epa_saved, palette = "ggsci::green_material") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "center"
   ) %>% 
   fmt_number(columns = c(total_xyac_epa,total_yac_epa,total_yac_epa_saved), decimals = 1)

# Just Chargers
yac_vs_xyac_epa_data_LAC <- yac_vs_xyac_epa_data %>% 
  filter(club == 'LAC') %>% 
  filter(OFT_Opp >= 5) %>% 
  head(10) 



yac_vs_xyac_epa_table_LAC <- yac_vs_xyac_epa_data_LAC %>% 
  gt() %>%
  cols_hide(club) %>% 
  tab_header(
    title = md("**Top Defenders against YAC Preview (Chargers)**"),
    subtitle = md("*min 5 Open Field Tackle Opportunities")
  ) %>% 
  opt_row_striping() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    team_logo_espn = "Team",
    #team_wordmark = "",
    tackler_display_name = "Name",
    headshot = "",
    position = "Position",
    OFT_Opp = "Open Field Tackle Opportunities",
    total_xyac_epa = "Total Expected YAC EPA Allowed",
    total_yac_epa = "Total Actual YAC EPA Allowed",
    total_yac_epa_saved = "Total YAC EPA Saved"
  ) %>%
  cols_align(align = "center") %>%
  #gtExtras::gt_img_rows(team_wordmark) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>%
  gtExtras::gt_img_rows(headshot, height = 60) %>%
  gtExtras::gt_color_rows(total_yac_epa_saved, palette = "ggsci::green_material") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "center"
  ) %>% 
  fmt_number(columns = c(total_xyac_epa,total_yac_epa,total_yac_epa_saved), decimals = 1)

gtsave(yac_vs_xyac_epa_table_LAC,"/Users/staci/Downloads/2024 NFL Data Bowl/Chargers_YAC_EPA.png")






