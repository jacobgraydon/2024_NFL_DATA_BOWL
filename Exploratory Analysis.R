library(nflfastR)
library(dplyr)
library(gt)
library(ggplot2)
library(ggimage)
library(ggthemes)

Week_1_Final_Output <- read.csv("Downloads/2024 NFL Data Bowl/Week_1_Output.csv") %>%
  distinct()
Week_2_Final_Output <- read.csv("Downloads/2024 NFL Data Bowl/Week_2_Output.csv") %>%
  distinct()
Week_3_Final_Output <- read.csv("Downloads/2024 NFL Data Bowl/Week_3_Output.csv") %>%
  distinct()
Week_4_to_6_Final_Output <- read.csv("Downloads/2024 NFL Data Bowl/Week_4_to_6_Output.csv") %>%
  distinct()
Week_7_to_9_Final_Output <- read.csv("Downloads/2024 NFL Data Bowl/Week_7_to_9_Output.csv") %>%
  distinct()

All_Output <- rbind(Week_1_Final_Output,Week_2_Final_Output,Week_3_Final_Output,
                    Week_4_to_6_Final_Output,Week_7_to_9_Final_Output)



All_Output<-All_Output %>%
  mutate(
    Open_Field_Tackle_Opportunity = Open_Field_Tackle_IND + Open_Field_Missed_Tackle_IND
  )







str(All_Output)
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
         solo_tackle,defense_personnel,was_pressure,posteam,defteam,defense_man_zone_type,out_of_bounds,
         defense_coverage_type,n_defense_box,n_blitzers,success,epa,air_epa,yac_epa,xyac_epa,surface,touchdown,touchback)

extended_pbp$old_game_id <- as.integer(extended_pbp$old_game_id)
str(extended_pbp)

Master_File <- All_Output %>%
  left_join(extended_pbp, by = c("gameId" = "old_game_id", "playId" = "play_id"))

Master_File %>%
  group_by(week) %>%
  summarise(games = n_distinct(gameId),
            plays = n())
#
Master_File %>% 
  distinct() %>% 
  filter(play_type %in% c('pass','run')) %>%
  group_by(gameId,playId) %>% 
  summarise(rows = n()) %>% 
  arrange(-rows)

Master_File %>% 
  distinct(gameId,playId,play_type,Open_Field_Tackle_IND,Open_Field_Tackle_Opportunity) %>% 
  group_by(gameId,playId,play_type) %>% 
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity)) %>% 
  group_by(play_type)
  


# Pass vs Run
Master_File %>% 
  distinct() %>% 
  filter(play_type %in% c('pass','run')) %>%
  mutate(helper = paste0(gameId,playId)) %>% 
  group_by(play_type) %>% 
  summarise(plays = n_distinct(helper),
            Open_Field_Tackles = sum(Open_Field_Tackle_IND),
            Open_Field_Tackle_Opportunities = sum(Open_Field_Tackle_Opportunity),
            Converted_OFT = Open_Field_Tackles/Open_Field_Tackle_Opportunities) %>%
  arrange(Converted_OFT) 

# Load in 2022 PBP
pbp_2022 <- nflreadr::load_participation(2022, include_pbp = T)
pbp_for_hank <- pbp_2022 %>%
  left_join(FTN_2022, by = c("nflverse_game_id","play_id" = "nflverse_play_id","week")) %>%
  select(old_game_id,week,play_id,desc,play_type,is_motion,is_play_action, is_rpo,
         solo_tackle,defense_personnel,was_pressure,posteam,defteam,defense_man_zone_type,out_of_bounds,
         defense_coverage_type,n_defense_box,n_blitzers,success,epa,air_epa,yac_epa,xyac_epa,surface,touchdown,touchback) %>% 
  filter(week <= 9,
         play_type %in% c('pass','run'))

#write.csv(pbp_for_hank, file = "/Users/staci/Downloads/2024 NFL Data Bowl/pbp_for_hank.csv",row.names = FALSE)

colnames(pbp_2022)
# Find EPA Allowed for Weeks in 2022
def_analysis_2022 <- pbp_2022 %>%
  filter(week <= 9,
         play_type %in% c('pass','run')) %>%
  group_by(defteam) %>%
  summarise(success_rate = mean(success),
            avgepa = mean(epa)) %>%
  arrange(avgepa)


OFT_vs_EPA_Allowed <- Master_File %>%
  filter(play_type %in% c('pass','run')) %>%
  group_by(defteam) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(-OFT_Made_Perc)


OFT_DEF_SCATTER_DATA <- OFT_vs_EPA_Allowed %>%
  left_join(teams_colors_logos,by = c('defteam'='team_abbr')) %>%
  left_join(def_analysis_2022, by = 'defteam')

colnames(OFT_DEF_SCATTER_DATA)





# Scatter with OFT_Made_Perc and Avg EPA Allowed
OFT_DEF_SCATTER_Plot <-OFT_DEF_SCATTER_DATA %>%
  ggplot(aes(x = OFT_Made_Perc, y = avgepa))+
  geom_hline(yintercept = mean(OFT_DEF_SCATTER_DATA$avgepa, linetype = "dashed"))+
  geom_vline(xintercept = mean(OFT_DEF_SCATTER_DATA$OFT_Made_Perc))+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",color = "dodgerblue")+
  geom_point()+
  #geom_smooth(method = "lm", se = FALSE, linetype = "dashed")+
  geom_image(image = OFT_DEF_SCATTER_DATA$team_logo_espn, asp = 16/9, size =.05)+
  scale_y_reverse()+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5))+
  labs(title = "Open Field Tackle % vs Defensive Efficiency",
       subtitle = "Pass/Run Plays",
       caption = "Data: Tracking Data / nflreadr")+
  xlab("Open Field Tackle %")+
  ylab("EPA/Play Allowed")+
  theme_classic()+
  theme(plot.title = element_text(size = 25,face = "bold", hjust = .5),
        plot.subtitle = element_text(size = 15, hjust = .5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),  # Adjust x-axis label size
        axis.text.y = element_text(size = 14),
        plot.caption = element_text(size = 11))

ggsave('/Users/staci/Downloads/2024 NFL Data Bowl/OFT_vs_Def_Efficiency.png',
       OFT_DEF_SCATTER_Plot,width = 14.25, height = 10, dpi = "retina")


colnames(Master_File)


# Creating GT Table for Best Open Field Tacklers
Player_Level <- Master_File %>%
  filter(play_type %in% c('pass','run')) %>%
  group_by(defteam,displayName,nflId,position) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(-OFT_Opp)


player_info <- nflreadr::load_players()
colnames(player_info)

Player_Level_Table_Data <- Player_Level %>%
  left_join(player_info %>%
              distinct(gsis_it_id,headshot) %>%
              filter(gsis_it_id != 47786)
            , by = c('nflId'='gsis_it_id')) %>%
  left_join(teams_colors_logos,by = c('defteam'='team_abbr')) %>%
  # Warning Caused from ID: 47786 Quinnen Williams. Only duplicate
  filter(OFT_Opp >= 25) %>%
  arrange(-OFT_Made_Perc) %>%
  ungroup() %>%
  select(team_logo_espn,displayName,headshot,position,OFTs,OFT_Opp,
         OFT_Made_Perc)

colnames(teams_colors_logos)
Best_Open_Field_Tacklers_tbl <- Player_Level_Table_Data %>%
  head(10) %>%
  gt() %>%
  tab_header(
    title = md("**Top Open Field Tacklers**"),
    subtitle = md("*min 25 Open Field Tackle Opportunities")
  ) %>% 
  opt_row_striping() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    team_logo_espn = "Team",
    #team_wordmark = "",
    displayName = "Name",
    headshot = "",
    position = "Position",
    OFTs = "Open Field Tackles",
    OFT_Opp = "Open Field Tackle Opportunities",
    OFT_Made_Perc = "Converted"
  ) %>%
  cols_align(align = "center") %>%
  #gtExtras::gt_img_rows(team_wordmark) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>%
  gtExtras::gt_img_rows(headshot, height = 60) %>%
  gtExtras::gt_color_rows(OFT_Made_Perc, palette = "ggsci::green_material") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "center"
  ) %>% 
  fmt_percent(columns = OFT_Made_Perc, decimals = 1)

gtsave(Best_Open_Field_Tacklers_tbl,"/Users/staci/Downloads/2024 NFL Data Bowl/Top_OF_Tacklers.png")

# Chargers Defensive Preview

Chargers_Def_Data <- Master_File %>%
  filter(play_type %in% c('pass','run')) %>%
  filter(defteam == 'LAC') %>% 
  group_by(defteam,displayName,nflId,position) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(-OFT_Made_Perc) %>% 
  filter(OFT_Opp >= 10) %>% 
  left_join(player_info %>%
              distinct(gsis_it_id,headshot) %>%
              filter(gsis_it_id != 47786)
            , by = c('nflId'='gsis_it_id')) %>%
  left_join(teams_colors_logos,by = c('defteam'='team_abbr'))  %>% 
  ungroup() %>%
  select(team_logo_espn,displayName,headshot,position,OFTs,OFT_Opp,
         OFT_Made_Perc)

# Build table
Chargers_Def_Table <- Chargers_Def_Data %>% 
  gt() %>%
  tab_header(
    title = md("**Top Open Field Tacklers Preview (Chargers)**"),
    subtitle = md("*min 10 Open Field Tackle Opportunities")
  ) %>% 
  opt_row_striping() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    team_logo_espn = "Team",
    #team_wordmark = "",
    displayName = "Name",
    headshot = "",
    position = "Position",
    OFTs = "Open Field Tackles",
    OFT_Opp = "Open Field Tackle Opportunities",
    OFT_Made_Perc = "Converted"
  ) %>%
  cols_align(align = "center") %>%
  #gtExtras::gt_img_rows(team_wordmark) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>%
  gtExtras::gt_img_rows(headshot, height = 60) %>%
  gtExtras::gt_color_rows(OFT_Made_Perc, palette = "ggsci::green_material") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "center"
  ) %>% 
  fmt_percent(columns = OFT_Made_Perc, decimals = 1)

gtsave(Chargers_Def_Table,"/Users/staci/Downloads/2024 NFL Data Bowl/Chargers_Defense_Preview.png")

  













# Creating table for best ball carriers at avoiding OFTs
off_teams <- Master_File %>%
  left_join(Plays %>% 
              select(gameId,playId,ballCarrierDisplayName), by = c('gameId','playId')) %>% 
  filter(play_type %in% c('pass','run')) %>%
  group_by(posteam) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  #filter(OFT_Opp > 45) %>% 
  #filter(OFT_Opp > 25) %>% 
  arrange(OFT_Made_Perc)

colnames(Master_File)

# Player Level for Offense
Player_Level_Offense <- Master_File %>%
  left_join(Plays %>% 
              select(gameId,playId,ballCarrierDisplayName,ballCarrierId), by = c('gameId','playId')) %>% 
  filter(play_type %in% c('pass','run')) %>%
  group_by(posteam,ballCarrierDisplayName,ballCarrierId) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  #filter(OFT_Opp > 45) %>% 
  filter(OFT_Opp > 25) %>% 
  arrange(OFT_Made_Perc)

Player_Level_Offense_Data <- Player_Level_Offense %>% 
  left_join(player_info %>%
              distinct(gsis_it_id,headshot,position) %>%
              filter(gsis_it_id != 47786)
            , by = c('ballCarrierId'='gsis_it_id')) %>%
  left_join(teams_colors_logos,by = c('posteam'='team_abbr')) %>% 
  ungroup() %>% 
  select(team_logo_espn,ballCarrierDisplayName,headshot,position,OFTs,OFT_Opp,
         OFT_Made_Perc)


Best_Open_Field_Offense_tbl <- Player_Level_Offense_Data %>%
  head(10) %>%
  gt() %>%
  tab_header(
    title = md("**Top Ball Carriers at avoiding Open Field Tackles**"),
    subtitle = md("*min 25 Open Field Tackle Opportunities")
  ) %>% 
  opt_row_striping() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    team_logo_espn = "Team",
    #team_wordmark = "",
    ballCarrierDisplayName = "Name",
    headshot = "",
    position = "Position",
    OFTs = "Open Field Tackles",
    OFT_Opp = "Open Field Tackle Opportunities",
    OFT_Made_Perc = "Converted"
  ) %>%
  cols_align(align = "center") %>%
  #gtExtras::gt_img_rows(team_wordmark) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>%
  gtExtras::gt_img_rows(headshot, height = 60) %>%
  gtExtras::gt_color_rows(OFT_Made_Perc, palette = "ggsci::green_material",
                          direction = -1) %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "center"
  ) %>% 
  fmt_percent(columns = OFT_Made_Perc, decimals = 1)

gtsave(Best_Open_Field_Offense_tbl,"/Users/staci/Downloads/2024 NFL Data Bowl/Top_OF_Avoiders.png")

# DAAAA Bears
Bears_Table_Data <- Master_File %>%
  left_join(Plays %>% 
              select(gameId,playId,ballCarrierDisplayName,ballCarrierId), by = c('gameId','playId')) %>% 
  filter(play_type %in% c('pass','run'),
         posteam == 'CHI') %>%
  group_by(posteam,ballCarrierDisplayName,ballCarrierId) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  left_join(player_info %>%
              distinct(gsis_it_id,headshot,position) %>%
              filter(gsis_it_id != 47786)
            , by = c('ballCarrierId'='gsis_it_id')) %>%
  left_join(teams_colors_logos,by = c('posteam'='team_abbr')) %>% 
  filter(OFT_Opp > 10) %>% 
  arrange(OFT_Made_Perc) %>% 
  ungroup() %>% 
  select(team_logo_espn,ballCarrierDisplayName,headshot,position,OFTs,OFT_Opp,
         OFT_Made_Perc)



Bears_Offense_tbl <- Bears_Table_Data %>%
  gt() %>%
  tab_header(
    title = md("**Open Field Tackle Avoidance Preview (Bears)**"),
    subtitle = md("*min 10 Open Field Tackle Opportunities")
  ) %>% 
  opt_row_striping() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    team_logo_espn = "Team",
    #team_wordmark = "",
    ballCarrierDisplayName = "Name",
    headshot = "",
    position = "Position",
    OFTs = "Open Field Tackles",
    OFT_Opp = "Open Field Tackle Opportunities",
    OFT_Made_Perc = "Converted"
  ) %>%
  cols_align(align = "center") %>%
  #gtExtras::gt_img_rows(team_wordmark) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>%
  gtExtras::gt_img_rows(headshot, height = 60) %>%
  gtExtras::gt_color_rows(OFT_Made_Perc, palette = "ggsci::green_material",
                          direction = -1) %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "center"
  ) %>% 
  fmt_percent(columns = OFT_Made_Perc, decimals = 1)

gtsave(Bears_Offense_tbl,"/Users/staci/Downloads/2024 NFL Data Bowl/Bears_Offense_Preview.png")










# Hardest teams to tackle in Open Field
Master_File %>%
  filter(play_type %in% c('pass','run')) %>%
  group_by(posteam) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(OFT_Made_Perc)
# Chicago's effectiveness at missing open field tackles is heavily driven by 
# the trio of Khalil Herbert, David Montgomery, and Justin Fields.
# From Weeks 1-9 the Bears then were 2nd in EPA/Rush


# Turf vs grass
unique(Master_File$surface)
Master_File %>% 
  filter(play_type %in% c('pass','run'),
         surface != '') %>%
  mutate(surface_ind = ifelse(grepl("turf", surface, ignore.case = TRUE), "turf", "grass")) %>% 
  group_by(surface_ind) %>% 
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(OFT_Made_Perc) 

# Man vs Zone (only applicable for pass plays)
Master_File %>% 
  filter(play_type == 'pass') %>% 
  group_by(defense_man_zone_type) %>% 
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(OFT_Made_Perc) 
# Defensive Coverage Level
Master_File %>% 
  filter(play_type == 'pass') %>% 
  group_by(defense_man_zone_type,defense_coverage_type) %>% 
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(OFT_Made_Perc) 

# Looking at xyac_epa vs actual yac_epa
extended_pbp %>%
  left_join(Plays %>% 
              select(gameId,playId,ballCarrierDisplayName), by = c('old_game_id'='gameId',
                                                                   'play_id'='playId')) %>% 
  filter(play_type =='pass') %>%
  group_by(posteam,ballCarrierDisplayName) %>%
  summarise(catches = n_distinct(play_id),
            total_xyac_epa = sum(xyac_epa),
            total_yac_epa = sum(yac_epa),
            yac_epa_oe = total_yac_epa - total_xyac_epa) %>%
  arrange(-yac_epa_oe)

# Best teams tackling in open field when blitzing
Master_File %>% 
  filter(play_type %in% c('pass','run')) %>%
  filter(n_blitzers > 0) %>% 
  group_by(defteam) %>%
  summarise(OFTs = sum(Open_Field_Tackle_IND),
            OFT_Opp = sum(Open_Field_Tackle_Opportunity),
            OFT_Made_Perc = OFTs/OFT_Opp) %>%
  arrange(-OFT_Made_Perc)




# Trying to get Caught Frame
i <- 1

#Get_Pass_Frame <- function(x){
Passes_Caught_Frames <- data.frame()


Caught_Passes <- Week_9 %>%
  left_join(Plays %>%
              select(gameId,playId,ballCarrierId,ballCarrierDisplayName),
            by = c("gameId","playId","nflId" = "ballCarrierId")) %>%
  filter(event == 'pass_outcome_caught',
         !is.na(ballCarrierDisplayName))
Passes_Caught_Frames <- bind_rows(Caught_Passes,Passes_Caught_Frames)

unique(Caught_Passes$gameId)
#####################




