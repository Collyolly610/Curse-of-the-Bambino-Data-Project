# Project 3
install.packages("Lahman")
# Initial thoughts: I want to do something related to the Curse of the Bambino
# It might be a lot of data. I want to see if there was a significant difference in both regular and post season success in the Boston Red Sox franchise before 1918, between 1918 to 2003, and 2004 and onward
# This might be a huge project, but my parents are good friends with Jason Varitek, so I must do him justice.
# This next bit is everything we did in class
library(tidyverse)
library(Lahman)
glimpse(People)
People |>
  filter(nameFirst == "Bryce", nameLast == "Harper")
glimpse(Batting)
People |>
  filter(nameFirst == "Bryce", nameLast == "Harper") |>
  left_join(Batting) |>
  select(yearID, HR, BB)
# Back to the Project... Chat says that it is very much possible. I am excited.
# I'm going to begin defining eras.
curse_era <- function(year) {
  if (year <= 1918) "Before"
  else if (year <= 2003) "During"
  else "After"}
# Before I continue, I think it is important to identify exactly what the Curse of the Bambino is, in order to identify what metrics we should use to test.
# After invigorating research on Wikipedia, the general public believes that the curse was only reflected in the lack of winning the world series, But I would like to provide more evidence outside of world series wins. I give you my favorite offensive statistic, OPS.
# If I measure OPS, or On-Base Plus Slugging, for both regular and post-seasons, we can gain a grasp to see if the curse is legit.
# OPS is not recorded in any of the data sets, so I will create it right now:
redsox_ops <- Teams |>
  filter(teamID == "BOS") |>
  select(yearID, teamID, G, AB, H, X2B = "2B", X3B = "3B", HR, BB, HBP, SF) |>
  mutate(
    TB = H + X2B + 2*X3B + 3*HR, # This is the formula for total bases
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF), # This is the formula for On-Base Percent
    SLG = TB / AB, # This is the formula for Slugging Percent
    OPS = OBP + SLG) # This is the formula for OPS or On-Base Plus Slugging
# This code did not work. After checking things over with Chat, my problem seems to lie in the fact that "2B" and "3B" need to be in "'" because it is saying the that the column "2B" does not exist.
# Fixed code:
redsox_ops <- Teams |>
  filter(teamID == "BOS") |>
  select(yearID, teamID, G, AB, H, X2B = '2B', X3B = '3B', HR, BB, HBP, SF) |>
  mutate(
    TB = H + X2B + 2*X3B + 3*HR, # This is the formula for total bases
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF), # This is the formula for On-Base Percent
    SLG = TB / AB, # This is the formula for Slugging Percent
    OPS = OBP + SLG) # This is the formula for OPS or On-Base Plus Slugging
# LOL the code didn't work again. Chat GPT was wrong. Let me check to see if the column names actually exist in the "Teams" data set.
colnames(Teams)
# Okay the names exist, but I got them mixed up with a visual in the Github. For the code to function, I just need "X2B" and "X3B".
# Third try is the charm:
redsox_ops <- Teams |>
  filter(teamID == "BOS") |>
  select(yearID, teamID, G, AB, H, X2B, X3B, HR, BB, HBP, SF) |>
  mutate(
    TB = H + X2B + 2*X3B + 3*HR, # This is the formula for total bases
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF), # This is the formula for On-Base Percent
    SLG = TB / AB, # This is the formula for Slugging Percent
    OPS = OBP + SLG) # This is the formula for OPS or On-Base Plus Slugging
# It worked! Hooray.
# I will now test with one year from each era: 1912, 1998, and 2009. Those were chosen by me because they seem fun.
redsox_ops |>
  filter(yearID %in% c(1912, 1998, 2009)) |>
  select(yearID, OPS) |>
  arrange(yearID)
# This shows a little bit of what I feared. Some stats weren't tracked. In order to do this I will see which stats were tracked in the earliest seasons.
# I'm pretty sure the first year of data in the Lahman data base was 1871, but let me check:
Teams |>
  filter(teamID == "BOS") |>
  summarize(min(yearID)) |>
  pull()
# After consulting the website, it appears as if the data is tracked for batting and pitching in 1871, but the team stats for the Red Sox began in '01.
# Testing my hypothesis:
Batting |>
  filter(teamID == "BOS") |>
  summarize(min(yearID)) |>
  pull()
# Embarrassing. After a quick Google search it appears as though the Red Sox was founded in 1901. 
# Now on to see what stats were tracked in 1901, going back to the "Teams" data set:
Teams |>
  filter(teamID == "BOS", yearID == 1901) |>
  summarize(across(everything(), ~ !is.na(.)))
# That summary is ugly. It appears as though the following stats were not recorded:
# divID, DivWin, WCWin, WSWin, CS, SF
# Oh no! SF or Sacrifice flies are missing, which is calculated in modern day OBS, thus OPS. To keep the stats fair and eliminate NAs, we will use the old formula to calculate OBS and OPS. New code:
redsox_ops_historic <- Teams |>
  filter(teamID == "BOS") |>
  select(yearID, teamID, G, AB, H, X2B, X3B, HR, BB, HBP) |> #SF is no longer inculded.
  mutate(
    TB = H + X2B + 2*X3B + 3*HR, # This is the formula for total bases
    OBP_historic = (H + BB) / (AB + BB), # This is the formula for On-Base Percent as it was calcuated before the 1954 season, where Sac Flies were recorded.
    SLG = TB / AB, # This is the formula for Slugging Percent
    OPS_historic = OBP_historic + SLG) # This is the formula for OPS or On-Base Plus Slugging
# The code works! Hooray. Now to check and see if there were any missing stats in any years ever to calculate our historic OPS.
any(is.na(redsox_ops_historic))
# Dang it.
redsox_ops_historic |>
  filter(if_any(everything(), is.na))
# Hold the dang it. It appears as though Batters hit by pitch, or HBP, was not tracked between 1911 and 1969, which appears to be the reason why the NA appears.
# That however, shouldn't even be included in our new redsox_ops_histroic table because it isn't used to calculate historic OBS thus OPS. Fixed table:
redsox_ops_historic <- Teams |>
  filter(teamID == "BOS") |>
  select(yearID, teamID, G, AB, H, X2B, X3B, HR, BB) |> #HBP is no longer included.
  mutate(
    TB = H + X2B + 2*X3B + 3*HR, # This is the formula for total bases
    OBP_historic = (H + BB) / (AB + BB), # This is the formula for On-Base Percent as it was calcuated before the 1954 season, where Sac Flies were recorded.
    SLG = TB / AB, # This is the formula for Slugging Percent
    OPS_historic = OBP_historic + SLG) # This is the formula for OPS or On-Base Plus Slugging
# It works. Now to test.
any(is.na(redsox_ops_historic))
# FALSE BABY!!! FIRST METRIC CALCULATED!!!
# Just to tease, I want to see what a chart looks like between curse eras.
# Here is the code:
# LOL I remembered I need to add curse era to the redsox_ops_historic table:
redsox_ops_historic <- redsox_ops_historic |>
  mutate(
    era = factor(curse_era(yearID), levels = c("Before", "During", "After")))
# It broke and opened up a "browser" tab on RStudio. I am scared.
# I am no longer scared. My function in the beginning was just poorly written. I will write it again lol
curse_era <- function(year) {
  case_when(
    year <= 1918 ~ "Before",
    year <= 2003 ~ "During",
    TRUE ~ "After")
}
# I think that fixed it? Let me check...
redsox_ops_historic
# Frick bro. What did I make it do??? Why is Browse[1] here and why is it scary.
# Fixing:
redsox_ops_historic <- redsox_ops_historic |>
  mutate(
    era = curse_era(yearID),
    era = factor(era, levels = c("Before", "During", "After")))
# Okay I just reloaded the page, ran the library functions, the code between 99 and 106, the code between 118 and 123, and the code between 128 and 131. It worked. Strange.
# Checking again:
redsox_ops_historic
# The error messages are gone. There is no Browse[1] haunting me anymore. I think my function I wrote at the beginning works, but I will stick with the code between 118 and 123 because it is currently working now.
# Now for the side by box plots:
ggplot(redsox_ops_historic, aes(x = era, y = OPS_historic, fill = era)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Red Sox OPS by Curse Era (Historic Formula)",
    x = "Era",
    y = "OPS"
  ) +
  scale_fill_manual(values = c("Before" = "#1f77b4",
                               "During" = "#ff7f0e",
                               "After" = "#2ca02c")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
# Wow. That is cool. I am however worried about baseball evolving over time. Maybe this trend appears all over the league.
# Onto making ops_historic for the entire league:
mlb_ops_historic <- Teams |>
  filter(yearID >= 1901) |> # Becasue some data is collected before 1901 for other teams, I don't want baseball treds from 1871 to 1900 to affect our data. We will only use the year 1901 and on for this reason.
  select(yearID, teamID, G, AB, H, X2B, X3B, HR, BB) |>   # Only stats needed for historic OPS
  mutate(
    TB = H + X2B + 2*X3B + 3*HR, # Total Bases
    OBP_historic = (H + BB) / (AB + BB), # Historic OBP (no HBP or SF)
    SLG = TB / AB, # Slugging
    OPS_historic = OBP_historic + SLG, # Historic OPS
    era = case_when(
      yearID <= 1918 ~ "Before",
      yearID <= 2003 ~ "During",
      TRUE ~ "After"), # This adds the era column. I'm not going to remake another function to make the curse_era to 1901 to 1917.
    era = factor(era, levels = c("Before", "During", "After")))
# No errors on the first try. Sunglasses emoji
# I took a break and found out you can add emojis to R. Let me try that again:
# No errors on the first try 😎
# In order to compare the Historic OPS of the Red Sox versus everyone else, we also need to add a column telling us the team:
mlb_ops_historic <- mlb_ops_historic |>
  mutate(
    team_group = if_else(teamID == "BOS", "Red Sox", "Other MLB Teams"))
# EZ mode. Now we can look at the box plots:
ggplot(mlb_ops_historic, aes(x = era, y = OPS_historic, fill = team_group)) +
  geom_boxplot(alpha = 0.75, position = position_dodge(width = 0.8)) +
  labs(
    title = "Historic OPS Comparison: Red Sox vs. MLB by Curse Era",
    x = "Era",
    y = "OPS (Historic Formula)",
    fill = "Team Group"
  ) +
  scale_fill_manual(values = c("Red Sox" = "red", "Other MLB Teams" = "gray50")) +
  theme_minimal(base_size = 14)
# This data is far more interesting. It appears as though mean Historic OPS after the curse has been broken is significantly higher than that of the rest of the MLB. Interesting.
# I've made great progress. It is late, but I will get a head start on one more thing...
# POST SEASON DATA!! Same thing as before. Here we go:
mlb_postseason_ops_historic <- BattingPost |> 
  filter(yearID >= 1901) |> # Same reason for filtering as before
  group_by(yearID, teamID) |> # Notice my comment in line 195. 
  summarize( 
    AB = sum(AB, na.rm = TRUE), 
    H = sum(H, na.rm = TRUE), 
    X2B = sum(X2B, na.rm = TRUE), 
    X3B = sum(XB3, na.rm = TRUE), 
    HR = sum(HR, na.rm = TRUE), 
    BB = sum(BB, na.rm = TRUE), 
    .groups = "drop" ) |> # You may ask yourself: Why are you summarizing here when you didn't sum in the Team dataset? Good question. The team dataset tracks team data: IE everything is alredy summed. The BattingPost dataset is individual, so we need to add them together to create team Postseason Batting data 
  mutate( 
    TB = H + X2B + 2*X3B + 3*HR, # We've been through this part you know what this means by now lol
    OBP_historic = (H + BB) / (AB + BB), 
    SLG = TB / AB, 
    OPS_historic = OBP_historic + SLG, 
    era = case_when( 
      yearID <= 1918 ~ "Before", 
      yearID <= 2003 ~ "During", 
      TRUE ~ "After" ), 
     era = factor(era, levels = c("Before", "During", "After")))
# It didn't work. Boooo
# It was only a typo! Hooray! Fixed code:
mlb_postseason_ops_historic <- BattingPost |> 
  filter(yearID >= 1901) |> 
  group_by(yearID, teamID) |> 
  summarize( 
    AB = sum(AB, na.rm = TRUE), 
    H = sum(H, na.rm = TRUE), 
    X2B = sum(X2B, na.rm = TRUE), 
    X3B = sum(X3B, na.rm = TRUE), # No more typo
    HR = sum(HR, na.rm = TRUE), 
    BB = sum(BB, na.rm = TRUE), 
    .groups = "drop" ) |> 
  mutate( 
    TB = H + X2B + 2*X3B + 3*HR,
    OBP_historic = (H + BB) / (AB + BB), 
    SLG = TB / AB, 
    OPS_historic = OBP_historic + SLG, 
    era = case_when( 
      yearID <= 1918 ~ "Before", 
      yearID <= 2003 ~ "During", 
      TRUE ~ "After" ), 
    era = factor(era, levels = c("Before", "During", "After")))
# Hooray no errors. Now to find Boston only data and make a table:
bos_postseason_ops_historic <- mlb_postseason_ops_historic |>
  filter(teamID == "BOS")
# Okay awesome. Now to mutate the entire MLB postseason OPS table to add a column if you are boston or not:
mlb_postseason_ops_historic <- mlb_postseason_ops_historic |>
  mutate(
    team_group = if_else(teamID == "BOS", "Red Sox", "Other MLB Teams"))
# No errors! Checking:
mlb_postseason_ops_historic
# She works! Now to see the postseason only visual:
ggplot(mlb_postseason_ops_historic,
       aes(x = era, y = OPS_historic, fill = team_group)) +
  geom_boxplot(alpha = 0.75,
               position = position_dodge(width = 0.8)) +
  labs(
    title = "Postseason Historic OPS: Red Sox vs. MLB by Curse Era",
    x = "Era",
    y = "Postseason OPS (Historic Formula)",
    fill = "Team Group"
  ) +
  scale_fill_manual(
    values = c(
      "Red Sox" = "red",
      "Other MLB Teams" = "gray50"
    )
  ) +
  theme_minimal(base_size = 14)
# These box plots are even more telling IMO. Before the curse, they were significantly above average, during the curse they were average, after the curse the are once again above average.
# Fun! Now I can combine the data sets and compare them together!
mlb_combined_ops <- mlb_ops_historic |>
  left_join(mlb_postseason_ops_historic,
            by = c("yearID", "teamID", "era"))
# It seems to have worked! Now to add the Red Sox Identifier:
mlb_combined_ops <- mlb_combined_ops |>
  mutate(
    team_group = if_else(teamID == "BOS", "Red Sox", "Other MLB Teams"))
# Awesome. Now let's see this combined data visual:
# Shoot. I just realized that because I left joined generally without more identifiers, some OPS stat things got over written. Let me retry the join after rerunning all of the correct code.
# Now onto renaming:
mlb_ops_historic <- mlb_ops_historic |> # This is to rename all of the regular season. 
  rename(
    TB_regular = TB,
    OBP_historic_regular = OBP_historic,
    SLG_regular = SLG,
    OPS_historic_regular = OPS_historic)
mlb_postseason_ops_historic <- mlb_postseason_ops_historic |> # This is for post season.
  rename(
    TB_post = TB,
    OBP_historic_post = OBP_historic,
    SLG_post = SLG,
    OPS_historic_post = OPS_historic)
mlb_combined_ops <- mlb_ops_historic |> # This is me now combining the data once and for all.
  left_join(
    mlb_postseason_ops_historic,
    by = c("yearID", "teamID", "era"))
# I need to do the rename thing for all of the variables. RIP
# Take 2:
mlb_ops_historic <- mlb_ops_historic |> # I got all of them this time.
  rename(
    AB_regular = AB,
    TB_regular = TB,
    H_regular = H,
    BB_regular = BB,
    X2B_regular = X2B,
    X3B_regular = X3B,
    HR_regular = HR,
    OBP_historic_regular = OBP_historic,
    SLG_regular = SLG,
    OPS_historic_regular = OPS_historic)
mlb_postseason_ops_historic <- mlb_postseason_ops_historic |>
  rename(
    AB_post = AB,
    TB_post = TB,
    H_post = H,
    BB_post = BB,
    X2B_post = X2B,
    X3B_post = X3B,
    HR_post = HR,
    OBP_historic_post = OBP_historic,
    SLG_post = SLG,
    OPS_historic_post = OPS_historic)
# Testing Time:
mlb_ops_historic
mlb_postseason_ops_historic
# They seemed to work. Now combining:
mlb_combined_ops <- mlb_ops_historic |>
  left_join(
    mlb_postseason_ops_historic,
    by = c("yearID", "teamID", "era")) |>
  mutate(
    team_group = if_else(teamID == "BOS", "Red Sox", "Other MLB Teams"))
# Now to test:
mlb_combined_ops
# Awesome. Now I will recreate the data set, but this time with a way to ID if it was the post season or not. My bad.
ops_long <- mlb_combined_ops |>
  select(
    yearID, teamID, era, team_group,
    OPS_historic_regular, OPS_historic_post) |>
  pivot_longer(
    cols = c(OPS_historic_regular, OPS_historic_post),
    names_to = "season_type",
    values_to = "OPS_historic") |>
  mutate(
    season_type = recode(
      season_type,
      "OPS_historic_regular" = "Regular Season",
      "OPS_historic_post" = "Postseason"))
# Perfect. Now let's see a visual.
ggplot(
  ops_long,
  aes(x = era, y = OPS_historic, fill = team_group)) +
  geom_boxplot(alpha = 0.75,
               position = position_dodge(width = 0.8)) +
  labs(
    title = "Historic OPS Comparison Across Curse Eras",
    subtitle = "Red Sox vs Rest of MLB in the Regular Season and Postseason",
    x = "Curse Era",
    y = "Historic OPS",
    fill = "MLB Team") +
  scale_fill_manual(
    values = c(
      "Red Sox" = "red",
      "Other MLB Teams" = "gray50")) +
  theme_minimal(base_size = 15) +
  facet_grid(
    rows = vars(season_type),
    cols = vars())
# This visual is pretty cool, but also relatively difficult to look at. Maybe instead we can run an anova analysis.
summary(aov(OPS_historic ~ era * team_group * season_type,data = ops_long)) # The missing observations at the bottom result from teams that don't make the post season.
# Awesome. And interesting. We can see that OPS had a significant change over eras, and the Boston was different than the rest of the league. We can also see that the MLB as a whole performed differently in the post-season than the regular season.
# That information is cool and all, but look at this line of data: "era:team_group". This is what we came for. This shows that the Red Sox did significantly different than the rest of the MLB depending on the era.
# This significance tells us that the 1919–2003 Red Sox did not perform relative to MLB the same way they did before the curse and after the curse. The curse is real. I rest my case.