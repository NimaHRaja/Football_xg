# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2023)




a_match <- function(i){
  Team1 <- epl_results$home_abbr[i]
  Team2 <- epl_results$away_abbr[i]
  probs <- c(epl_results$forecast_win[i],epl_results$forecast_draw[i],epl_results$forecast_loss[i])
  
  ss <- data.frame(
    Team1,
    Team2,
    scenario_id = 1:1000,
    outcome = sample (c(Team1,"DRW",Team2), size=1000, replace=T, prob= probs))
  
  rbind(
    ss %>% mutate(points = if_else(Team1 == outcome, 3, if_else(outcome == "DRW",1,0))) %>% select(Team = Team1, scenario_id, points),
    ss %>% mutate(points = if_else(Team2 == outcome, 3, if_else(outcome == "DRW",1,0))) %>% select(Team = Team2, scenario_id, points)
  ) }


outcome <- do.call("rbind", lapply(1:380, a_match))




outcome %>% 
  group_by(scenario_id, Team) %>% 
  summarise(points = sum(points), .groups = "drop") %>% 
  group_by(scenario_id) %>% 
  mutate(rrr = rank(desc(points),ties.method = "random")) %>% 
  filter(rrr == 1) %>% 
  group_by(Team) %>% 
  summarise(n()) %>% View()
