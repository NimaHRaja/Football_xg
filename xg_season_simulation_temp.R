library(worldfootballR)
library(dplyr)
library(ggplot2)

source("generate_match_samples.R")
source("write_a_batch_points_summary.R")


write_a_batch_points_summary(year = 2020, num_scenarios = 10000, batch_id = 3)


DF <- 
  do.call("rbind", lapply(
    list.files("data/", full.names = TRUE),
    read.csv))

DF %>% 
  group_by(scenario_id, batch_id) %>% 
  mutate(rrr = rank(desc(points),ties.method = "random")) %>% 
  filter(rrr == 1) %>% 
  group_by(Team) %>% 
  summarise(freq = n()) %>% 
  View()



DF %>% 
  group_by(scenario_id, batch_id, Team) %>% 
  summarise(points = sum(points), .groups = "drop") %>% 
  group_by(scenario_id, batch_id) %>% 
  mutate(rrr = rank(desc(points),ties.method = "random")) %>% 
  filter(rrr <= 5) %>% 
  group_by(Team) %>% 
  summarise(freq = n()) %>% 
  View()




DF %>% 
  group_by(scenario_id, batch_id, Team) %>% 
  summarise(points = sum(points), .groups = "drop") %>% 
  group_by(scenario_id, batch_id) %>% 
  mutate(rrr = rank(desc(points),ties.method = "random")) %>% 
  filter(rrr >= 18) %>% 
  group_by(Team) %>% 
  summarise(freq = n()) %>% 
  View()



DDF <- 
  DF %>% 
  group_by(scenario_id, batch_id, Team) %>% 
  summarise(points = sum(points), .groups = "drop") %>% 
  group_by(scenario_id, batch_id) %>% 
  mutate(rrr = rank(desc(points),ties.method = "random")) %>% 
  ungroup() %>% 
  group_by(Team, rrr) %>% 
  summarise(freq = n(), .groups = "drop")


DDF_meanrank <- 
  DDF %>% 
  group_by(Team) %>% 
  summarise(mean_pos = sum(freq * rrr)/sum(freq)) 


DDF$Team <- factor(DDF$Team, levels=(DDF_meanrank$Team)[order(DDF_meanrank$mean_pos)])

DDF %>% 
  ggplot(aes(x = Team, y = rrr, fill = freq)) + 
  geom_tile()



DDF %>% filter(round(freq/sum(freq)*2000,0) > 0) %>% 
  ggplot(aes(x = Team, y = rrr, fill = freq, label = round(freq/sum(freq)*2000,0))) +
  # xlim(levels(unique(DDF$Team)[order(DDF$mean_pos)])) +
  # scale_x_discrete(limits=(DDF$Team)[order(DDF$mean_pos)]) +
  # geom_tile()+
  geom_text() +
  scale_y_continuous(trans = "reverse") 
