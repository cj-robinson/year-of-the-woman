library(tidyverse)

women <- read_csv("../data/Bucchianeri_PB_Replication_Data.csv")

women %>% 
  group_by(year, party, GenWin) %>% 
  tally() %>% 
  write_clip()

women %>% 
  group_by(year, party) %>% 
  summarize(total = n(),
            perc_win = sum(primary_winner) / n()) %>% 
  write_clip()

women %>% 
  group_by(year, party, IncumbentOppo) %>% 
  summarize(total = n(),
            perc_win = sum(GenWin) / n()) %>% 
  write_clip()


women %>% 
  group_by(year, party) %>% 
  summarize(total = n(),
            perc_win = sum(GenWin) / n()) %>% 
  ggplot(aes(x = year, y = perc_win, color = party )) + 
  geom_line()


women %>% 
  group_by(year, party, IncumbentOppo) %>% 
  summarize(total = n(),
            perc_win = sum(GenWin) / n()) %>% 
  ggplot(aes(x = year, y = perc_win, color = party )) + 
  facet_wrap(~IncumbentOppo) + 
  geom_line()

women %>% 
  group_by(year, party, GenWin) %>% 
  summarize(total = n()) %>% 
  mutate(c = paste(party, GenWin)) %>% 
  select(year, c, total) %>% 
  pivot_wider(names_from = c, values_from=total) %>% 
  write_csv("/Users/cjrobinson/Downloads/elec_viz.csv")

total_wom <- women %>% 
  group_by(year, party, GenWin) %>% 
  summarize(total = n()) 

women %>% 
  ggplot(aes(x = GenVote, y = PrimMargin, color = party )) + 
  facet_wrap(~Post90) +
  geom_smooth(method = "lm") + 
  geom_point()


# percent change year over year
#who unseated an incumbent and how many were men vs women
# women success rate in flipping a seat vs men
# when women win what's the turnout vs previous races

# new districts with a new incumbent 
# start or increases in funding for nonprofits taht got women into office
# scandal