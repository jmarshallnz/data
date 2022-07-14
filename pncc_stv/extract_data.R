library(tabulizer)
library(tidyverse)

tab <- extract_tables(file = "iteration-report-13-oct-2019.pdf")

test <- bind_rows(tab[3:25] %>% map(as.tibble) %>% map_df(set_names, nm = c("Iter", "Candidate", "Votes")))

final <- test %>% mutate(Iter = as.numeric(Iter), Votes = parse_number(Votes))

ggplot(final, aes(x=Iter, y=Votes, fill=Candidate)) + geom_col(position='stack', col='black')

# yay!

# Unfortunately, we don't have the information needed to figure out where the vote go, as the votes
# from the top 5 candidates have all been reallocated at once.

# now, HTF do you do a sankey
library(ggalluvial)

ggplot(test,
       aes(,
           y=Votes)) +
  geom_flow()

# we need to figure out the flows I guess so we can put the stratum in?