library(ggplot2)
library(tidyverse)
library(fivethirtyeight)

#1)
primaryPolls <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date <- as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPollsCopy <- primaryPolls # so we don't have to read in the data set again every time we change

# Subsetting the data to include only the Super Tuesday states and the relevant candidates
primaryPolls <- primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
primaryPolls <- primaryPolls[primaryPolls$candidate_name%in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"),]

# Plotting the results
# Including changing theme, as well as axis and legend labels
# Also, fixed up the y-axis and the made the date labels easier to read
ggplot(data = primaryPolls, mapping = aes(x=start_date, y=pct, color=candidate_name))+ 
  geom_smooth() + 
  facet_wrap(~candidate_name) + 
  theme_minimal() +
  labs(title= "In Preparation for Super Tuesday: the Current State of the Race") + 
  labs(x="Date", y="Percent Polling", color="2020 Presidential Candidates") +
  scale_y_continuous(limits=c(-10,100)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  

#2)
primaryPolls <- primaryPollsCopy #resetting to the original data
primaryPolls %>% pivot_wider(names_from = state, values_from = pct)

#3)

# Read in libraries at the top
# Reading in data/creating new variables below
polls <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020

# Cleaning up the Endorsements dataset
Endorsements <- rename(Endorsements, candidate_name = endorsee)
Endorsements <- as_tibble(Endorsements)

# Cleaning up the polls dataset
polls <- filter(polls, candidate_name == c("Amy Klobuchar", "Bernard Sanders","Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
polls <- select(polls, candidate_name, sample_size, start_date, party, pct)

# Bringing the two datasets together
unique(Endorsements$candidate_name)
toString(unique(polls$candidate_name)) # disturbed that I got a factor
# Looks like there are only two with different names, Biden and Sanders

polls <- polls %>% mutate(candidate_name = recode(candidate_name, "Bernard Sanders" = "Bernie Sanders"))
polls <- polls %>% mutate(candidate_name = recode(candidate_name, "Joseph R. Biden Jr." = "Joe Biden"))

dems.race.2020 <- polls %>% inner_join(Endorsements, by = "candidate_name")
unique(dems.race.2020$candidate_name) # There are 5!

# Counting endorsements
num_endorsements <- dems.race.2020 %>% group_by(candidate_name) %>% filter(!is.na(candidate_name)) %>% summarise(count = n()) 
p <- ggplot(data = num_endorsements, mapping = aes(x = candidate_name, y = count)) +
    geom_point()
p + labs(title = "Who the Who's Who Think Should Win in 2020", x = "Number of Endorsements", y = "2020 Presidential Candidate") +
  theme_light() + 
  theme(axis.text.y = element_text(angle = 15), axis.text.x = element_text(angle = 15))

