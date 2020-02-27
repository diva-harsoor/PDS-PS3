library(ggplot2)

#1)
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")

# Subsetting the data to include only the Super Tuesday states and the relevant candidates
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"),]

# Plotting the results, including changing theme, as well as axis and legend labels
ggplot(data=primaryPolls, mapping=aes(x=start_date, y=pct, color=candidate_name))+ 
  geom_smooth() + 
  facet_wrap(~candidate_name) + 
  labs(title= "In Preparation for Super Tuesday: the Current State of the Race") + 
  labs(x="Date", y="Percent Polling", color="Candidates") +
  scale_y_continuous(limits=c(-10,100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  