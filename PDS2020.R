library(ggplot2)
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state=="Alabama"]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"),]

ggplot(data=primaryPolls, mapping=aes(x=start_date, y=pct, color=candidate_name))+
  geom_smooth()+
  facet_wrap(~ state, nrow=2)

#ggplot(data=primaryPolls)+
#  geom_point(mapping = aes(x=start_date, y=pct, color= candidate_name)) +
#  facet_wrap(~ candidate_name, nrow=2) #geom_point can also become geom_smooth
