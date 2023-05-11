# Participation Infographic

library(ggplot2)
# state/tribal org by total_ave by year

f <- read.csv("/Users/angelo/Desktop/DSC 465 Project/WIC_FULL.csv")
f <- f[,1:22]
f$year <- as.numeric(f$year)

tribeorg.f <- f[grepl(",",f$state_tribalorg),]
write.csv(tribeorg.f,"/Users/Angelo/Desktop/DSC 465 Project/WIC_Tribeorg.csv")
Nontribeorg.f <- f[!grepl(",",f$state_tribalorg),]
write.csv(Nontribeorg.f,"/Users/Angelo/Desktop/DSC 465 Project/WIC_non_Tribeorg.csv")

ggplot(tribeorg.f, aes(x = tribeorg.f$year)) +
  geom_line(aes(y=tribeorg.f$ninfants_avg), color = "red") +
  geom_line(aes(y=tribeorg.f$nchild_avg), color = "blue") +
  geom_line(aes(y=tribeorg.f$nwomen_avg), color = "yellow") 
  


df <- data.frame(unique(f$state_tribalorg))
df$total_ave_2013 <- f[f$year == "2013",8]
df$total_ave_2014 <- f[f$year == "2014",8]
df$total_ave_2015 <- f[f$year == "2015",8]
df$total_ave_2016 <- f[f$year == "2016",8]

df$ninfants_2013 <- f[f$year == "2013",3]
df$ninfants_2014 <- f[f$year == "2014",3]
df$ninfants_2015 <- f[f$year == "2015",3]
df$ninfants_2016 <- f[f$year == "2016",3]

df$nchild_2013 <- f[f$year == "2013",4]
df$nchild_2014 <- f[f$year == "2014",4]
df$nchild_2015 <- f[f$year == "2015",4]
df$nchild_2016 <- f[f$year == "2016",4]

df$nwomen_2013 <- f[f$year == "2013",7]
df$nwomen_2014 <- f[f$year == "2014",7]
df$nwomen_2015 <- f[f$year == "2015",7]
df$nwomen_2016 <- f[f$year == "2016",7]

# 2013 pop - 2016 pop / 2013 pop
df$percent_change <- (df$total_ave_2013 - df$total_ave_2016) / df$total_ave_2013
df$difference <- df$total_ave_2013 - df$total_ave_2016
df_ordered <- df[order(df$percent_change),]
df_ordered <- df_ordered[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

write.csv(df,"/Users/Angelo/Desktop/DSC 465 Project/WIC_Participation_year.csv")

tribeorg.df <- df[grepl(",",df$unique.f.state_tribalorg.),]
write.csv(tribeorg.df,"/Users/Angelo/Desktop/DSC 465 Project/WIC_Tribeorg_Participation_year.csv")
Nontribeorg.df <- df[!grepl(",",df$unique.f.state_tribalorg.),]
write.csv(Nontribeorg.df,"/Users/Angelo/Desktop/DSC 465 Project/WIC_non_Tribeorg_Participation_year.csv")


# Participation Shiny app
library(ggplot2)
library(dplyr)
library(hrbrthemes)

f <- read.csv("/Users/angelo/Desktop/DSC 465 Project/WIC_Participation/data/WIC_FULL.csv")
f <- f[,1:22]
f <- f %>%
  mutate(bool = case_when(
    grepl(",", state_tribalorg) ~ "Tribal_Org",
    !grepl(",", state_tribalorg) ~ "State_Territory"
  ))

f %>%
  ggplot( aes(x=nwomen_avg, fill=bool)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 100) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
# this doesnt look like anything....


# For each row, find the percentage of total_avg for women, infant, children

# make two side-by-side bar graphs, where the tribal orgs % are compared to state/territories
f %>%
  ggplot( aes(x=women_percent, fill=bool)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 100) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

f %>%
  ggplot( aes(x=infant_percent, fill=bool)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 100) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


f <- read.csv("/Users/angelo/Desktop/DSC 465 Project/WIC_Participation/data/WIC_FULL.csv")
f <- f[,1:22]
f <- f %>%
  mutate(bool = case_when(
    grepl(",", state_tribalorg) ~ "Tribal_Org",
    !grepl(",", state_tribalorg) ~ "State_Territory"
  ))
f$women_percent <- f$nwomen_avg / f$total_avg
f$infant_percent <- f$ninfants_avg / f$total_avg
f$child_percent <- f$nchild_avg / f$total_avg

f %>%
  ggplot( aes(x=child_percent, fill=bool)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 100) +
  scale_fill_manual(name = "Participants",
                    labels = c("States & Territories", "Tribal Organizations"),
                    values=c("#69b3a2", "#404080")) +
  xlab("Percent Child")+
  ylab("Count")+
  labs(title = "Percent of Total Participants That Are Children", 
       subtitle = "Comparing Tribal Orgs to States and Territories")

