library(tidyverse)
install.packages("dplyr")
library(dplyr)
library(tidyr)
library(randomForest)
install.packages("randomForest")
install.packages("vip")
library(vip)
install.packages("ranger")
library(ranger)
install.packages("writexl")
library(writexl)

merged <- bind_rows(
  NCAA_2010,
  NCAA_2011,
  NCAA_2012,
  NCAA_2013,
  NCAA_2014,
  NCAA_2015,
  NCAA_2016,
  NCAA_2017
)

merged$made_tourn <- ifelse(merged$Seed =="N/A", 0, 1)

tourney_teams <- merged %>% filter(made_tourn == 1)
tourney_teams <- tourney_teams %>% select(-starts_with("..."))
as.factor(tourney_teams$`Seed #`)


merged <- merged %>%
  mutate(Conf_W = ifelse(is.na(Conf_W), Ovr_W, Conf_W),
         Conf_L = ifelse(is.na(Conf_L), Ovr_L, Conf_L))



merged<-merged %>% filter(!is.na(Seed), !is.na(SRS))

merged <- merged %>% select(-starts_with("..."))

colSums(is.na(merged))


the_field <- glm(made_tourn ~ 
                    `Power 6`+ Ovr_W+ Ovr_L+  SRS+ SOS+ `W-L%` +
                 Conf_W+ Conf_L+  Tm.+ Opp.+ Pace+ FTr+ `3PAr`+`TRB%`+ 
                   `AST%`+ `STL%`+ `BLK%`+ `TS%`+`TOV%`+ `ORB%`+ `FT/FGA` +
                 ORtg+  `eFG%`+
                 `Off Eff.`+ `Dff Eff.`+ `Pwr Rtg.`+ WAB,
                data = merged, family = "binomial")
#conf, home games, and away games 

vip(the_field, num_features = 15)+ theme_bw() +
  theme(axis.text = element_text(size=14, face = "bold")) + ggtitle("Projected Field VIP")+ 
  theme(plot.title = element_text(size = 30),
  )


projected_seed <- lm(`Seed #` ~ 
                   `Power 6`+ Ovr_W+ Ovr_L+  SRS+ SOS+ `W-L%` +
                     Tm.+ Opp.   + +Conf_W+ Conf_L + Pace+
                     ORtg+ FTr+ `3PAr`+ `TS%`+ `TRB%`+ `AST%`+ `STL%`+ `BLK%`+`eFG%`+
                     `TOV%`+ `ORB%`+ `FT/FGA` +`Off Eff.`+ `Dff Eff.`+ `Pwr Rtg.`+ WAB,
                 data = tourney_teams)
#conf, home games, and away games `Conf`
vip(projected_seed, num_features = 15) + theme_bw() +
theme(axis.text = element_text(size=14, face = "bold")) + ggtitle("Projected Seed VIP")+ 
  theme(plot.title = element_text(size = 30),
  )


summary(the_field)
pred_model_2018 <- predict(the_field, NCAA_2018, type = "response")
summary(pred_model_2018)
ncaa_18_projs <- cbind(NCAA_2018, pred_model_2018)


bracket_2018 <- predict(projected_seed, projfield_2018, type = "response")
summary(bracket_2018)
ncaa_18_projs <- cbind(projfield_2018, bracket_2018)

write_xlsx(ncaa_18_projs)





pred_model_2019 <- predict(the_field, NCAA_2019, type = "response")
summary(pred_model_2019)
ncaa_19_projs <- cbind(NCAA_2019, pred_model_2019)

ncaa_19_projs <- ncaa_19_projs %>% select(-starts_with("brack"))

bracket_2019 <- predict(projected_seed, ncaa_19_projs, type = "response")
summary(bracket_2019)
ncaa_19_projs <- cbind(ncaa_19_projs, bracket_2019)

write_xlsx(ncaa_19_projs)
setwd("~/Downloads")
write_xlsx(ncaa_19_projs, "ncaa_19_projs.xlsx")




pred_model_2021 <- predict(the_field, NCAA_2021, type = "response")
summary(pred_model_2021)
NCAA_2021 <- NCAA_2021 %>% select(-("W-L%")) #got rid of win percentage for 2021 model
ncaa_21_projs <- cbind(NCAA_2021, pred_model_2021)

ncaa_19_projs <- ncaa_19_projs %>% select(-starts_with("brack"))

bracket_2021 <- predict(projected_seed, projfield_2021, type = "response")
summary(bracket_2021)
projfield_2021 <- cbind(projfield_2021, bracket_2021)


setwd("~/Downloads")
write_xlsx(projfield_2021, "projfield_2021.xlsx")




pred_model_2022 <- predict(the_field, NCAA_2022, type = "response")
summary(pred_model_2022)
NCAA_2022 <- NCAA_2022 %>% select(-("W-L%")) 
ncaa_22_projs <- cbind(NCAA_2022, pred_model_2022)

ncaa_22_projs <- ncaa_22_projs %>% select(-starts_with("..."))

bracket_2022 <- predict(projected_seed, projfield_2022, type = "response")
summary(bracket_2022)
projfield_2022 <- projfield_2022 %>% select(-("bracket_2022")) 
projfield_2022 <- cbind(projfield_2022, bracket_2022)


setwd("~/Downloads")
write_xlsx(projfield_2022, "projfield_2022.xlsx")
