library(dplyr)
library(stringr)
library(ggplot2)
library(rpart.plot)
library(car)
library(interactions)
# Opening and loading in data.
filenames <- list.files("Kickstarter/2019_week19", pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
new <- data.frame(ldf[1]) %>% inner_join(data.frame(ldf[2]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[3]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[4]), by=c("project_slug"))
rm(ldf)

diff_amt <- as.numeric(new$Pledge_USD) - as.numeric(new$Goal_USD)

new <- new %>% select(Project_my_id.x, Project_description, Category, Goal_USD, Pledge_USD)
new_v2 <- new %>% distinct()

new_v2$Goal_USD <- as.numeric(new_v2$Goal_USD)
new_v2$Goal_USD <- round(new_v2$Goal_USD, 0)
new_v2$Pledge_USD <- as.numeric(new_v2$Pledge_USD)
new_v2$Pledge_USD <- round(new_v2$Pledge_USD, 0)
new_v2$goal_reached <- ifelse(new_v2$Pledge_USD > new_v2$Goal_USD, "Goal_Reached", "Goal_Not_Reached")
new_v2$goal_reached <- new_v2$Pledge_USD - new_v2$Goal_USD
new_v2$lenDescription <- str_count(new_v2$Project_description, '\\w+')

new_v2$Project_description <- tolower(new_v2$Project_description)
Category_filter_digital <- c("Digital Art", "Webcomics", "Action", "Animation", "Comedy", "Documentary", "Drama", "Experimental", "Family", "Fantasy", "Fiction")
Category_filter_physical <- c("Accessories", "Apparel", "Childrenswear", "Couture", "Footwear", "Jewelry", "Pet Fashion", "Ready-to-wear","Toys", "Illustration", "Installation", "Painting", "Sculpture", "Textiles", "Anthologies", "Comic Books", "Graphic Novels", "Candles", "Crochet", "DIY", "Embroidery", "Glass", "Knitting", "Pottery", "Printing", "Quilts", "Stationery", "Taxidermy", "Weaving", "Woodworking" )
Category_filter_other <- c("Social Practice", "Events", "Performances", "Residencies", "Spaces", "Workshops", "Festivals" )

new_v2$TypeOfProject <- ifelse(new_v2$Category %in% Category_filter_digital, "Digital", 
                               ifelse(new_v2$Category %in% Category_filter_physical, "Physical", 
                                      ifelse(new_v2$Category %in% Category_filter_other, "Other", 0)))

Dictionary_Physical <- c("boardgame", "shipping", "comic", "painting", "novel", "machine")
Dictionary_Digital <- c("videogame", "movie", "film", "music")
Dictionary_Other <- c("dance", "exhibition", "event", "conference")

pattern_Physical <- paste(Dictionary_Physical, collapse = "|")
pattern_Digital <- paste(Dictionary_Digital, collapse = "|")
pattern_Other <- paste(Dictionary_Other, collapse = "|")

new_v2$CountOfPhysical <- str_count(new_v2$Project_description, regex(pattern_Physical))
new_v2$CountOfDigital <- str_count(new_v2$Project_description, regex(pattern_Digital))
new_v2$CountOfOther <- str_count(new_v2$Project_description, regex(pattern_Other))

new_v2[new_v2$TypeOfProject == "0",]$TypeOfProject <- ifelse((new_v2$CountOfPhysical > new_v2$CountOfDigital) & (new_v2$CountOfPhysical > new_v2$CountOfOther) , "Physical", 
                               ifelse((new_v2$CountOfDigital > new_v2$CountOfPhysical) & (new_v2$CountOfDigital > new_v2$CountOfOther), "Digital", "Other"))[as.numeric(rownames(new_v2[new_v2$TypeOfProject == "0",]))]

ggplot(new_v2, aes(x=reorder(TypeOfProject, TypeOfProject, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Type of Project') +
  ggtitle("Distribution of project types")

ggplot(new_v2, aes(x=reorder(Category, Category, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Category') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ggtitle("Distribution of categories")

ggplot(new_v2, aes(x=Goal_USD)) + geom_histogram() +
  ggtitle("Distribution of GoalUSD")

boxplot(new_v2$goal_reached)$out
min(new_v2$goal_reached)
length(unique(new_v2$Category))

new_v2 <- new_v2[new_v2$lenDescription > 5,]
new_v2 <- new_v2[as.numeric(ave(new_v2$Category, new_v2$Category, FUN=length)) >= 100, ]
new_v2 <- new_v2[new_v2$goal_reached > quantile(new_v2$goal_reached, 0.05) & new_v2$goal_reached < quantile(new_v2$goal_reached, 0.95),]

ggplot(new_v2, aes(x=reorder(TypeOfProject, TypeOfProject, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Type of Project') +
  ggtitle("Distribution of project types")

ggplot(new_v2, aes(x=reorder(Category, Category, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Category') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ggtitle("Distribution of categories")

ggplot(new_v2, aes(x=goal_reached)) + geom_histogram() +
  ggtitle("Distribution of Goal Reached")

new_v2$factor_ToP <- as.factor(new_v2$TypeOfProject)
new_v2$factor_Cat <- as.factor(new_v2$Category)
new_v2$factor_ToP = relevel(new_v2$factor_ToP, ref="Other")

model1 <- lm(goal_reached ~ factor_ToP, data = new_v2)
summary(model1)

model2 <- lm(goal_reached ~ factor_ToP + lenDescription + Goal_USD, data = new_v2)
summary(model2)
interact_plot(model2, pred = Goal_USD, modx = factor_ToP)
interact_plot(model2, pred = lenDescription, modx = factor_ToP)

model3 <- lm(goal_reached ~ factor_ToP + lenDescription + Goal_USD + factor_Cat, data = new_v2)
summary(model3)

tab <- matrix(c(AIC(model1), BIC(model1), AIC(model2), BIC(model2), AIC(model2), BIC(model2)), ncol=2, byrow=TRUE)
colnames(tab) <- c('AIC','BIC')
rownames(tab) <- c('Model 1','Model 2','Model 3')
tab <- as.table(tab)
tab

# sample <- sample(nrow(new),1000)
# new_sample <- new[sample,]
# new_sample$goal_reached_v2 <- new_sample$Pledge_USD - new_sample$Goal_USD 
# model3_sample <- lm(goal_reached ~ TypeOfProject + CountOfDigital + CountOfPhysical + CountOfOther, data = new_sample)
# summary(model3_sample)
# model4_sample <- lm(goal_reached_v2 ~ TypeOfProject + CountOfDigital + CountOfPhysical + CountOfOther, data = new_sample)
# summary(model4_sample)
# # Model 4 more easily readable due to positive impact means positive goal exeedance
# # Make model 4 for full data set:
# 
# full_data_for_mod4 <- new
# full_data_for_mod4$goal_reached_v2 <- new$Pledge_USD - new$Goal_USD
# model4_full_data <- lm(goal_reached_v2 ~ TypeOfProject + CountOfDigital + CountOfPhysical + CountOfOther, data = full_data_for_mod4)
# summary(model4_full_data)
# # Anova to compare two models in one plot/vis but can't do that with two different variables.
# # anova(model3_sample, model4_sample)
# #
# 
# table(new$Category)
# length(unique(new[["Category"]]))
# 
# # 
# # removed_outlier <- new[new$goal_reached < 10000 & new$goal_reached > -10000,]
# # 
# # ggplot(removed_outlier, aes(x=TypeOfProject, y= goal_reached)) +
# #  geom_jitter()
