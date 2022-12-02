library(dplyr)
library(stringr)
library(ggplot2)
library(rpart.plot)

# Opening and loading in data.
filenames <- list.files("Kickstarter/2019_week19", pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
new <- data.frame(ldf[1]) %>% inner_join(data.frame(ldf[2]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[3]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[4]), by=c("project_slug"))
rm(ldf)

diff_amt <- as.numeric(new$Pledge_USD) - as.numeric(new$Goal_USD)
diff_amt


new <- new %>% select(Project_my_id.x, Project_description, Category, Goal_USD, Pledge_USD)
new_v2 <- new %>% distinct()

new_v2$Goal_USD <- as.numeric(new_v2$Goal_USD)
new_v2$Goal_USD <- round(new_v2$Goal_USD, 0)
new_v2$Pledge_USD <- as.numeric(new_v2$Pledge_USD)
new_v2$Pledge_USD <- round(new_v2$Pledge_USD, 0)
new_v2$goal_reached <- ifelse(new_v2$Pledge_USD > new_v2$Goal_USD, "Goal_Reached", "Goal_Not_Reached")
new_v2$goal_reached <- new_v2$Pledge_USD - new_v2$Goal_USD


new_v2$Project_description <- tolower(new_v2$Project_description)
Category_filter_digital <- c("Digital Art", "Webcomics", "Action", "Animation", "Comedy", "Documentary", "Drama", "Experimental", "Family", "Fantasy")
Category_filter_physical <- c("Accessories", "Apparel", "Childrenswear", "Couture", "Footwear", "Jewelry", "Pet Fashion", "Ready-to-wear","Toys", "Illustration", "Installation", "Painting", "Sculpture", "Textiles", "Anthologies", "Comic Books", "Graphic Novels", "Candles", "Crochet", "DIY", "Embroidery", "Glass", "Knitting", "Pottery", "Printing", "Quilts", "Stationery", "Taxidermy", "Weaving", "Woodworking" )
Category_filter_other <- c("Social Practice", "Events", "Performances", "Residencies", "Spaces", "Workshops", "Festivals" )

Dictionary_Physical <- c("boardgame", "shipping", "comic", "painting", "novel", "machine")
Dictionary_Digital <- c("videogame", "movie", "film", "music")
Dictionary_Other <- c("dance", "exhibition", "event")

pattern_Physical <- paste(Dictionary_Physical, collapse = "|")
pattern_Digital <- paste(Dictionary_Digital, collapse = "|")
pattern_Other <- paste(Dictionary_Other, collapse = "|")


new_v2$CountOfPhysical <- str_count(new_v2$Project_description, regex(pattern_Physical))
new_v2$CountOfDigital <- str_count(new_v2$Project_description, regex(pattern_Digital))
new_v2$CountOfOther <- str_count(new_v2$Project_description, regex(pattern_Other))

new_v2$TypeOfProject <- ifelse((new_v2$CountOfPhysical > new_v2$CountOfDigital) & (new_v2$CountOfPhysical > new_v2$CountOfOther) , "Physical", 
                            ifelse((new_v2$CountOfDigital > new_v2$CountOfPhysical) & (new_v2$CountOfDigital > new_v2$CountOfOther), "Digital", "Other"))

table <- table(new_v2$TypeOfProject, new_v2$goal_reached)

chi2 = chisq.test(table, correct=F)

chi2
c(chi2$statistic, chi2$p.value)
# sqrt(chi2$statistic / sum(table))

#train <- new_v2[ind == 1,]
#test <- new_v2[ind == 2,]

#tree <- rpart::rpart(goal_reached ~ TypeOfProject + Goal_USD + Category, data = train, cp = 0.01)
#rpart.plot(tree)

class(new_v2$TypeOfProject)
ToP <- as.factor(new_v2$TypeOfProject)
ToP <- relevel(ToP, ref = 2)
levels(ToP)
levels(new_v2$TypeOfProject)

splitfactor(ToP)

colnames(new_v2)

model <- lm(new_v2$goal_reached ~ ToP, data = new_v2)
summary(model)

model2 <- lm(new_v2$goal_reached ~ ToP + CountOfDigital + CountOfPhysical + CountOfOther, data = new_v2)
summary(model2)

sample <- sample(nrow(new),1000)
new_sample <- new[sample,]
new_sample$goal_reached_v2 <- new_sample$Pledge_USD - new_sample$Goal_USD 
model3_sample <- lm(goal_reached ~ TypeOfProject + CountOfDigital + CountOfPhysical + CountOfOther, data = new_sample)
summary(model3_sample)
model4_sample <- lm(goal_reached_v2 ~ TypeOfProject + CountOfDigital + CountOfPhysical + CountOfOther, data = new_sample)
summary(model4_sample)
# Model 4 more easily readable due to positive impact means positive goal exeedance
# Make model 4 for full data set:

full_data_for_mod4 <- new
full_data_for_mod4$goal_reached_v2 <- new$Pledge_USD - new$Goal_USD
model4_full_data <- lm(goal_reached_v2 ~ TypeOfProject + CountOfDigital + CountOfPhysical + CountOfOther, data = full_data_for_mod4)
summary(model4_full_data)
# Anova to compare two models in one plot/vis but can't do that with two different variables.
# anova(model3_sample, model4_sample)
#

table(new$Category)
length(unique(new[["Category"]]))

# 
# removed_outlier <- new[new$goal_reached < 10000 & new$goal_reached > -10000,]
# 
# ggplot(removed_outlier, aes(x=TypeOfProject, y= goal_reached)) +
#  geom_jitter()
