library(dplyr)
library(stringr)
library(ggplot2)
library(rpart.plot)

filenames <- list.files("C:/Users/Faysal el Kaaoichi/Downloads/Kickstarter/2019_week19", pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)


new <- data.frame(ldf[1]) %>% inner_join(data.frame(ldf[2]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[3]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[4]), by=c("project_slug"))
rm(ldf)

class(new$Goal_USD)
class(new$Pledge_USD)

new$Goal_USD
new$Pledge_USD

diff_amt <- as.numeric(new$Goal_USD) - as.numeric(new$Pledge_USD)
diff_amt





















new <- new %>% select(Project_my_id.x, Project_description, Category, Goal_USD, Pledge_USD)
new <- new %>% distinct()

new$Goal_USD <- as.numeric(new$Goal_USD)
new$Goal_USD <- round(new$Goal_USD, 0)
new$Pledge_USD <- as.numeric(new$Pledge_USD)
new$Pledge_USD <- round(new$Pledge_USD, 0)
new$goal_reached <- ifelse(new$Pledge_USD > new$Goal_USD, "Goal_Reached", "Goal_Not_Reached")
new$goal_reached <- new$Goal_USD - new$Pledge_USD


new$Project_description <- tolower(new$Project_description)
Category_filter_digital <- c("Digital Art", "Webcomics", "Action", "Animation", "Comedy", "Documentary", "Drama", "Experimental", "Family", "Fantasy")
Category_filter_physical <- c("Accessories", "Apparel", "Childrenswear", "Couture", "Footwear", "Jewelry", "Pet Fashion", "Ready-to-wear","Toys", "Illustration", "Installation", "Painting", "Sculpture", "Textiles", "Anthologies", "Comic Books", "Graphic Novels", "Candles", "Crochet", "DIY", "Embroidery", "Glass", "Knitting", "Pottery", "Printing", "Quilts", "Stationery", "Taxidermy", "Weaving", "Woodworking" )
Category_filter_other <- c("Social Practice", "Events", "Performances", "Residencies", "Spaces", "Workshops", "Festivals" )

Dictionary_Physical <- c("boardgame", "shipping", "comic", "painting", "novel", "machine")
Dictionary_Digital <- c("videogame", "movie", "film", "music")
Dictionary_Other <- c("dance", "exhibition", "event")

pattern_Physical <- paste(Dictionary_Physical, collapse = "|")
pattern_Digital <- paste(Dictionary_Digital, collapse = "|")
pattern_Other <- paste(Dictionary_Other, collapse = "|")


new$CountOfPhysical <- str_count(new$Project_description, regex(pattern_Physical))
new$CountOfDigital <- str_count(new$Project_description, regex(pattern_Digital))
new$CountOfOther <- str_count(new$Project_description, regex(pattern_Other))

new$TypeOfProject <- ifelse((new$CountOfPhysical > new$CountOfDigital) & (new$CountOfPhysical > new$CountOfOther) , "Physical", 
                            ifelse((new$CountOfDigital > new$CountOfPhysical) & (new$CountOfDigital > new$CountOfOther), "Digital", "Other"))

table <- table(new$TypeOfProject, new$goal_reached)

chi2 = chisq.test(table, correct=F)

chi2
c(chi2$statistic, chi2$p.value)
# sqrt(chi2$statistic / sum(table))

train <- new[ind == 1,]
test <- new[ind == 2,]

#tree <- rpart::rpart(goal_reached ~ TypeOfProject + Goal_USD + Category, data = train, cp = 0.01)
#rpart.plot(tree)

class(new$TypeOfProject)

ToP <- as.factor(new$TypeOfProject)
ToP <- relevel(ToP, ref = 2)
levels(ToP)
levels(new$TypeOfProject)

colnames(new)

new$Category

model <- lm(new$goal_reached ~ ToP + Category, data = new)
summary(model)
# 
# removed_outlier <- new[new$goal_reached < 10000 & new$goal_reached > -10000,]
# 
# ggplot(removed_outlier, aes(x=TypeOfProject, y= goal_reached)) +
#  geom_jitter()
