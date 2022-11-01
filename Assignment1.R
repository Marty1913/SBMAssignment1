library(dplyr)
library(stringr)


filenames <- list.files("Kickstarter/2019_week19", pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)


new <- data.frame(ldf[1]) %>% inner_join(data.frame(ldf[2]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[3]), by=c("project_slug"))
new <- new %>% inner_join(data.frame(ldf[4]), by=c("project_slug"))
rm(ldf)

new <- new %>% select(Project_my_id.x, Project_description, Category, Goal_USD, Pledge_USD)
new <- new %>% distinct()

new$Goal_USD <- as.numeric(new$Goal_USD)
new$Goal_USD <- round(new$Goal_USD, 0)
new$Pledge_USD <- as.numeric(new$Pledge_USD)
new$Pledge_USD <- round(new$Pledge_USD, 0)
new$goal_reached <- new$Pledge_USD > new$Goal_USD


new$Project_description <- tolower(new$Project_description)


Dictionary_Physical <- c("boardgame", "shipping")
Dictionary_Digital <- c("videogame")
Dictionary_Other <- c("blue")

pattern_Physical <- paste(Dictionary_Physical, collapse = "|")
pattern_Digital <- paste(Dictionary_Digital, collapse = "|")


new$CountOfPhysical <- str_count(new$Project_description, regex(pattern_Physical))
new$CountOfDigital <- str_count(new$Project_description, regex(pattern_Digital))
  
new$TypeOfProject <- ifelse(new$CountOfPhysical > new$CountOfDigital, "Physical", "Digital")
