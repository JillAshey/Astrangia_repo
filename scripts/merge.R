color <- read.csv("../../../../ColorScore_Astrangia2021_Experimental.csv")
head(color)



color <- color %>% rename(colony_id = ID)
test <- merge(color, pr.out, by = "colony_id")
write.csv(test, "../../../../checks.csv")
