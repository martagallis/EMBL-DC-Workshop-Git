# We will be talking about data.frames

# Let's import some data
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

library(tidyverse)

surveys <-  read_csv("data_raw/portal_data_joined.csv")

head(surveys)

str(surveys)

dim(surveys)

nrow(surveys)
ncol(surveys)

tail(surveys)

names(surveys)
# equivalent to
colnames(surveys)
rownames(surveys)

summary(surveys)

# Indexing and subsetting
surveys[1, 6]

surveys[1,]
surveys[,1]

surveys[c(1,2,3),c(5,6)]
surveys[c(1,2,3),]

surveys[1:3, 5:6]

# All columns without the first one
surveys[,-1]

surveys[,"sex"]
surveys["sex"]
surveys$plot_id #returned as vector
surveys["plot_id"]

surveys_200 <- surveys[200,]
nrow(surveys_200)

nrow(surveys)
surveys[nrow(surveys),]

nrow(surveys)/2

surveys[nrow(surveys)/2,]

my_list <- list(names = c("Nora", "Lisanna", "Francesco"),
                money = c(1, 6, 7, 3, 5, 8))
my_list[[1]]
my_list$names

surveys[[3]]

#Factors
str(surveys)
surveys$sex <- factor(surveys$sex)

levels(surveys$sex)
nlevels(surveys$sex)

sex <-  factor(c("male", "female", "female", "male"))
sex <- factor(sex,levels = c("male", "female"))

#Challenge
surveys$taxa <- factor(surveys$taxa)
surveys$genus <- factor(surveys$genus)

nlevels(surveys$genus)
levels(surveys$taxa)

sum(surveys$taxa == "Rabbit")
summary(surveys$taxa)

# Convert factors
as.character(sex)
sex

year_fct <- factor(c(1990,1983, 1977, 1997))
as.numeric(year_fct)

as.numeric(as.character(year_fct))
as.numeric(levels(year_fct))[year_fct]

# Renaming factors
plot(surveys$sex)
summary(surveys$sex) #NA not showing in plot because levels are only F and M
sex <- surveys$sex
levels(sex)
sex <- addNA(sex)
levels(sex)

levels(sex)[3] <- "undetermined"
levels(sex)
sex

plot(sex)

#Why factor and not vector on plotting (It's using default instructions specific to R)
#plot(c("a", "b", "a", "a", "a"))
#plot(factor(c("a", "b", "a", "a", "a")))

levels(sex)[1:2] <- c("female", "male")
plot(sex)
sex <- factor(sex, levels = c("undetermined", "female", "male"))
plot(sex)
