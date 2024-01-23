weight_g <- c(50,60,65,82)

animals <- c("mouse","rat","dog")

# get the length of the vector
length(animals)

# get the type of data contained in the vector
class(animals)
class(weight_g)

# structure of the object
str(animals)

# how to add an element to the beginning of a vector
animals <- c("cincilla",animals)
animals <- c(animals,"frog")

typeof(animals)
class(animals)

# What happens in the next cases?
num_char <- c(1,2,3,"a")
num_logical <-  c(1,2,3,TRUE)
char_logical <- c("a","b","c",TRUE)
tricky <- c(1,2,3,"4")
# logical -> numeric -> character
# logical -> character

# subsetting a vector
animals[2]
animals[c(1,2)]

more_animals <- animals[c(1,2,3,2,1,4)]

weight_g
weight_g[c(FALSE,FALSE,TRUE,TRUE)]
weight_g > 63
weight_g[weight_g > 63]
weight_g[weight_g > 63 & weight_g < 80]
weight_g[weight_g <58 | weight_g > 80]
# <, >, ==, !=, <=, >=, !

animals[animals == "rat" | animals == "frog"]
# %in% helps us find all elements corresponding to a vector of elements of our choice
animals %in% c("rat","frog","cat","duck","dog")
animals[animals %in% c("rat","frog","cat","duck","dog")]

#An example of a vector with missing data
heights <- c(2,4,4,NA,6)
mean(heights)
mean(heights, na.rm = TRUE)
max(heights, na.rm = TRUE)

# identify which are the missing data
is.na(heights)
heights[!is.na(heights)]
# omit the missing data
na.omit(heights)
# extract the complete cases
heights[complete.cases(heights)]

heights_in <-  c(63, 69, 60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63)
heights_no_na <- heights_in[!is.na(heights_in)]
median (heights_in, na.rm = TRUE)

heights_no_na[heights_no_na > 67]

length(heights_no_na[heights_no_na > 67])
