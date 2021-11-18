
library(dplyr)

param <- 2 # 1

a <- data.frame(obs, val) # 2
print(a)

x <- read_data("path_to_file") # 3
b <- mutate(a, name = "boogers") # 4 <- 2 
output_data(x) # 5 (x)

d <- get_some(dumb, stuff, too) # 6
d[["col"]] <- "sunset" # 7 <- 6
d1 <- mutate(d, col0 = "string") # 8 <- 7
d1$col1 <- pull_some_strings() # 9 <- 8
interrupter <- "sillyhat" # 10
d1[["col2"]] <- "other string" # 11 <- 9
d1["col3"] <- "yet another" # 12 <- 11
d <- mutate(x) # 13 <- 3
output_data(d1) # 14 <- 12

d <- "new definition" # 15
d <- mutate(d) # 16 <- 15
e <- mutate(d) # 17 <- 16
f <- join(join(b, x), e) # 18 <- 4, 3, 17
g <- mutate(f) # 19 <- 18
h <- mutate(d1) # 20 <- 12
output_data(g) # 21 <- 19