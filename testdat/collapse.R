
library(dplyr)

param <- 2 # 1

a <- data.frame(obs, val) # 2
print(a)

x <- read_data("path_to_file") # 3
b <- mutate(a, name = "boogers") # 4 (a)
output_data(x) # 5 (x)

d <- get_some(dumb, stuff, too) # 6
d[["col"]] <- "sunset" # 7 (d)
d1 <- mutate(d, col0 = "string") # 8 (d)
d1$col1 <- pull_some_strings() # 9 (d1)
interrupter <- "sillyhat" # 10
d1[["col2"]] <- "other string" # 11 (d1)
d1["col3"] <- "yet another" # 12 (d1)
d <- mutate(x) # 13 (x)
output_data(d1) # 14 (d1)

d <- "new definition" # 15
d <- mutate(d) # 16 (d)
e <- mutate(d) # 17 (d)
f <- join(join(b, x), e) # 18 (b,x,e)
g <- mutate(f) # 19 (f)
h <- mutate(d1) # 20 (d1)
output_data(g) # 21 (g)