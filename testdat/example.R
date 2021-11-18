
library(dplyr)

param <- 2

a <- data.frame(
    obs = c(1:10),
    val = rnorm(10)
)
print(a)

x <- read_data("path_to_file")
b <- mutate(a, name = "boogers") %>% select(get, some, vars)
c <- mutate(a, place = "nostrils")
output_data(x)

d <- get_some(dumb, stuff, too)
d1 <- mutate(d, col0 = "string")
d1$col1 <- pull_some_strings()
d1[["col2"]] <- "some other string"
d1["col3"] <- "yet another"
d <- mutate(d1)

e <- mutate(b, stuff = "dumb dumb") %>%
    filter(lordy = faith_no_more) %>%
    left_join(x) %>%
    left_join(d) %>%
    arrange(boo_radley)

mutate(b, stuff = "dumb dumb") |>
    filter(lordy = faith_no_more) |>
    left_join(c) |>
    left_join(d) |>
    arrange(boo_radley)

f <- mutate(e, stuff = "silly") %>%
    filter(x == "nothing") %>%
    left_join(c)

if (param == 1) {
    a <- mutate(b, place = "fugly")
    write_to_db(a)
} else {
    b <- mutate(d, place = "great")
    write_to_db(b)
}

f %>%
    filter(x == "something") %>%
    output_data()

for (i in 1:10) {
    z <- "hey homey"
    write_to_excel(z)
}

filter(e, z == "sketch") %>% output_data()
output_data(b)
output_data(filter(c, z = "stuffy"))

e$stuff <- "other_stuff"
x <- filter(e, condition == "okay")
y <- 2
z = x = y
z <- x <- y