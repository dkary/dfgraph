
library(dplyr)

param <- 2

a <- data.frame(
    obs = c(1:10),
    val = rnorm(10)
)
print(a)

b <- mutate(a, name = "boogers") %>% select(get, some, vars)
c <- mutate(b, place = "nostrils")
d <- get_some(a, b, c, dumb, stuff, too)

e <- mutate(b, stuff = "dumb dumb") %>%
    filter(lordy = faith_no_more) %>%
    left_join(c) %>%
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

f$stuff <- "stuff"
x <- 1
y <- 2
z <- x <- y