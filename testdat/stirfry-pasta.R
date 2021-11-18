# make tasty food with R code
# spaghetti-code version

locations <- c(
    "cupboard",
    "pantry",
    "faucet",
    "fridge"
) 

search_device <- "flashlight"

get <- function(item, location, person, device) { 
    path <- find_shortest_path(location, person)
    retrieve(item, path, person, device)
}
call_upstairs <- function() {
    
}
# function with global var dependencies
get_from_cupboard <- function(item, name = "Dan", where = house) {
    # this red herring would break the dependency with the global "locations"
    # locations <- "red herring"
    call_person <- function(person) {
        call_upstairs(person)
    }
    find_location <- function() {
        x <- locations["cupboard"]
        x
    }
    call_name <- call_person(name)
    place <- find_location
    device <- search_device
    get(item, place, call_name, device)
}

add <- function(vessel, item) { }
cook <- function(vessel, min) { }
heat <- function(vessel, temp, min) { }
fry <- function(vessel, temp, min) { }
chop <- function(...) { }
mix <- function(vessel) { }
serve <- function(vessel) { }

# 1. Start the Rice
water <- get("water", locations["faucet"])
rice <- get("rice", locations["pantry"])
rice_cooker <- get_from_cupboard("rice_cooker")
cooked_rice <- rice_cooker |>
    add(rice) |>
    add(water) |>
    cook(min = 20)

# 2. Start the Tofu
tofu <- get("tofu", locations["fridge"])
wok <- get("wok", locations["cupboard"])
oil <- get_from_cupboard("oil")
heated_wok <- wok |>
    add(oil) |>
    heat(temp = "hi", min = 3)
chopped_tofu <- chop(tofu)
seared_tofu <- heated_wok |>
    add(chopped_tofu) |>
    fry(temp = "med-hi", min = 5)

# 3. Add the Veggies
peppers <- get("peppers", locations["fridge"])
onions <- get("onions", locations["pantry"])
spices <- get("spices", locations["pantry"])
mixing_bowl <- get_from_cupboard("mixing_bowl")
veggies <- chop(peppers, onions)
mixed_veggies <- mixing_bowl |>
    add(veggies) |>
    add(oil) |>
    add(spices) |>
    mix()
stirfry <- seared_tofu |>
    add(mixed_veggies) |>
    fry(temp = "med", min = 5) |>
    fry(temp = "low", min = 10)

# 4. Serve!
bowls <- get_from_cupboard("bowls")
chopsticks <- get("chopsticks", locations["cupboard"])
bowls |>
    add(cooked_rice) |>
    add(stirfry) |>
    add(chopsticks) |>
    serve()
