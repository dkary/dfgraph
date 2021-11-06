# make tasty food with R code

# 1. Start the Rice
water <- get("water", "faucet")
rice <- get("rice", "pantry")
rice_cooker <- get("rice_cooker", "cupboard")
cooked_rice <- rice_cooker |>
    add(rice) |>
    add(water) |>
    cook(min = 20)

# 2. Start the Tofu
tofu <- get("tofu", "fridge")
wok <- get("wok", "cupboard")
oil <- get("oil", "cupboard")
wok <- wok |>
    add(oil) |>
    heat(temp = "hi", min = 3)
tofu <- cube(tofu)
wok <- wok |>
    add(tofu) |>
    fry(temp = "med-hi", min = 5)

# 3. Add the Veggies
peppers <- get("peppers", "fridge")
onions <- get("onions", "pantry")
mixing_bowl <- get("mixing_bowl", "cupboard")
veggies <- cube(peppers, onions)
veggies <- mixing_bowl |>
    add(veggies) |>
    add(oil) |>
    mix()
stirfry <- wok |>
    add(veggies) |>
    fry(temp = "med", min = 5) |>
    fry(temp = "low", min = 10)

# 4. Serve!
bowls <- get("bowls", "cupboard")
bowls |>
    add(cooked_rice) |>
    add(stirfry) |>
    serve()
