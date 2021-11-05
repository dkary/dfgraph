
# TODO 

## Dependencies

- probably no need for these:
    + dplyr
    + tibble
- these are probably worthwhile
    + rlang
    + ggdag
    + dagitty (seems like Rcpp is also needed for this one to function)

## Limitations

- We can misidentify dependencies due to name scoping (e.g., column "d" vs. dataframe "d"). Is this solvable? (seems unlikely)

## Data Prep

- [x] catch assignment like df$col
- [x] correct ordering with conditionals
    + whereas now all assignment come before all effects
- [x] work on incorporating with ggdag package
    + should edges look more like the output of ggdag::tidy_dagitty?
    + probably better: produce a dot langauge representation of nodes/edges, which you can then graph using ggdag (or similar)
- [ ] collapse assignments that only depend on themselves to previous nodes
- [ ] get sequential integers for repeated occurrences of assignment nodes
    + getting rid of the current node_id workaround
- [ ] ignore function assignment
- [ ] maybe treat apply (and purrr functions) like for loops
- [ ] catch assignment with "="
- [x] target and dependency numbering, so we can:
    + have target (objects) with the same name occur as separate nodes
    + have the correct node referenced in dependencies (i.e., the most recent with the matching name)

## Graphing
   
- [x] `edges_to_dot()`: converting parsed/dependencies to a useful format (e.g., dotfile)
- [ ] get a nice default appearance, potentially with a wrapper function
- [ ] (if time) incorporate interactivity in output
- [ ] try it on some real life code from SA Github
- [ ] (maybe) convert nesting to pipes for display (or use the srcref attribute from parse())
- [ ] (maybe) wrapper func to save to dotfile for editing and replotting

## Lower Priority

- [ ] consider capturing control flow and nesting in parsed output (which could seemingly be useful to visualize)
- [ ] Be able to view corresponding code on clicking a node
    + Interesting that the native pipe is automatically converted to nested calls in the expression. Makes sense, but won't be nice for viewing code if that feature is included
