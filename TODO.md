
# TODO 

- [x] propagate function global dependencies
- [x] set up R package structure (and roxygen docs)
- [x] proof of concept
- [ ] interactivity and corresponding POC
- [ ] vignette and/or links to blog posts
- [ ] testing

## Bugs

- [x] Error in if (is.name(f_formals[[i]]) & f_formals[[i]] != "") { :  argument is of length zero
- [ ] `cannot open connection errors` are vague (i.e., when a source script can't be found)
    + will need to do a bit of a refresher on error handling in R
- [ ] `Error: syntax error in line 7 near 'answer'`: For the B4W > svy/3-flags.R. It occurs in a `tibble::tribble()` in lines 29 to 47

## Data Prep

- [ ] dealing with script options such as `knitr::opts_chunk$set()`. Probably want to exclude these
- [x] catch assignment like df$col
- [x] correct ordering with conditionals
    + whereas now all assignment come before all effects
- [x] work on incorporating with ggdag package
    + should edges look more like the output of ggdag::tidy_dagitty?
    + probably better: produce a dot langauge representation of nodes/edges, which you can then graph using ggdag (or similar)
- [x] collapse assignments that only depend on themselves to previous nodes
- [x] get sequential integers for repeated occurrences of assignment nodes
    + getting rid of the current node_id workaround
- [x] ignore function assignment
- [ ] maybe treat apply (and purrr functions) like for loops
- [x] catch assignment with "="
- [x] target and dependency numbering, so we can:
    + have target (objects) with the same name occur as separate nodes
    + have the correct node referenced in dependencies (i.e., the most recent with the matching name)

## Graphing
   
- [x] `edges_to_dot()`: converting parsed/dependencies to a useful format (e.g., dotfile)
- [x] aliases (the target name) instead of the current name+ID displayed
- [x] get a nice default appearance, potentially with a wrapper function
- [ ] exclude_nodes (vector of nodes ids) that will be treated like assigned functions (i.e., their dependencies will be propagated, but they won't be displayed)
    + use case: a parameter which is used in many nodes, clutters the display without being terribly informative
- [ ] (maybe) a display_assignment = TRUE option that will show assigned variable names (instead of function name) for non-input nodes
- [ ] exclude_text = TRUE option (removing corresponding code from the tooltip) to make for much more compact dot files
- [ ] (maybe) convert nesting to pipes for display (or use the srcref attribute from parse())
- [ ] (maybe) wrapper func to save to dotfile for editing and replotting
- [x] correctly work with escaped quotes in tooltips
- [ ] increase tooltip size (and maybe responsiveness)
- [x] better implementation of numbers stripped from node names for labelling (e.g., if we want to show attributes, we would want the exact name assigned, even if it has numbers)
- [ ] correctly represent `|>` in tooltips (instead of using the parsed representation which converts to nested functions)
- [x] (maybe) give effects (i.e., terminal nodes) their own color (green seems to make the most sense, so maybe yellow for intermediate)
- [ ] (maybe) prune_ids option to remove individual nodes based on their numeric IDs
- [ ] (maybe) better node collapsing
    + [ ] fix bug with ifelse in column assignment (from wmi code examples)
    + [ ] collapse to unique mutates rather than all the way to input. The basic issue it that parallell mutate tracks can get collapsed, but this is counterintuitive
    
## Interactivity

- [ ] Plot interactivity for collapsing/expanding nodes
    + watershed metaphor: every edge could have a +/-
    + a "-" would collapse everything above the edge to one node
    + a "+" would expand a collapsed node to some degree
- [ ] lines from the file would improve my ability to use the graph alongside the code
    + see hand notes
    
- [ ] (maybe) an alternative option (or default approach) to propagating function globals b/c it can introduce a spaghetti-looking effect when a function is run multiple times (e.g., when only one argument varies across runs). Maybe it would be more useful to show globally-assigned functions in their own track with their dependencies specified. Note that this situation happens with the nc lifetime analysis script:
    + and it might actually be more interesting to be able to examine what is going on within the big run_analysis() function than the script, so expanding a function node would be a useful feature

## Lower Priority

- [ ] consider capturing control flow and nesting in parsed output (which could seemingly be useful to visualize)
    + Interesting that the native pipe is automatically converted to nested calls in the expression. Makes sense, but won't be nice for viewing code if that feature is included
    