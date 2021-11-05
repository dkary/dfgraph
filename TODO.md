
# TODO 

## Data Prep

- [x] catch assignment like df$col
- [x] correct ordering with conditionals
    + whereas now all assignment come before all effects
- [ ] target and dependency numbering, so we can:
    + have target (objects) with the same name occur as separate nodes
    + have the correct node referenced in dependencies (i.e., the most recent with the matching name)
 
## DAG Representation

- [ ] `to_dotfile`: converting parsed/dependencies to a useful format (e.g., dotfile)

## Graphing
   
- [ ] probably use some existing graph library
    + take a look at `ggdag`
- [ ] try it on some real life code from SA Github
- [ ] (maybe) convert nesting to pipes for display (or use the srcref attribute from parse())

### Functions

- [ ] dataflow::view(script.R, type = "static"), which wraps several functions:
    + exprs <- parse(script.R) (and maybe not needed)
    + parsed <- get_parsed(exprs)
    + dependencies <- identify_dependencies(parsed)
    + dotfile <- to_dotfile(parsed, dependencies)
    + plot_static(dotfile)
    + plot_interactive(dotfile)
    
## Lower Priority

- [ ] consider capturing control flow and nesting in parsed output (which could seemingly be useful to visualize)
- [ ] Be able to view corresponding code on clicking a node
    + Interesting that the native pipe is automatically converted to nested calls in the expression. Makes sense, but won't be nice for viewing code if that feature is included
