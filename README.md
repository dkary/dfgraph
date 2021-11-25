
# dataflow

Visualize how data flows through an R script to illuminate unfamiliar workflows or guide your works-in-progress. 

Note: This package is in a **pre-alpha state** and not yet intended for serious use (although you're welcome to test/experiment). There will likely be major changes in the near future (e.g., to enable interactivity).

### Limitations

The most obvious limitation is that code is inherently flexible, and I won't be able to capture all the ways people might program. For example:

- We can misidentify dependencies due to name scoping (e.g., dataframe$column "d" vs. global variable "d") with non-standard evaluation (e.g., in `dplyr`).

However, I suspect that I can capture enough of the common data science coding patterns for the package to nonetheless be useful (more details in [Proof of Concept](ref/POC.md)).

## Installation

From the R console:

```r
install.packages("remotes")
remotes::install_github("dkary/dataflow")
```

## Usage

Run `dataflow::plot_flow("path_to_R_or_Rmd_file")` from the R console (which leverages the [DiagrammeR](https://github.com/rich-iannone/DiagrammeR) package with a   [DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) format under the hood).

```r
# Example
dataflow::plot_flow(
    "testdat/svy-weight.R",
    # exclude diagnostic checks from the plot
    prune_labels = c("count", "summary", "sapply", "glimpse", "all.equal")
)
```

![](ref/img/assemble.png)

### Customize

Home in on information of interest:

```r
dataflow::plot_flow(
    "testdat/svy-weight.R",
    prune_all_mutates = TRUE, # exclude nodes with only a self-dependency
    label_option = "auto", # display either assignment or function contextually
    prune_labels = c("count", "summary", "sapply", "glimpse", "all.equal")
)
```

![](ref/img/assemble-minimal.png)

### Focus on one Node

Focus on the network of a specified node (which you can reveal interactively by hovering over a node):

```r
dataflow::plot_flow(
    "testdat/svy-weight.R", focus_node = 20, 
    prune_all_mutates = TRUE, label_option = "auto"
)
```

![](ref/img/assemble-focus.png)

