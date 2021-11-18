
# Research

Taking a look at what's out there, whether this tool would be useful, and how I might implement it.

## Useful Coding Info

- https://renkun.me/2020/11/08/using-parse-data-to-analyze-r-code/
- Advanced R chapter on Expressions

## Interactivity

Looking at Graphviz interactivity options:

- https://stackoverflow.com/questions/14838820/interactive-graphviz-viewer-with-basic-node-edge-filtering
    + https://gephi.org

## Does it exist? 

- Find out what exists out there.
    + This AWS `GetDataflowGraph` seems to be exactly what I'm trying to do (but just with python, and part of AWS I guess): https://docs.aws.amazon.com/glue/latest/webapi/API_GetDataflowGraph.html
    
### Is there interest?

People have asked about this online:

- https://stackoverflow.com/questions/61827351/static-dataflow-graph-generator-for-python
- https://stackoverflow.com/questions/14166151/find-dependencies-in-a-python-source-script
    + This one has some example code that uses the `tokenize` module
    
### IBM Tool

IBM may have done this, but it's not really clear how it works:

- https://github.com/ibm/rflowgraph
- https://github.com/ibm/pyflowgraph

A paper on the topic (Semantic flow graph): https://www.epatters.org/assets/papers/2018-semantic-enrichment.pdf