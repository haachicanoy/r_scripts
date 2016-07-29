# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Sankey diagram
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

setwd(dir)

# load links data
links <- read.csv('Links_Values.csv')

# load nodes names
nodes <- read.csv('Nodes_Names.csv')

# create a list object
Test <- list(nodes=data.frame(name=nodes$Name),
             links=links)

sink('test_sankey.json') # redirect console output to a file
toJSON(Test, pretty=FALSE)
sink()

library(networkD3)
sankeyNetwork(Links=Test$links, Nodes=Test$nodes, Source="source",
              Target="target", Value="value", NodeID="name",
              units="Counts", fontSize=12, nodeWidth=30)
