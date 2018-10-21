##############################################################
# R Package Social Network Graph Visualization
##############################################################

# http://www.r-bloggers.com/visualizing-the-r-packages-galaxy/

library(igraph)
dat <- available.packages()

# For each package, we produce a character string with all its dependencies (Imports and Depends fields) separated with commas.

dat.imports <- paste(dat[, "Imports"], dat[, "Depends"], sep = ", ")
dat.imports <- as.list(dat.imports)
dat.imports <- lapply(dat.imports, function(x) gsub("\\(([^\\)]+)\\)", "", x))
dat.imports <- lapply(dat.imports, function(x) gsub("n", "", x))
dat.imports <- lapply(dat.imports, function(x) gsub(" ", "", x))


# Next step, we split the strings and we use the stack function to get the complete list of edges of the graph.

dat.imports <- sapply(dat.imports, function(x) strsplit(x, split = ","))
dat.imports <- lapply(dat.imports, function(x) x[!x %in% c("NA", "R")])
names(dat.imports) <- rownames(dat)
dat.imports <- stack(dat.imports)
dat.imports <- as.matrix(dat.imports)
dat.imports <- dat.imports[-which(dat.imports[, 1]==""), ]

# Finally we create the graph with the list of edges. Here, I select the largest connected component because there are many isolated vertices which will make the graph harder to represent.

g <- graph.edgelist(dat.imports)
g <- decompose.graph(g)
g <- g[[which(sapply(g, vcount) == max(sapply(g, vcount)))]]

# plot the igraph
# plot(g)

##############################################################
# Convert igraph image plot to Gephi
##############################################################

# install.packages("rgexf")
library(rgexf)

# construct the nodes and edges data for gexf conversion
nodes <- data.frame(cbind(V(g), as.character(V(g))))
edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))

write.csv(nodes, "C:/Users/derek/Desktop/nodes.csv")
write.csv(edges, "C:/Users/derek/Desktop/edges.csv")

# do the conversion
df <- write.gexf(nodes, edges)  


# Exporting to some place
print(gexf2, output="C://Users//derek//Documents//mygraph.gexf", replace=TRUE)


gexf2 <- igraph.to.gexf(g)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Converts the given igraph object to GEXF format and saves it at the given filepath location
#     g: input igraph object to be converted to gexf format
#     filepath: file location where the output gexf file should be saved
#


saveAsGEXF = function(g, filepath="converted_graph.gexf")
  
{
  
  require(igraph)
  
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
 
  # check if the input vertices has label already present
  
  # if not, just have the ids themselves as the label
  
  if(is.null(V(g)$label))
    
    V(g)$label <- as.character(V(g)$name)
  
  # similarily if edges does not have weight, add default 1 weight
  
  if(is.null(E(g)$weight))
    
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  
  vAttrNames <- setdiff(list.vertex.attributes(g), "label")
  
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))),
                         
                         stringsAsFactors = FALSE)
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  
  eAttrNames <- setdiff(list.edge.attributes(g), "weight")
  
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))),
                         
                         stringsAsFactors = FALSE)
  
  # generate the gexf object
  
  output <- write.gexf(nodes, edges,
                       
                       edgesWeight=E(g)$weight,
                       
                       edgesAtt = edgesAtt,
                       
                       nodesAtt = nodesAtt)
  
  print(output, filepath, replace=T)
  
}


saveAsGEXF(g, "C:/Users/derek/Documents/")