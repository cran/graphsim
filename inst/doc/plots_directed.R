## ---- message=F---------------------------------------------------------------
library("igraph")
library("graphsim")

## -----------------------------------------------------------------------------
graph_edges <- rbind(c("A", "C"), c("B", "C"), c("C", "D"), c("D", "E"), c("D", "F"), c("F", "G"), c("F", "I"), c("H", "I"))
graph <- graph.edgelist(graph_edges, directed = T)

## -----------------------------------------------------------------------------
plot(graph)
plot_directed(graph)

## -----------------------------------------------------------------------------
plot_directed(graph, layout = layout.kamada.kawai)
plot_directed(graph, fill.node = "lightblue", border.node = "royalblue")

## -----------------------------------------------------------------------------
names(V(graph))
colour_vector <- ifelse(names(V(graph)) %in% c("A", "D", "I"), 1, 2)
plot_directed(graph, fill.node = c("lightblue", "grey")[colour_vector], border.node = c("royalblue", "black")[colour_vector])

## -----------------------------------------------------------------------------
plot_directed(graph, state = "activating")
plot_directed(graph, state = "inhibiting")
plot_directed(graph, state = 1)
plot_directed(graph, state = -1)
plot_directed(graph, state = 0)
plot_directed(graph, state = 2)

## -----------------------------------------------------------------------------
E(graph)
plot_directed(graph, state = c(1, 1, -1, -1, 1, -1, 1, -1))

