% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjmat.R
\name{make_adjmatrix}
\alias{make_adjmatrix}
\alias{make_adjmatrix_graph}
\title{Generate Adjacency Matrix}
\usage{
make_adjmatrix_graph(graph, directed = FALSE)
}
\arguments{
\item{graph}{An \code{\link[igraph:aaa-igraph-package]{igraph}} object. May be directed or weighted.}

\item{directed}{logical. Whether directed information is passed to the adjacency matrix.}
}
\value{
An adjacency matrix compatible with generating an expression matrix
}
\description{
Compute the adjacency matrix of a (directed) \code{\link[igraph:aaa-igraph-package]{igraph}}
structure, preserving node/column/row names (and direction).
}
\examples{

# construct a synthetic graph module
library("igraph")
graph_test_edges <- rbind(c("A", "B"), c("B", "C"), c("B", "D"))
graph_test <- graph.edgelist(graph_test_edges, directed = TRUE)

# compute adjacency matrix for toy example
adjacency_matrix <- make_adjmatrix_graph(graph_test)
adjacency_matrix

# construct a synthetic graph network
graph_structure_edges <- rbind(c("A", "C"), c("B", "C"), c("C", "D"), c("D", "E"),
                               c("D", "F"), c("F", "G"), c("F", "I"), c("H", "I"))
graph_structure <- graph.edgelist(graph_structure_edges, directed = TRUE)
# compute adjacency matrix for toy network
graph_structure_adjacency_matrix <- make_adjmatrix_graph(graph_structure)
graph_structure_adjacency_matrix

# import graph from package for reactome pathway
# TGF-\eqn{\Beta} receptor signaling activates SMADs (R-HSA-2173789)
TGFBeta_Smad_graph <- identity(TGFBeta_Smad_graph)

# compute adjacency matrix for TGF-\eqn{\Beta} receptor signaling activates SMADs
TGFBeta_Smad_adjacency_matrix <- make_adjmatrix_graph(TGFBeta_Smad_graph)
dim(TGFBeta_Smad_adjacency_matrix)
TGFBeta_Smad_adjacency_matrix[1:12, 1:12]

}
\seealso{
See also \code{\link[graphsim]{generate_expression}} for computing the simulated data,
\code{\link[graphsim]{make_sigma}} for computing the Sigma (\eqn{\Sigma}) matrix,
\code{\link[graphsim]{make_distance}} for computing distance from a graph object,
\code{\link[graphsim]{make_state}} for resolving inhibiting states.

See also \code{\link[graphsim]{plot_directed}} for plotting graphs or 
\code{\link[gplots]{heatmap.2}} for plotting matrices.

See also \code{\link[graphsim]{make_laplacian}}
or  \code{\link[graphsim]{make_commonlink}} for computing input matrices.

See also \code{\link[igraph:aaa-igraph-package]{igraph}} for handling graph objects.

Other graphsim functions: 
\code{\link{generate_expression}()},
\code{\link{make_commonlink}},
\code{\link{make_distance}},
\code{\link{make_laplacian}},
\code{\link{make_sigma}},
\code{\link{make_state}},
\code{\link{plot_directed}()}

Other graph conversion functions: 
\code{\link{make_commonlink}},
\code{\link{make_laplacian}}
}
\author{
Tom Kelly \email{tom.kelly@riken.jp}
}
\concept{graph conversion functions}
\concept{graphsim functions}
\keyword{adjacency}
\keyword{graph}
\keyword{igraph}
\keyword{network}
