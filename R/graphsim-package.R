#' The graphsim package
#' 
#' graphsim is a package to simulate normalised expression data from networks 
#' for biological pathways using \sQuote{\code{igraph}} objects and multivariate
#' normal distributions. 
#' 
#' @rdname graphsim-package
#' @name graphsim-package
#' @aliases graphsim-package graphsim
#' @docType package
#' 
#' @author \bold{Maintainer}:  Tom Kelly  \email{tom.kelly@@riken.jp}
#' 
#' Authors:
#' \itemize{
#' \item Tom Kelly (RIKEN IMS) \href{https://orcid.org/0000-0003-3904-6690}{ORCID})
#' \item Mik Black (Otago University) (\href{https://orcid.org/0000-0003-1174-6054}{ORCID})
#' }
#' 
#' Reviewers:
#' \itemize{
#' \item Cory Brunson (UConn) (\href{https://orcid.org/0000-0003-3126-9494}{ORCID})
#' \item Robrecht Cannoodt (Ghent University) (\href{https://orcid.org/0000-0003-3641-729X}{ORCID})
#' }
#' 
#' Editor: Mark Jensen (Frederick National Laboratory for Cancer Research)
#' 
#' @details This package provides functions to develop simulated continuous data 
#' (e.g., gene expression) from a Sigma (\eqn{\Sigma}) covariance matrix derived from a 
#' graph structure in \sQuote{\code{igraph}} objects. Intended to extend 
#' \sQuote{\code{mvtnorm}} to take 'igraph' structures rather than sigma 
#' matrices as input. This allows the use of simulated data that correctly
#' accounts for pathway relationships and correlations. Here we present
#' a versatile statistical framework to simulate correlated gene expression
#' data from biological pathways, by sampling from a multivariate normal
#' distribution derived from a graph structure. This package allows the
#' simulation of biologicalpathways from a graph structure based on a
#' statistical model of gene expression, such as simulation of expression
#' profiles that of log-transformed and normalised data from microarray
#' and RNA-Seq data.
#' 
#' @section Introduction:
#' This package enables the generation of simulated gene expression datasets 
#' containing pathway relationships from a known underlying network.
#' These simulated datasets can be used to evaluate various bioinformatics 
#' methodologies, including statistical and network inference procedures.
#' 
#' These are computed by 1) resolving inhibitory states to derive a consistent
#' matrix of positive and negative edges, 2) inferring relationships between
#' nodes from paths in the graph, 3) weighting these in a Sigma (\eqn{\Sigma}) 
#' covariance matrix and 4) using this to sample a multivariate normal 
#' distribution.
#' 
#' @section Getting Started:
#' The \code{\link[graphsim]{generate_expression}} function is a wrapper 
#' around all necessary functions to give a final simulated dataset.
#' 
#' Here we set up an example graph object using the 
#' \code{\link[igraph:aaa-igraph-package]{igraph}} package.
#' 
#' \preformatted{
#' library("igraph")
#' graph_structure_edges <- rbind(c("A", "C"), c("B", "C"), c("C", "D"),c("D", "E"),
#'                                c("D", "F"), c("F", "G"), c("F", "I"), c("H", "I"))
#' graph_structure <- graph.edgelist(graph_structure_edges, directed = TRUE)
#' }
#' 
#' Then we can call \code{\link[graphsim]{generate_expression}} to return
#' the simulated data based on the relationships defined in the graph
#' structure. Various options are available to fine-tune this.
#' 
#' \preformatted{
#' expr <- generate_expression(100, graph_structure,
#'                             cor = 0.8,
#'                             mean = 0,
#'                             sd = 1,
#'                             comm = FALSE,
#'                             dist = TRUE,
#'                             absolute = FALSE,
#'                             laplacian = FALSE)
#' }
#' 
#' Here we can see the final result. The graph
#' structure defines the covariance matrix used
#' by \code{\link[mvtnorm:Mvnorm]{rmvnorm}} to
#' generate a multivariate distribution.
#' 
#' \preformatted{
#' dim(expr)
#' 
#' library("gplots")
#' heatmap.2(expr,
#'           scale = "none",
#'           trace = "none",
#'           col = bluered(50),
#'           colsep = 1:4,
#'           rowsep = 1:4)
#' }
#' 
#' This dataset consists of 9 rows (one for each vertex or gene)
#' in the graph and 100 columns (one for each sample or observation).
#' 
#' Input with an adjacency matrix is available using the
#' \code{\link[graphsim:generate_expression]{generate_expression_mat}}
#' function.
#' 
#' @section Creating Input Data:
#' Graph structures can be passed directly from the
#' \code{\link[igraph:aaa-igraph-package]{igraph}} package.
#' Using this package, you can create an \sQuote{\code{igraph}}
#' class object.
#' 
#' 
#' \preformatted{
#' > class(graph_structure)
#' [1] "igraph"
#' 
#' > graph_structure
#' IGRAPH ba7fa2f DN-- 9 8 -- 
#'   + attr: name (v/c)
#'   + edges from ba7fa2f (vertex names):
#'     [1] A->C B->C C->D D->E D->F F->G F->I H->I
#' }
#' 
#' This \sQuote{\code{igraph}} object class can be passed
#' directly to \code{\link[graphsim:generate_expression]{generate_expression}}
#' shown above and internal functions described below:
#' \code{\link[graphsim:make_sigma]{make_sigma_mat_graph}},
#' \code{\link[graphsim:make_sigma]{make_sigma_mat_dist_graph}},
#' \code{\link[graphsim:make_distance]{make_distance_graph}},
#' and
#' \code{\link[graphsim:make_state]{make_state_matrix}}.
#' 
#' The \sQuote{\code{graphsim}} package also supports various
#' matrix formats and has functions to handle these.
#' The following functions will compute matrices from an
#' \sQuote{\code{igraph}} object class:
#' 
#' \itemize{
#' 
#' \item  \code{\link[graphsim:make_adjmatrix]{make_adjmatrix_graph}}
#' to derive the adjacency matrix for a graph structure.
#' 
#' \item  \code{\link[graphsim:make_commonlink]{make_commonlink_graph}}
#' to derive the \sQuote{common link} matrix for a graph structure of
#' mutually shared neighbours.
#' 
#' \item  \code{\link[graphsim:make_laplacian]{make_laplacian_graph}}
#' to derive the Laplacian matrix for a graph structure.
#' 
#' } 
#' 
#' The following functions will compute matrices from an
#' \code{\link[graphsim:make_adjmatrix]{adjacency matrix}}:
#' 
#' \itemize{
#' 
#' \item  \code{\link[graphsim:make_commonlink]{make_commonlink_adjmat}}
#' to derive the \sQuote{common link} matrix for a graph structure of
#' mutually shared neighbours.
#' 
#' \item  \code{\link[graphsim:make_laplacian]{make_laplacian_adjmat}}
#' to derive the Laplacian matrix for a graph structure.
#' 
#' } 
#' 
#' We provide some pre-generate pathways from Reactoem database
#' for testing and demonstrations:
#' 
#' \itemize{
#' 
#' \item  \code{\link[graphsim:RAF_MAP_graph]{RAF_MAP_graph }}
#' for the interactions in the \dQuote{RAF/MAP kinase} cascade (17 vertices
#'  and 121 edges).
#' 
#' \item  \code{\link[graphsim:Pi3K_graph]{Pi3K_graph}}
#' for the phosphoinositide-3-kinase cascade (35 vertices and 251 edges).
#' 
#' \item  \code{\link[graphsim:Pi3K_AKT_graph]{Pi3K_AKT_graph}}
#' for the phosphoinositide-3-kinase activation of Protein kinase B
#' pathway \dQuote{PI3K/AKT activation} (275 vertices and 21106 edges).
#' 
#' 
#' \item  \code{\link[graphsim:TGFBeta_Smad_graph]{TGFBeta_Smad_graph}}
#' for the TGF-\eqn{\beta} receptor signaling activates SMADs
#' pathway (32 vertices and 173 edges).
#' } 
#' 
#' Please note that demonstrations on larger graph objects. These
#' can be called directly from the pakage:
#' 
#' \preformatted{
#' > graphsim::Pi3K_graph
#' IGRAPH 21437e3 DN-- 35 251 -- 
#'   + attr: name (v/c)
#'   + edges from 21437e3 (vertex names):
#'      [1] AKT1->AKT2  AKT1->AKT3  AKT1->CASP9 AKT1->CASP9
#'      [5] AKT1->CASP9 AKT1->FOXO1 AKT1->FOXO1 AKT1->FOXO1
#'      [9] AKT1->FOXO3 AKT1->FOXO3 AKT1->FOXO3 AKT1->FOXO4
#'      [13] AKT1->FOXO4 AKT1->FOXO4 AKT1->GSK3B AKT1->GSK3B
#'      [17] AKT1->GSK3B AKT1->NOS1  AKT1->NOS2  AKT1->NOS3 
#'      [21] AKT1->PDPK1 AKT2->AKT3  AKT2->CASP9 AKT2->CASP9
#'      [25] AKT2->CASP9 AKT2->FOXO1 AKT2->FOXO1 AKT2->FOXO1
#'      [29] AKT2->FOXO3 AKT2->FOXO3 AKT2->FOXO3 AKT2->FOXO4
#'      + ... omitted several edges
#'      + ... omitted several edges
#' }
#' 
#' They can also be imported into R:
#' 
#' \preformatted{
#' data(Pi3K_graph)
#' }
#' 
#' You can assign them to your local environment
#' by calling with from the package:
#' 
#' \preformatted{
#' graph_object <- identity(Pi3K_graph)
#' }
#' 
#' You can also change the object class directly
#' from the package:
#' 
#' \preformatted{
#' library("igraph")
#' Pi3K_adjmat <- as_adjacency_matrix(Pi3K_graph)
#' }
#' 
#'  \code{\link[graphsim:Pi3K_AKT_graph]{Pi3K_AKT_graph}} and 
#'  \code{\link[graphsim:TGFBeta_Smad_graph]{TGFBeta_Smad_graph}}
#'  contain graph edge attributes for the \sQuote{state} parameter
#'  described below.
#'  
#'  \preformatted{
#'  > TGFBeta_Smad_graph
#'  IGRAPH f3eac04 DN-- 32 173 -- 
#'    + attr: name (v/c), state (e/n)
#'    + edges from f3eac04 (vertex names):
#'      [1] BAMBI ->SMAD7  BAMBI ->TGFB1  BAMBI ->TGFBR1 BAMBI ->TGFBR2
#'      [5] CBL   ->NEDD8  CBL   ->NEDD8  CBL   ->TGFBR2 CBL   ->TGFBR2
#'      [9] CBL   ->UBE2M  CBL   ->UBE2M  FKBP1A->TGFB1  FKBP1A->TGFBR1
#'      [13] FKBP1A->TGFBR2 FURIN ->TGFB1  FURIN ->TGFB1  MTMR4 ->SMAD2 
#'      [17] MTMR4 ->SMAD2  MTMR4 ->SMAD3  MTMR4 ->SMAD3  NEDD4L->RPS27A
#'      [21] NEDD4L->SMAD7  NEDD4L->SMURF1 NEDD4L->SMURF2 NEDD4L->TGFB1 
#'      [25] NEDD4L->TGFBR1 NEDD4L->TGFBR2 NEDD4L->UBA52  NEDD4L->UBB   
#'      [29] NEDD4L->UBC    NEDD8 ->TGFBR2 NEDD8 ->UBE2M  PMEPA1->SMAD2 
#'      + ... omitted several edges
#'      
#'  > E(TGFBeta_Smad_graph)$state
#'  [1] 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#'  [32] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#'  [63] 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#'  [94] 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#'  [125] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#'  [156] 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 1 1 1
#'  
#'  > states <- E(TGFBeta_Smad_graph)$state
#'  > table(states)
#'  states
#'  1   2 
#'  103  70 
#'  }
#' 
#' @section Internal Functions:
#' 
#' The following functions are used by
#' \code{\link[graphsim:generate_expression]{generate_expression}}
#' to compute a simulated dataset. They can be called separately
#' to summarise the steps used to compute the final data matrix
#' or for troubleshooting.
#' 
#' \itemize{
#' 
#' \item \code{\link[graphsim:make_sigma]{make_sigma_mat_adjmat}},
#' \code{\link[graphsim:make_sigma]{make_sigma_mat_comm}}, 
#' \code{\link[graphsim:make_sigma]{make_sigma_mat_laplacian}}, and
#' \code{\link[graphsim:make_sigma]{make_sigma_mat_graph}} will
#' compute a Sigma (\eqn{\Sigma}) covariance matrix from an
#' adjacency matrix, common link matrix, Laplacian matrix,
#' or an \sQuote{igraph} object. There are computed as above
#' and passed to \code{\link[mvtnorm:Mvnorm]{rmvnorm}}.
#' 
#' \item \code{\link[graphsim:make_distance]{make_distance_adjmat}},
#' \code{\link[graphsim:make_distance]{make_distance_comm}}, 
#' \code{\link[graphsim:make_distance]{make_distance_laplacian}}, and
#' \code{\link[graphsim:make_distance]{make_distance_graph}} will
#' compute a distance matrix of relationships from an
#' adjacency matrix, common link matrix, Laplacian matrix,
#' or an \sQuote{igraph} object. There are computed as above
#' and passed to \code{\link[graphsim:make_sigma]{make_sigma}}.
#' 
#' \item \code{\link[graphsim:make_state]{make_state_matrix}}
#' will compute a \dQuote{state matrix} resolving positive and
#' negative correlations from a vector of edge properties. This
#' is called by \code{\link[graphsim:make_sigma]{make_sigma}}
#' and \code{\link[graphsim:generate_expression]{generate_expression}}
#' to ensure that the signs of correlations are consistent.
#' }
#' 
#' @section Examining Step-by-Step:
#'
#' These internal functions can be called to compute steps of
#' the simulation procedure and examine the results.
#' 
#' 1. first we create a graph structure and define the input parameters
#' 
#' \preformatted{
#' library("igraph")
#' graph_structure_edges <- rbind(c("A", "C"), c("B", "C"), c("C", "D"),c("D", "E"),
#'                                c("D", "F"), c("F", "G"), c("F", "I"), c("H", "I"))
#' graph_structure <- graph.edgelist(graph_structure_edges, directed = TRUE)
#' #sample size
#' data.n <- 100
#' #data distributions
#' data.cor <- 0.75
#' data.mean <- 3
#' data.sd <- 1.5
#' #inhibition states
#' edge_states <- c(1, 1, -1, -1, 1, 1, 1, 1)
#' }
#' 
#' 2. examine the relationships between the genes.
#' 
#' Here we can see which nodes share an edge:
#' 
#' \preformatted{
#' > adjacency_matrix <- make_adjmatrix_graph(graph_structure)
#' > adjacency_matrix
#'   A C B D E F G I H
#' A 0 1 0 0 0 0 0 0 0
#' C 1 0 1 1 0 0 0 0 0
#' B 0 1 0 0 0 0 0 0 0
#' D 0 1 0 0 1 1 0 0 0
#' E 0 0 0 1 0 0 0 0 0
#' F 0 0 0 1 0 0 1 1 0
#' G 0 0 0 0 0 1 0 0 0
#' I 0 0 0 0 0 1 0 0 1
#' H 0 0 0 0 0 0 0 1 0
#' }
#' 
#' Here we define a geometrically decreasing series of relationships
#' between genes based on distance by paths in the graph:
#' 
#' \preformatted{
#' > relationship_matrix <- make_distance_graph(graph_structure, absolute = FALSE)
#' > relationship_matrix
#'   A          C          B          D          E          F          G          I          H
#' A 1.00000000 0.20000000 0.10000000 0.10000000 0.06666667 0.06666667 0.05000000 0.05000000 0.04000000
#' C 0.20000000 1.00000000 0.20000000 0.20000000 0.10000000 0.10000000 0.06666667 0.06666667 0.05000000
#' B 0.10000000 0.20000000 1.00000000 0.10000000 0.06666667 0.06666667 0.05000000 0.05000000 0.04000000
#' D 0.10000000 0.20000000 0.10000000 1.00000000 0.20000000 0.20000000 0.10000000 0.10000000 0.06666667
#' E 0.06666667 0.10000000 0.06666667 0.20000000 1.00000000 0.10000000 0.06666667 0.06666667 0.05000000
#' F 0.06666667 0.10000000 0.06666667 0.20000000 0.10000000 1.00000000 0.20000000 0.20000000 0.10000000
#' G 0.05000000 0.06666667 0.05000000 0.10000000 0.06666667 0.20000000 1.00000000 0.10000000 0.06666667
#' I 0.05000000 0.06666667 0.05000000 0.10000000 0.06666667 0.20000000 0.10000000 1.00000000 0.20000000
#' H 0.04000000 0.05000000 0.04000000 0.06666667 0.05000000 0.10000000 0.06666667 0.20000000 1.00000000
#' }
#'
#' Here we can see the resolved edge states through paths in the
#' adjacency matrix:
#' 
#' \preformatted{
#' > names(edge_states) <- apply(graph_structure_edges, 1, paste, collapse = "-")
#' > edge_states
#' A-C B-C C-D D-E D-F F-G F-I H-I 
#' 1   1  -1  -1   1   1   1   1 
#' > state_matrix <- make_state_matrix(graph_structure, state = edge_states)
#' > state_matrix
#'    A  C  B  D  E  F  G  I  H
#' A  1  1  1 -1  1 -1 -1 -1 -1
#' C  1  1  1 -1  1 -1 -1 -1 -1
#' B  1  1  1 -1  1 -1 -1 -1 -1
#' D -1 -1 -1  1 -1  1  1  1  1
#' E  1  1  1 -1  1 -1 -1 -1 -1
#' F -1 -1 -1  1 -1  1  1  1  1
#' G -1 -1 -1  1 -1  1  1  1  1
#' I -1 -1 -1  1 -1  1  1  1  1
#' H -1 -1 -1  1 -1  1  1  1  1
#' }
#' 
#' 3. define a Sigma (\eqn{\Sigma}) covariance matrix
#' 
#' Here we can see that the signs match the \code{state_matrix}
#' and the covariance is based on the \code{relationship_matrix}
#' weighted by the correlation (\code{cor}) and standard
#' deviation (\code{sd}) parameters.
#' 
#' Note that where \code{sd = 1}, the diagonals will be \code{1}
#' and the off-diagonal terms will be correlations.
#' 
#' \preformatted{
#' > sigma_matrix <- make_sigma_mat_dist_graph(
#' +     graph_structure,
#' +     state = edge_states,
#' +     cor = data.cor,
#' +     sd = data.sd,
#' +     absolute = FALSE
#' + )
#' > sigma_matrix
#'    A         C         B        D         E        F         G         I         H
#' A  2.250000  1.687500  0.843750 -0.84375  0.562500 -0.56250 -0.421875 -0.421875 -0.337500
#' C  1.687500  2.250000  1.687500 -1.68750  0.843750 -0.84375 -0.562500 -0.562500 -0.421875
#' B  0.843750  1.687500  2.250000 -0.84375  0.562500 -0.56250 -0.421875 -0.421875 -0.337500
#' D -0.843750 -1.687500 -0.843750  2.25000 -1.687500  1.68750  0.843750  0.843750  0.562500
#' E  0.562500  0.843750  0.562500 -1.68750  2.250000 -0.84375 -0.562500 -0.562500 -0.421875
#' F -0.562500 -0.843750 -0.562500  1.68750 -0.843750  2.25000  1.687500  1.687500  0.843750
#' G -0.421875 -0.562500 -0.421875  0.84375 -0.562500  1.68750  2.250000  0.843750  0.562500
#' I -0.421875 -0.562500 -0.421875  0.84375 -0.562500  1.68750  0.843750  2.250000  1.687500
#' H -0.337500 -0.421875 -0.337500  0.56250 -0.421875  0.84375  0.562500  1.687500  2.250000
#' }
#' 
#' 4. generate an expression dataset using this sigma matrix
#' 
#' We use \code{generate_expression} to compute and expression
#' dataset, simulated using these parameters:
#' 
#' \preformatted{
#' > expression_data <- generate_expression(
#' +     n = data.n,
#' +     graph_structure,
#' +     state = edge_states,
#' +     cor = data.cor,
#' +     mean = data.mean,
#' +     sd = data.sd,
#' +     comm = FALSE,
#' +     dist = FALSE,
#' +     absolute = FALSE,
#' +     laplacian = FALSE
#' + )
#' > dim(expression_data)
#' [1]   9 100
#' }
#' 
#' Here we also compute the final observed correlations
#' in the simulated dataset:
#' 
#' \preformatted{
#' > cor_data <- cor(t(expression_data))
#' > dim(cor_data)
#' [1] 9 9
#' }
#' 
#' These functions are demonstrated in more detail
#' in the \href{https://CRAN.R-project.org/package=graphsim/vignettes/simulate_expression.html}{main} vignette.
#'  
#' @section Data Visualization:
#' 
#' Heatmaps can be used from the \code{\link[gplots:heatmap.2]{gplots}}
#' package to display these simulated datasets.
#' 
#' \preformatted{
#' library("gplots")
#' heatmap.2(adjacency_matrix, scale = "none", trace = "none",
#'           col = colorpanel(50, "white", "black"), key = FALSE)
#'           
#' heatmap.2(relationship_matrix, scale = "none", trace = "none",
#'           col = colorpanel(50, "white", "red"))
#'           
#' heatmap.2(state_matrix, scale = "none", trace = "none",
#'           col = colorpanel(50, "royalblue", "palevioletred"),
#'           colsep = 1:length(V(graph_structure)),
#'           rowsep = 1:length(V(graph_structure)))
#' 
#' heatmap.2(sigma_matrix, scale = "none", trace = "none",
#'           col = colorpanel(50, "royalblue", "white", "palevioletred"),
#'           colsep = 1:length(V(graph_structure)),
#'           rowsep = 1:length(V(graph_structure)))
#'           
#' heatmap.2(expression_data, scale = "none", trace = "none",
#'           col = colorpanel(50, "royalblue", "white", "palevioletred"),
#'           colsep = 1:length(V(graph_structure)),
#'          rowsep = 1:length(V(graph_structure)))
#' 
#' heatmap.2(cor_data, scale = "none", trace = "none",
#'           col = colorpanel(50, "royalblue", "white", "palevioletred"),
#'           colsep = 1:length(V(graph_structure)),
#'           rowsep = 1:length(V(graph_structure)))
#' }
#' 
#' In particular we can see here that the expected correlations
#' show by the \code{sigma_matrix} are similar to the observed
#' correlations in the \code{cor_data}.
#'  
#' @section Graph Visualization:
#'
#' The \sQuote{graphsim} package comes with a built-in plotting
#' function to display graph objects. 
#' 
#' \preformatted{
#' graph_structure_edges <- rbind(c("A", "C"), c("B", "C"), c("C", "D"),c("D", "E"),
#'                                c("D", "F"), c("F", "G"), c("F", "I"), c("H", "I"))
#' graph_structure <- graph.edgelist(graph_structure_edges, directed = TRUE)
#' plot_directed(graph_structure, layout = layout.kamada.kawai)
#' }
#'
#' This supports the \sQuote{state} parameter to display
#' activating relationships (with positive correlations)
#' and inhibiting or repressive relationships (with
#' negative correlations).
#'
#' \preformatted{
#' edge_states <- c(1, 1, -1, -1, 1, -1, 1, -1)
#' graph_structure <- graph.edgelist(graph_structure_edges, directed = TRUE)
#' plot_directed(graph_structure, state = edge_states,
#'               col.arrow = c("darkgreen", "red")[edge_states / 2 + 1.5]
#'               layout = layout.kamada.kawai)
#' }
#'
#' These states can also be passed from the \sQuote{state} edge
#' attribute of the graph object.
#'
#' \preformatted{
#" library("scales")
#' graph_pathway <- identity(TGFBeta_Smad_graph)
#' edge_properties <- E(graph_pathway)$state
#' plot_directed(graph_pathway,
#'               col.arrow = c(alpha("navyblue", 0.25),
#'                             alpha("red", 0.25))[edge_properties],
#'               fill.node = c("lightblue"),
#'               layout = layout.kamada.kawai)
#' }
#' 
#' This plotting function is demonstrated in more detail
#' in the plots_directed.Rmd plotting vignette.
#'
#' @section Further information:
#'   The graphsim package is published in the \emph{Journal of Open Source Software}.
#'   See the paper here for more details:
#'    \doi{10.21105/joss.02161}
#'   
#'   The graphsim GitHub repository is here:
#'   \href{https://github.com/TomKellyGenetics/graphsim}{TomKellyGenetics/graphsim}
#'   You can find the development version and submit an
#'   \href{https://github.com/TomKellyGenetics/graphsim/issues/new/choose}{issue}
#'   if you have questions or comments.
#'   
#' @section Citation:
#'   
#'   To cite package 'graphsim' in publications use:
#'   
#'   Kelly, S.T. and Black, M.A. (2020). graphsim: An R package for simulating gene
#'   expression data from graph structures of biological pathways.
#'   \emph{Journal of Open Source Software}, \bold{5}(51), 2161,
#'   \doi{10.21105/joss.02161}
#'   
#'   A BibTeX entry for LaTeX users is: \preformatted{
#'   @article{Kelly2020joss02161,
#'      doi = {10.21105/joss.02161},
#'      year = {2020},
#'      publisher = {The Open Journal},
#'      volume = {5},
#'      number = {51},
#'      pages = {2161},
#'      author = {S. Thomas Kelly and Michael A. Black},
#'      title = {graphsim: An R package for simulating gene expression data from graph structures of biological pathways},
#'      journal = {Journal of Open Source Software} 
#'    }
#'  }
#'  
#' @seealso 
#'  
#'  Publication at \emph{Journal of Open Source Software}:
#'  
#'  \itemize{
#'  \item \doi{10.21105/joss.02161}
#'  }
#'  
#'  GitHub repository:
#'  
#'  \itemize{
#'  \item \url{https://github.com/TomKellyGenetics/graphsim/}
#'  }
#'  
#'  Report bugs:
#'  
#'  \itemize{
#'  \item \url{https://github.com/TomKellyGenetics/graphsim/issues}
#'  }
#'  
#'  Contributions:
#'  
#'  \itemize{
#'  \item \url{https://github.com/TomKellyGenetics/graphsim/blob/master/CONTRIBUTING.md}
#'  }
#'  
NULL
