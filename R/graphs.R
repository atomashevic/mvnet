library(igraph)

PlotBoK <- function(graph){
# Plots an igraph object with vertices labeled by name and edges labeled by name.
# If the graph has 4 vertices, a pre-defined layout is used, otherwise the
# layout is generated using layout_nicely.
#
# Args:
#   graph: an igraph object
#
# Returns:
#   A plot of the igraph object with vertices labeled by name and edges labeled
# by name.

  if (!inherits(graph,"igraph"))
    stop("'graph' must be an igraph object")
  if (length(V(graph)) == 0)
    stop("'graph' must have vertices")
  else {
    nV <- length(V(graph))
    nE <- length(E(graph))
  }
  if (length(V(graph)$name == 0)){
    V(graph)$name = LETTERS[1:nV]
  }
  if (nV == 4){
     l = rbind(c(0,2),c(1,1),c(1,3),c(2,2))
  }
  else {
     l = layout_nicely(graph)
  }
  plot.igraph(graph,
            vertex.label = V(graph)$name, vertex.label.color = "gray20",
            vertex.size = 20,
            edge.label = E(graph)$name,
            edge.label.family = "Helvetica",
            edge.label.color = "black",
            vertex.label.family = "Helvetica",
            vertex.color = "gray90", vertex.frame.color = "gray20",
            edge.width = 2,
            layout = l)
}

HasEulerianPath <- function(graph, start=NULL){
# Determines whether an igraph object has an Eulerian path.
#
# Args:
#   graph: an igraph object
#   start: a character of length 1 representing the starting vertex (optional)
#
# Returns:
#   TRUE if the graph has an Eulerian path, FALSE otherwise.
#
# Raises:
#   An error if the graph is not an igraph object,
# if start is not NULL or a character of length 1,
# if the graph vertices are not named when start is used,
# or if start is not a node of the graph.

  if (!inherits(graph,"igraph"))
      stop("'graph' must be an igraph object.")
  if (!is.null(start)){
      if (!inherits(start, "character") || length(start) != 1)
          stop("start must be NULL or a character of length 1.")
      if (length(V(graph)$name==0))
          stop("graph vertices have to be named if start vertice is used")
      if (!start %in% V(graph)$name)
          stop("start must be either NULL or a node of graph.")
  }
  vertices <- V(graph)
  if (length(vertices) == 0)
      return(TRUE)
  if (length(E(graph)) == 0 && is.null(start))
      return(TRUE)
  deg <- degree(graph, loops = TRUE)
  vertices <- vertices[deg>0]
  graph <- induced.subgraph(graph=graph, v=vertices)
  if(!is.connected(graph))
      return(FALSE)
  has.eulerian.path <- FALSE
  # each vertices' degree must be even
  # (with possible exception at start & end node)
  edL <- edges(graph, which=vertices)
  odd.deg.index <- which(deg %% 2 == 1)
  if(length(odd.deg.index)==0){
      if(is.null(start) || start %in%  V(graph)$name){
          has.eulerian.path <- TRUE
      }
  } else if(length(odd.deg.index) <= 2){
      start.v <- vertices[odd.deg.index]
      if (is.null(start) || start %in% start.v$name){
          has.eulerian.path <- TRUE
      }
  }
  return(has.eulerian.path)
}

CheckCI <- function(graph) {
  # Get the adjacency matrix of the graph
  adj_matrix <- as_adjacency_matrix(graph)
  
  # Get the names of the vertices
  vertices <- V(graph)$name
  
  # Initialize a vector to store the results
  results <- character(0)
  
  # Loop over all pairs of vertices
  for (i in 1:(length(vertices) - 1)) {
    for (j in (i + 1):length(vertices)) {
      # If there is no edge between the vertices, they are conditionally independent
      if (adj_matrix[i, j] == 0 && adj_matrix[j, i] == 0) {
        results <- c(results, paste(vertices[i], "||", vertices[j]))
      }
    }
  }
  
  # Return the results
  return(results)
}