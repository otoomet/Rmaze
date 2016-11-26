#' Create a connected maze graph that represents a rectangular grid.
#' Nodes represent cells and edges betweem nodes represent wall sites.
#' All walls are initially on.
#'
#' @param nrows maze height (number of rows); default value set to 0.
#' @param ncols maze width (number of columns); default value set to 0.
#'
#' @return This function creates and returns a maze graph that represents a rectangular grid
#'  that matches user specified maze dimensions. Nodes in the graph will be named in the
#'  format Aij, where i corresponds to a row number  and j corresponds to a column number.
#'
#' @examples
#' maze1 <- makeGraph(10, 10)
#' maze1 <- makeMaze_dfs(maze1)
#' plotMaze(maze1, 10, 10)
#'
#' @export

makeGraph <- function(nrows=0, ncols=0){

  if (nrows <= 0)
    stop("Number of rows (height) has to be larger than zero!")

  if (ncols <= 0)
    stop("Number of columns (width) has to be larger than zero!")

  # Create list of edges
  df_hlp = data.frame(node1=c(), node2=c())
   rows <- seq(1, nrows)
   cols <- seq(1, ncols)
   nodes <- matrix(seq(length=nrows*ncols), nrows, ncols)
                           # nodes as integers
   nodeNames <- outer(rows, cols,
                      function(i, j) paste("A", i, j, sep="_"))
   hLinks <- cbind(as.integer(nodes[,-ncol(nodes)]),
                   as.integer(nodes[,-1]))
                           # link all nodes (except the rightmost column)
                           # to the nodes immediately right of them
   vLinks <- cbind(as.integer(nodes[-nrow(nodes),]),
                   as.integer(nodes[-1,]))
                           # link all nodes (except the lowermost row)
                           # to the nodes immediately below them
   links <- rbind(hLinks, vLinks)
   df_hlp <- data.frame(node1=nodeNames[links[,1]],
                        node2=nodeNames[links[,2]])
  # Create undirected graph from the edge list (data frame)
  gD <- igraph::simplify(igraph::graph_from_data_frame(df_hlp, directed=FALSE))

  # Set edge attribute (wall - meaning a wall between cells)
  gD <- igraph::set_edge_attr(gD , "wall", value = "ON")

  # Set node attributes (visited - a flag that will be used in traversing the graph)
  gD <- igraph::set_vertex_attr(gD , "visited", value = 0)


  # Return the graph
  gD
}
