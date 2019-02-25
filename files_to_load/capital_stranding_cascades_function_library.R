
# The function below creates the cascade network
cascade_network <- function(graph, node, q = 0.25, depth = NA) {
  if (is.na(depth)) {
    # Find the maximum possible depth (one vertex per layer)
    depth <- length(V(graph))
  }
  cn.graph <- make_empty_graph(n = 0, directed = TRUE) + vertex(node)
  V(cn.graph)[node]$layer <- 1
  V(cn.graph)[node]$input_loss <- 1
  for (l in 1:depth) {
    new <- 0
    # Loop over all vertices in the previous layer
    for (v in V(cn.graph)[layer == l]$name) {
      q.val <- quantile(E(graph)[.from(V(graph)[v])]$weight, 1-q)
      v.elist <- E(graph)[.from(V(graph)[v]) & weight >= q.val]
      for (e in v.elist[order(v.elist$weight, decreasing=TRUE)]) {
        v.end <- ends(graph, e)[,2]
        if (!v.end %in% V(cn.graph)$name) {
          cn.graph <- cn.graph + vertex(v.end, layer = l+1, input_loss=0)
          new <- new + 1
        }
        if (!v.end %in% V(cn.graph)[layer <= l]$name){
          cn.graph <- cn.graph + edge(V(cn.graph)[v], V(cn.graph)[v.end], weight = E(graph)[e]$weight*V(cn.graph)[v]$input_loss)
          V(cn.graph)[v.end]$input_loss<-V(cn.graph)[v.end]$input_loss+Gt[v.end,v]/diag(Gt)[v.end]
        }
      }
    }
    # Break if no more nodes to assign -- that is, nothing "new"
    if (!new) break
  }
  return(cn.graph)
}

# The function below creates the minimal fully-connected version of a network
mfc <- function(m) {
  m.thresh <- max(m)
  m.offdiag <- m
  diag(m.offdiag) <- 0
  for (i in 1:nrow(m.offdiag)) {
    cmax <- apply(m.offdiag,1,max)[i]
    rmax <- apply(m.offdiag,2,max)[i]
    ijmax <- max(cmax, rmax)
    if (ijmax < m.thresh) {
      m.thresh = ijmax
    }
  }
  m.mfc <- m
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if (m[i,j] < m.thresh) {
        m.mfc[i,j] = 0.0
      }
    }
  }
  return(m.mfc)
  thresh<<-m.thresh
}

