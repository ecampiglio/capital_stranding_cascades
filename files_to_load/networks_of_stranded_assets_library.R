#
agg_io <- function(mat, agg_sect, disagg_sect) {
  newmat <- mat
  if (length(disagg_sect) == 1) {
    newmat[,agg_sect] <- newmat[,disagg_sect]
    newmat[agg_sect,] <- newmat[disagg_sect,]
  } else {
    newmat[,agg_sect] <- apply(newmat[,disagg_sect], 1, sum)
    newmat[agg_sect,] <- apply(newmat[disagg_sect,], 2, sum)
  }
  newmat[,disagg_sect] <- NULL
  return(newmat[!rownames(newmat) %in% disagg_sect,])
}

remove_io <- function(mat, remove_sect) {
  mat[,remove_sect] <- NULL
  mat <- mat[!rownames(mat) %in% remove_sect,]
  return(mat)
}

io_entry <- function(mat, sector, type = "11") {
  if (type == "11") {
    return(mat[rownames(mat) %in% sector, sector])
  } else if (type == "12") {
    return(mat[rownames(mat) %in% sector, !colnames(mat) %in% sector])
  } else if (type == "21") {
    return(mat[!rownames(mat) %in% sector, sector])
  } else if (type == "22") {
    return(mat[!rownames(mat) %in% sector, !colnames(mat) %in% sector])
  } else {
    return("Error: 'type' must be '11', '12', '21', or '22'")
  }
}

# Note: emode is "in", "out", "all", or "total"
ego_shells <- function(graph, node, emode) {
  shells <- list()
  shells[[1]] <- node
  order <- 1
  shells[[order + 1]] <- V(graph)[setdiff(V(graph)[ego(graph, node, mode = emode, order=order)[[1]]], V(graph)[node])]
  repeat {
    shell <- setdiff(ego(graph, node, mode = emode, order=order + 1)[[1]], ego(graph, node, mode = emode, order=order)[[1]])
    if (!length(shell)) {
      break
    }
    order <- order + 1
    shells[[order + 1]] <- V(graph)[shell]
  }
  remaining <- V(graph)[setdiff(V(graph), ego(graph, node, mode = emode, order=order + 1)[[1]])]
  if (length(remaining)) {
    shells[[order + 2]] <- remaining
  }
  return(shells)
}

# Note: emode is "in", "out", "all", or "total"
ego_layout <- function(graph, node, emode, jitter = NULL, nmax = 15, inverted = FALSE) {
  shells <- ego_shells(graph, node, emode)
  xy <- array(NA, dim=c(gorder(graph), 2));
  lengths <- array()
  for (shndx in 1:length(shells)) {
    lengths[shndx] <- length(shells[[shndx]])
  }
  maxlen <- min(nmax,max(lengths))
  
  for(shndx in 1:length(shells)) {
    if (inverted) {
      y <- (shndx-1) /(length(shells) - 1);
    } else {
      y <- 1 - (shndx-1) /(length(shells) - 1);
    }
    shell_indices <- as.integer(V(graph)[shells[[shndx]]]);
    ys <- rep(y, length(shell_indices))
    if (length(shell_indices) == 1) {
      xs <- 0
      nlines <- 1
      currlen <- 1
    } else {
      currlen <- length(shell_indices)
      nlines <- ceiling(currlen/nmax)
      linelen <- ceiling(currlen/nlines)
      xs <- (linelen/maxlen) * (2 * (1:linelen - 1)/(linelen - 1) - 1)
      if (!is.null(jitter)) {
        offset <- -jitter * 0.5 * (nlines-1)
        leftndx <- 1
        for (i in 1:nlines) {
          rightndx <- min(currlen,leftndx + linelen - 1)
          ys[leftndx:rightndx] <- y + offset
          leftndx <- leftndx + linelen
          offset <- offset + jitter
        }
      }
    }
    xy[shell_indices, 1] <- rep(xs,nlines)[1:currlen];
    xy[shell_indices, 2] <- ys;
  }
  return(xy);
}

ego_circle_layout <- function(graph, node, nlayer = 4) {
  xy <- array(NA, dim=c(gorder(graph), 2))
  node.row <- E(graph)[.from(V(graph)[node])]
  if (length(node.row) < length(V(graph))) {
    nlayer.conn <- nlayer - 1
  } else {
    nlayer.conn <- nlayer
  }
  node.row.names <- ends(graph, node.row)
  sum.n <- nlayer.conn * (nlayer.conn + 1)/2
  probs <- c(0,cumsum(seq(nlayer.conn,1,-1)/sum.n))
  breaks <- quantile(node.row$weight, probs=probs)
  ranks <- cut(node.row$weight, breaks=breaks, labels=seq(nlayer.conn,1,-1), include.lowest=TRUE)
  # First, assign nlayer to all vertices
  V(graph)$rank <- nlayer
  # Then override for the ones assigned to connected nodes
  # Have to do some contortions to get rank values (levels), not factor index
  V(graph)[ends(graph, node.row)[,2]]$rank <- as.numeric(levels(ranks)[ranks])
  # Finally, assign node rank 0
  V(graph)[node]$rank <- 0
  
  ### Add onto circles
  r <- 0.5
  twopi <- 2.0 * base::pi # It may be hidden/overwritten
  for (layer in 0:nlayer) {
    r.layer <- r * layer/nlayer
    layer.vs <- V(graph)[V(graph)$rank == layer]
    theta_step <- twopi/length(layer.vs)
    theta <- theta_step/2
    for (i in 1:length(layer.vs)) {
      V(graph)[layer.vs][i]$x <- r.layer * cos(theta)
      V(graph)[layer.vs][i]$y <- r.layer * sin(theta)
      theta <- theta + theta_step
    }
  }
  for (i in 1:nrow(xy)) {
    xy[i,1] <- V(graph)[i]$x
    xy[i,2] <- V(graph)[i]$y
  }
  return(xy)
}

# Return a branching-tree type network starting from node
# Pick the top q items by weight for each node (so edges must have a "weight" attribute)
# If "depth" is NA, then include all nodes
# The returned network is directed, weighted, with a "layer" atrribute on vertices
# TODO: (?) Allow nodes to appear in more than one layer (have to be careful with label/name)
cascade_network <- function(graph, node, q = 0.25, depth = NA) {
  if (is.na(depth)) {
    # Find the maximum possible depth (one vertex per layer)
    depth <- length(V(graph))
  }
  cn.graph <- make_empty_graph(n = 0, directed = TRUE) + vertex(node)
  V(cn.graph)[node]$layer <- 0
  for (l in 1:depth) {
    new <- 0
    # Loop over all vertices in the previous layer
    for (v in V(cn.graph)[layer == l - 1]$name) {
      q.val <- quantile(E(graph)[.from(V(graph)[v])]$weight, 1-q)
      v.elist <- E(graph)[.from(V(graph)[v]) & weight >= q.val]
      for (e in v.elist[order(v.elist$weight, decreasing=TRUE)]) {
        v.end <- ends(graph, e)[,2]
        if (!v.end %in% V(cn.graph)$name) {
          # Haven't already added it
          cn.graph <- cn.graph + vertex(v.end, layer = l)
          cn.graph <- cn.graph + edge(V(cn.graph)[v], V(cn.graph)[v.end], weight = E(graph)[e]$weight)
          new <- new + 1
        }
      }
    }
    # Break if no more nodes to assign -- that is, nothing "new"
    if (!new) break
  }
  return(cn.graph)
}


# EC modification of function above to allow for edges within layers
cascade_network_EC <- function(graph, node, q = 0.25, depth = NA) {
  if (is.na(depth)) {
    # Find the maximum possible depth (one vertex per layer)
    depth <- length(V(graph))
  }
  cn.graph <- make_empty_graph(n = 0, directed = TRUE) + vertex(node)
  V(cn.graph)[node]$layer <- 1
  for (l in 1:depth) {
    new <- 0
    # Loop over all vertices in the previous layer
    for (v in V(cn.graph)[layer == l]$name) {
      q.val <- quantile(E(graph)[.from(V(graph)[v])]$weight, 1-q)
      v.elist <- E(graph)[.from(V(graph)[v]) & weight >= q.val]
      for (e in v.elist[order(v.elist$weight, decreasing=TRUE)]) {
        v.end <- ends(graph, e)[,2]
        if (!v.end %in% V(cn.graph)$name) {
          # Haven't already added it
          cn.graph <- cn.graph + vertex(v.end, layer = l+1)
          new <- new + 1
        }
        if (!v.end %in% V(cn.graph)[layer <= l]$name){
          cn.graph <- cn.graph + edge(V(cn.graph)[v], V(cn.graph)[v.end], weight = E(graph)[e]$weight)
        }
      }
    }
    # Break if no more nodes to assign -- that is, nothing "new"
    if (!new) break
  }
  return(cn.graph)
}


# EC modification of function above to allow for edges within layers
cascade_network_EC2 <- function(graph, node, q = 0.25, depth = NA) {
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
        #E(graph)[e]$input.loss<-Gt[v.end,v.init]/diag(Gt)[v.end]
        #casc_S<-E(graph)[e]$weight*E(graph)[e]$input.loss
        #ends(graph,e)[,2]
        if (!v.end %in% V(cn.graph)$name) {
          # Haven't already added it
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

# Note: emode is "in", "out", "all", or "total"
ego_directed <- function(graph, node, emode) {
  # If "all" or "total", nothing to do
  if (!emode %in% c("in","out")) {
    return(graph)
  }
  shells <- ego_shells(graph, node, emode)
  v_remain <- V(graph)
  retval <- graph
  for (shell in shells) {
    v_remain <- difference(v_remain, V(graph)[shell])
    for (v_ego in V(graph)[shell]) {
      for (v_alter in v_remain) {
        if (emode == "in") {
          if (are_adjacent(graph, v_ego, v_alter)) {
            edge_name <- paste(V(graph)[v_ego]$name,V(graph)[v_alter]$name,sep="|")
            retval <- retval - edge(edge_name)
          }
        } else {
          if (are_adjacent(graph, v_alter, v_ego)) {
            edge_name <- paste(V(graph)[v_alter]$name,V(graph)[v_ego]$name,sep="|")
            retval <- retval - edge(edge_name)
          }
        }
      }
    }
  }
  return(retval)
}

# Input: An n x n matrix m
# Output: An n x n matrix m with (approximately) unit marginals
uniform_scaling <- function(m) {
  mcalc <- as.matrix(m)
  epsilon <- 1e-5
  mr <- sign(apply(mcalc, 1, sum))
  mc <- sign(apply(mcalc, 2, sum))
  mr <- mr * length(mr)/sum(mr)
  mc <- mc * length(mc)/sum(mc)
  i <- 0
  repeat {
    i <- i + 1
    testvar <- (sum(abs(apply(mcalc, 1, sum) - mr)) + sum(abs(apply(mcalc, 2, sum) - mc)))/nrow(m)
    if (testvar < epsilon | i > 100) {
      break
    }
    r <- ifelse(!mr, 1, mr/apply(mcalc, 1, sum))
    mcalc <- diag(r) %*% mcalc
    s <- ifelse(!mc, 1, mc/apply(mcalc, 2, sum))
    mcalc <- mcalc %*% diag(s)
  }
  return(mcalc)
}

###########################################
#
# Backward and forward links
#
###########################################
leontinv <- function(a) {
  return(solve(diag(nrow(a)) - a))
}

fb_links <- function(m) {
  # Not necessarily Leontief -- can use for Ghosh as well
  leont <- solve(diag(nrow(m)) - m)
  r <- nrow(leont) * apply(leont,1,sum)/sum(leont)
  c <- nrow(leont) * apply(leont,2,sum)/sum(leont)
  return(list(row = r, column = c))
}

# a: Leontief inter-industry matrix
# f: Final 
# s: named sectors
fb_links_cella_1 <- function(a, f, s) {
  for (type in c("11","12","21","22")) {
    assign(paste0("a",type), io_entry(a, s, type = type))
  }
  if (is.null(dim(a11))) {
    # Convert scalar to 1x1 array, if only one sector
    a11 <- as.matrix(a11)
    a12 <- t(as.matrix(a12))
    a21 <- as.matrix(a21)
  }
  f1 <- f[s]
  f2 <- f[!names(f) %in% s]
  b11 <- leontinv(a11)
  b22 <- leontinv(a22)
  h <- solve(diag(nrow(a11)) - a11 - a12 %*% b22 %*% a21)
  # Backward and forward links
  bl <- sum((h - b11) %*% f1) + sum(b22 %*% a21 %*% h %*% f1)
  fl <- sum(h %*% a12 %*% b22 %*% f2) + sum(b22 %*% a21 %*% h %*% a12 %*% b22 %*% f2)
  return(c(bl,fl))
}

fb_links_cella <- function(a, f) {
  bl <- vector()
  fl <- vector()
  for (s in names(f)) {
    bfl <- fb_links_cella_1(a, f, s)
    bl <- append(bl, bfl[1])
    fl <- append(fl, bfl[2])
  }
  names(bl) <- names(f)
  names(fl) <- names(f)
  return(list(backward = bl, forward = fl))
}

###########################################
#
# Minimal fully-connected network
#
###########################################
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
  print(thresh)
}

###########################################
#
# Plot I/O
#
###########################################
plot_io <- function(m) {
  # From http://www.phaget4.org/R/image_matrix.html and http://www.phaget4.org/R/myImagePlot.R
  # ----- Define a function for plotting a matrix ----- #
  myImagePlot <- function(x, ...){
    old.par <- par(no.readonly = TRUE) # all par settings which could be changed.
    on.exit(par(old.par))
    min <- min(x)
    max <- max(x)
    yLabels <- rownames(x)
    xLabels <- colnames(x)
    title <-c()
    # check for additional function arguments
    if( length(list(...)) ){
      Lst <- list(...)
      if( !is.null(Lst$zlim) ){
        min <- Lst$zlim[1]
        max <- Lst$zlim[2]
      }
      if( !is.null(Lst$yLabels) ){
        yLabels <- c(Lst$yLabels)
      }
      if( !is.null(Lst$xLabels) ){
        xLabels <- c(Lst$xLabels)
      }
      if( !is.null(Lst$title) ){
        title <- Lst$title
      }
    }
    # check for null values
    if( is.null(xLabels) ){
      xLabels <- c(1:ncol(x))
    }
    if( is.null(yLabels) ){
      yLabels <- c(1:nrow(x))
    }
    
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
    
    # Red and green range from 0 to 1 while Blue ranges from 1 to 0
    ColorRamp <- rgb( seq(1,0,length=256),  # Red
                      seq(1,0,length=256),  # Green
                      seq(1,0,length=256))  # Blue
    ColorLevels <- seq(min, max, length=length(ColorRamp))
    
    # Reverse Y axis
    reverse <- nrow(x) : 1
    yLabels <- yLabels[reverse]
    x <- x[reverse,]
    
    # Data Map
    par(mar = c(3,5,2.5,2))
    image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
          ylab="", axes=FALSE, zlim=c(min,max))
    if( !is.null(title) ){
      title(main=title)
    }
    axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
    axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
         cex.axis=0.7)
    
    # Color Scale
    par(mar = c(3,2.5,2.5,2))
    image(1, ColorLevels,
          matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
          col=ColorRamp,
          xlab="",ylab="",
          xaxt="n")
    
    layout(1)
  }
  # ----- END plot function ----- #
  myImagePlot(as.matrix(m),xLabels = rownames(m), yLabels = colnames(m))
}


###########################################################
#Function for plotting an elliptical node
#https://stackoverflow.com/questions/48457824/how-to-plot-a-ellipse-node-with-igraph
#########################################################

myellipse <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/30 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  vertex.border <- params("vertex", "frame.color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.border <- vertex.border[v]
  }
  
  draw.ellipse(x=coords[,1], y=coords[,2],
               a = vertex.size, b=vertex.size/2, col=vertex.color, border=vertex.border)
}

