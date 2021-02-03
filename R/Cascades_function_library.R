
############# Capital Stranding Cascades function library ##################

## Structure:
# - Vector ranking & naming
# - Disaggregation of the fossil sector (with ICIO ratios)
# - Matrix balancing (TRAS algorithm)
# - Cascade network functions
# - Exposure network functions
# - Simplification of plotting chunks (barcharts & networks)


# load packages used in the functions below

# required package names
pkgs <- c("Matrix", "dplyr", "reshape2")

# install packages not yet installed
installed_pkgs <- pkgs %in% rownames(installed.packages())
if (any(installed_pkgs == FALSE)) {
  install.packages(pkgs[!installed_pkgs])
}

# load packages
invisible(lapply(pkgs, library, character.only = TRUE))



# Vector ranking & naming ------------------------------------------------------------

## this function takes a given vector x and returns a ranked vector with names and the corresponding (rounded) values in brackets (e.g. "AUT_B(1.433)")
rank_and_name <- function(x, round_digits=3, decreasing = TRUE){
  order<-x[order(x, decreasing = decreasing)]
  out <- paste0(names(order)," (",round(order,round_digits),")")
  return(out)}



# Disaggregation of the fossil sector ------------------------------------------------

## function for the fossil disaggregation of Z into 3 sub-sectors using ratios from ICIO: 

fossil_dissaggregate_ICIO <- function(Z_mat, RatiosCols, RatiosRows) { 
  
  # step 1: disaggregate the columns of Z with the FossilRatiosCols matrix
  # note that if we start disaggregating with the columns, we need to take into account that rows of Z are not already disaggregated, so we need a version of the FossilRatiosCols matrix which has been computed with MIN+ in the rows
  
  Z_step1 <- matrix(0, nrow = length(country_sec_base), ncol = length(country_sec), dimnames = list(country_sec_base, country_sec))
  
  for (country in countries){
    for (sector in sectors){
      if (sector == "MINfos"){
        Z_step1[, paste0(country,"_",sector)] <- Z_mat[,paste0(country,"_MIN+")] * RatiosCols[, country, "fos"]
      } else if (sector == "MINoth") {
        Z_step1[, paste0(country,"_",sector)] <- Z_mat[,paste0(country,"_MIN+")] * RatiosCols[, country, "oth"]
      } else if (sector == "MINsup") {
        Z_step1[, paste0(country,"_",sector)] <- Z_mat[,paste0(country,"_MIN+")] * RatiosCols[, country, "sup"]
      } else {
        Z_step1[, paste0(country,"_",sector)] <- Z_mat[,paste0(country,"_",sector)]
      }
    }
  }
  
  # step 2: disaggregate the columns of Z_step1 with the FossilRatiosRows matrix
  
  Z_step2 <- matrix(0, nrow = length(country_sec), ncol = length(country_sec), dimnames = list(country_sec, country_sec))
  
  for (country in countries){
    for (sector in sectors){
      if (sector == "MINfos") {
        Z_step2[paste0(country,"_",sector),] <- Z_step1[paste0(country,"_MIN+"),] * RatiosRows[country, , "fos"]
      } else if (sector == "MINoth") {
        Z_step2[paste0(country,"_",sector),] <- Z_step1[paste0(country,"_MIN+"),] * RatiosRows[country, , "oth"]
      } else if (sector == "MINsup") {
        Z_step2[paste0(country,"_",sector),] <- Z_step1[paste0(country,"_MIN+"),] * RatiosRows[country, , "sup"]
      } else {
        Z_step2[paste0(country,"_",sector),] <- Z_step1[paste0(country,"_",sector),]
      }
    }
  }
  
  return(Z_step2)
}



## same as above, but for for simple vectors (like output and capital stock)

fossil_dissaggregate_vect_ICIO <- function (input_vector, ratio_matrix){
  
  output_vector <- array(0, dim=length(country_sec), dimnames=list(country_sec))
  
  for (country in countries){
    for (sector in sectors) {
      if (sector == "MINfos"){
        output_vector[paste0(country,"_",sector)] <- input_vector[paste0(country,"_MIN+")] * ratio_matrix[country,"fos"]
      } else if (sector == "MINoth") {
        output_vector[paste0(country,"_",sector)] <- input_vector[paste0(country,"_MIN+")] * ratio_matrix[country,"oth"]
      } else if (sector == "MINsup") {
        output_vector[paste0(country,"_",sector)] <- input_vector[paste0(country,"_MIN+")] * ratio_matrix[country,"sup"]
      } else {
        output_vector[paste0(country,"_",sector)] <- input_vector[paste0(country,"_",sector)]
      }
    }
  }
  return(output_vector)
}


# Matrix balancing --------------------------------------------------------------------

# TRAS ("two stage RAS"/"tri-poportional RAS"), according to Gilchrist & St.Louis (2004)
# additionally to row and column targets, it takes a third target element: matrix "blockgoal", ensuring block totals of known aggregates of cells
# the aggregation rules (leading from the disaggregated to the known aggregate matrix) are given by aggregation matrices P and Q

TRAS <- function(Z0, rowgoal, colgoal, blockgoal, P, Q, tol = 1e-5, maxiter = 100, verbose = T){
  
  # check input, convert to matrix if necessary
  if(class(Z0)!="matrix") 
  {Z0 <- data.matrix(Z0)}
  
  # create sparse versions of the aggregation matrices (faster multiplications!!!)
  sP <- Matrix(P, sparse = T)
  sQ <- Matrix(Q, sparse = T)
  
  iter <- 1 
  
  repeat{
    
    # standard RAS steps (step 1 & 2)
    
    r_mar <- rowgoal/rowSums(Z0)
    r_mar[!is.finite(r_mar)] <- 1
    Z0 <-  Z0*r_mar
    
    s_mar <- colgoal/colSums(Z0)
    s_mar[!is.finite(s_mar)] <- 1
    Z0 <-  t(t(Z0)*s_mar)
    
    # TRAS step (step 3)
    
    G0 <- blockgoal / (sP %*% Z0 %*% sQ)
    G0[!is.finite(G0)] <- 1 
    
    T0 <- as.matrix(t(sP) %*% G0 %*% t(sQ))
    Z0 <- Z0 * T0
    
    # print maximum values of margins
    
    if (verbose == T) {
      cat("iteration:", iter, "\n", "rowmargin_max =", max(abs(1-r_mar)), "\n", "colmargin_max =", max(abs(1-s_mar)), "\n", "blockmargin_max =", max(abs(1-T0)),"\n")
      # cat("iteration:", iter, "\n", "rowmargin_max =", max(abs(1-r_mar[r_mar !=0])), "\n", "colmargin_max =", max(abs(1-s_mar[s_mar !=0])), "\n", "blockmargin_max =", max(abs(1-T0[T0 !=0])),"\n")
    }
    
    # breaking condition: stop once maximum margin ratio is smaller than tolerance
    
    if (max(abs(1-r_mar), abs(1-s_mar), abs(1-T0)) <= tol){ # or (max(abs(1-r_mar[r_mar !=0]), abs(1-s_mar[s_mar !=0]), abs(1-T0[T0 !=0])) <= tol)
      cat("Convergence reached after", iter , "iterations!\n")
      break
    }
    
    #  # alternatively use absolute difference between target and current margins
    #  if (max (abs(colSums(Z0)-colgoal), abs(rowSums(Z0)-rowgoal), abs(as.matrix((sP %*% Z0 %*% sQ))-blockgoal)) <= tol) {
    #    cat("Convergence reached after", iter , "iterations!\n")
    #  break
    #  }
    
    if(iter >= maxiter){
      cat("No convergence reached after", maxiter , "iterations!\n") 
      break
    } 
    
    iter <- iter + 1
  }
  res <- list("Z" = Z0, "rowmargin" = r_mar, "colmargin" = s_mar, "blockmargin" = T0)
  return(res)
}



## extended TRAS allowing negative values --> used to balance whole IOT (including FD and VA which contain negative values)
# it removes negative values from the matrix, adapts the given row/column/block targets accordingly and adds them back in the end
TRAS_extended <- function(IOT, rowgoal, colgoal, blockgoal, P, Q, tol = 1e-3, maxiter = 300, verbose = T){
  
  # check input, convert to matrix if necessary
  if(class(IOT)!="matrix") 
  {IOT <- data.matrix(IOT)}
  
  # create sparse versions of the aggregation matrices (faster multiplications!)
  sP <- Matrix(P, sparse = T)
  sQ <- Matrix(Q, sparse = T)
  
  # extract for negative values in the input matrix
  IOT_neg <- IOT
  IOT_neg[IOT_neg > 0] <- 0
  # remove negative values from the IOT
  IOT_pos <- IOT
  IOT_pos[IOT_pos < 0] <- 0
  
  # adapt row, column and block targets to matrix without negative values
  rowgoal_pos <- rowgoal - rowSums(IOT_neg)
  colgoal_pos <- colgoal - colSums(IOT_neg)
  blockgoal_pos <- blockgoal #- as.matrix(sP %*% IOT_neg %*% sQ)
  blockgoal_pos[blockgoal_pos < 0] <- 0 
  
  iter <- 1 
  
  repeat{
    
    # standard RAS steps (step 1 & 2)
    
    r_mar <- rowgoal_pos/rowSums(IOT_pos)
    r_mar[!is.finite(r_mar)] <- 1
    IOT_pos <-  IOT_pos*r_mar
    
    s_mar <- colgoal_pos/colSums(IOT_pos)
    s_mar[!is.finite(s_mar)] <- 1
    IOT_pos <-  t(t(IOT_pos)*s_mar)
    
    # TRAS step (step 3)
    
    G0 <- blockgoal_pos / (sP %*% IOT_pos %*% sQ)
    G0[!is.finite(G0)] <- 1 
    
    T0 <- as.matrix(t(sP) %*% G0 %*% t(sQ))
    IOT_pos <- IOT_pos * T0
    
    # print maximum values of margins
    
    if (verbose == T) {
      cat("iteration:", iter, "\n", "rowmargin_max =", max(abs(1-r_mar)), "\n", "colmargin_max =", max(abs(1-s_mar)), "\n", "blockmargin_max =", max(abs(1-T0)),"\n")
      # cat("iteration:", iter, "\n", "rowmargin_max =", max(abs(1-r_mar[r_mar !=0])), "\n", "colmargin_max =", max(abs(1-s_mar[s_mar !=0])), "\n", "blockmargin_max =", max(abs(1-T0[T0 !=0])),"\n")
    }
    
    # breaking condition: stop once maximum margin ratio is smaller than tolerance
    
    if (max(abs(1-r_mar), abs(1-s_mar), abs(1-T0)) <= tol){ # or (max(abs(1-r_mar[r_mar !=0]), abs(1-s_mar[s_mar !=0]), abs(1-T0[T0 !=0])) <= tol)
      cat("Convergence reached after", iter , "iterations!\n")
      break
    }
    
    #  # alternatively use absolute difference between target and current margins
    #  if (max (abs(colSums(Z0)-colgoal), abs(rowSums(Z0)-rowgoal), abs(as.matrix((sP %*% Z0 %*% sQ))-blockgoal)) <= tol) {
    #    cat("Convergence reached after", iter , "iterations!\n")
    #  break
    #  }
    
    if(iter >= maxiter){
      cat("No convergence reached after", maxiter , "iterations!\n") 
      break
    } 
    
    iter <- iter + 1
  }
  
  # re-enter the negative values into the system
  IOT_fin <- IOT_pos + IOT_neg
  Z_fin <- IOT_fin[1:(nrow(IOT)-1), 1:(ncol(IOT)-1)]
  FD_fin <- IOT_fin[1:(nrow(IOT)-1), "FD"]
  VA_fin <- IOT_fin["VA", 1:(ncol(IOT)-1)]
  
  res <- list("Z" = Z_fin, "FD" = FD_fin, "VA" = VA_fin, "IOT" = IOT_fin, "rowmargin" = r_mar, "colmargin" = s_mar, "blockmargin" = T0)
  return(res)
}



# Cascade network functions ---------------------------------------------------------------------------

# Return a branching-tree type network starting from node
# pick the top n items by weight for each node (so edges must have a "weight" attribute)
# the returned network is directed, weighted, with a "layer" attribute on vertices

# final version:
# uses only first stranding round (S1 = diag(k_int)%*%Bt) matrix as main argument 
# input loss is computed as loss of previous node multiplied by the corresponding element of Bt
# if a node has several incoming stranding links, it's input loss parameter is the sum of input loss calculations from of all incoming links
# sectors can appear multiple times in the network, but only once per layer
# note that the layer name of the originating node is set to 0

cascade_network_fin <- function(matrix, node, n_top = 3, depth = NA, B_matrix = B) {
  
  # first, take care of the case in which depth is not defined. 
  if (is.na(depth)) {
    # Find the maximum possible depth (=columns of the matrix)
    stop ("depth has to be defined in this function")
  }
  # create a graph with only the root vertex
  cn.graph <- make_empty_graph(n = 0, directed = TRUE) + vertex(node)
  # the root vertex is assigned two attributes: layer (=0) and input_loss (=1)
  V(cn.graph)[node]$layer <- 0 
  V(cn.graph)[node]$input_loss <- 1
  # we start the loop that assigns vertices to layers (number of layers defined by "depth")
  for (l in 0:(depth-1)) {
    # loop over all vertices in the previous layer
    for (v in V(cn.graph)[layer == l]$name) {
      # for sector v, we extract the corresponding column in the S or SB matrix 
      v_sect_col <- matrix[,v]
      # remove self-loops
      v_sect_col[v] <- 0 
      # remove B sector(s)
      if (nchar(names(v_sect_col)[1]) <= 3){  # if the length of the first sector name is <= 3 (i.e. if no country prefixes are used)
        v_sect_col[sect_focus] <- 0 } else {
          v_sect_col[grepl(paste0("_", sect_focus), names(v_sect_col))] <- 0 }
      # apply input loss to the whole column
      v_sect_col_weighted <- v_sect_col*V(cn.graph)[layer==l][v]$input_loss
      # and take the top n stranding links in this column (excluding zeros)
      n.list <- head(sort(v_sect_col_weighted[v_sect_col_weighted > 0], decreasing = TRUE), n_top)
      
      # now we add vertices given by the sectors in the ordered n.list (only if those sectors are not yet included)
      for (s in names(n.list)){ 
        if (! s %in% V(cn.graph)[layer == l+1]$name  ) { 
          cn.graph <- cn.graph + vertex(s, layer = l+1, input_loss = 0)
        }
        # and connect them with edges, simply given by the value in the matrix
        # i.e. the weight of the edge is the value [s,v] from the S matrix, filtered by the input_loss value of the origin vertex  
        # if (! s %in% V(cn.graph)[layer <= l]$name){ # remove this condition if upward links should be allowed as well!
        if (s %in% V(cn.graph)[layer == l+1]$name) { # this condition is not really necessary (tautology)
          cn.graph <- cn.graph + edge(V(cn.graph)[layer==l][v],V(cn.graph)[layer==l+1][s], weight = n.list[s]) 
          # add input to input loss, also if the vertex already exists and only a new edge to it is drawn
          V(cn.graph)[layer == l+1][s]$input_loss <- V(cn.graph)[layer == l+1][s]$input_loss + V(cn.graph)[layer==l][v]$input_loss * t(B_matrix)[s,v]
        }
      }  
    }
    
  }
  return(cn.graph)
} 


# Exposure network functions ------------------------------------------------------------------------------------------

# for each exposed sector on the bottom, identify the n most important incoming direct, 2-step and 3-step stranding linkages from fossil sectors
# --> final version

exposure_network_fin <- function(exposed, m_top = 2, depth = 2, B = B, S1 = S1, color_1 = 'rgba(0,0,102,0.75)', color_2 = 'rgba(255,0,102,0.75)', color_3 = 'rgba(230,210,60,0.75)') {
  
  # first, take care of the case in which depth is not defined
  if (!depth %in% c(2,3)) {
    stop ("depth has to be defined in this function and can either be 2 or 3")
  }
  
  # create a graph with the exposed sectors in the lowest and the originating sectors in the highest layer
  exp.graph <- make_empty_graph(n = 0, directed = TRUE) + vertices(exposed, layer = depth)
  
  # save the transpose of B as a variable
  Bt <- t(B)
  #extract fossil column so of S1
  S1_fosCols <- S1[, grepl("_MINfos", colnames(S1))] 
  
  ## step 1:
  # find the fossil sectors they are most exposed to directly and add them to the top layer
  
  for (v in V(exp.graph)[layer == depth]$name){
    # for sector v, we extract the corresponding row the S1 and Bt matrix 
    v_S1_fos_row <- S1_fosCols[v,]
    v_Bt_row <- Bt[v,]
    # exclude the fossil sector of the same country
    sect_focus_v <- paste0(substr(v,1,3),"_",sect_focus)
    v_S1_fos_row[sect_focus_v] <- 0
    # take the top sectors
    top.list1 <- head(sort(v_S1_fos_row, decreasing = TRUE), m_top)
    
    #now we add vertices given by the sectors in the ordered top.list (only if those sectors are not yet included)
    for (s in names(top.list1)){ 
      if (! s %in% V(exp.graph)[layer == 0]$name) { 
        exp.graph <- exp.graph + vertex(s, layer = 0)
      }
      #and connect them with edges, given by the direct stranding link in the S1 matrix
      exp.graph <- exp.graph + edge(V(exp.graph)[layer==0][s], V(exp.graph)[layer==depth][v], weight = S1[v,s], color = color_1, length = 1, final = TRUE)
    }
    
    ## step 2: 
    # find the most significant 2-step incoming linkages for exposed sectors, place originating sectors in layer 0 and intermediate sectors in layer 1
    
    # for this, create a matrix of all possible 2-step linkages ending in v, obtained by multiplying the row of v in Bt as a column vector with Bt
    # in this matrix, the elements represent the stranding in v that originates from a unitary shock in the column sectors, then passes on to the row sectors and finally arrives in v
    twostep_strand <- v_Bt_row * Bt * k_int[v]
    # exclude self-loops
    diag(twostep_strand) <- 0  
    # remove the fossil sector of the same country 
    twostep_strand[sect_focus_v,] <- 0
    twostep_strand[,sect_focus_v] <- 0
    # remove the bottom layer itself
    twostep_strand[v,] <- 0
    # constrain the search to stranding channels originating in fossil sectors
    twostep_strand_fos <- twostep_strand[,grepl(sect_focus, colnames(twostep_strand))]
    
    # select top values
    top.list2 <- reshape2::melt(twostep_strand_fos, as.is = TRUE)
    top.list2 <- head(top.list2[order(top.list2$value, decreasing=TRUE),], m_top) %>% # slice_max(top.list2, order_by = value, n = m_top) 
      rename(inter1 = Var1, origin = Var2) %>% relocate (inter1, .after = origin)
    
    # now add vertices: the originating sector is the column sectors of top.list2
    # add it only of the node does not yet exist in this layer
    for (t in top.list2$origin){ 
      if (!t %in% V(exp.graph)[layer == 0]$name) { 
        exp.graph <- exp.graph + vertex(t, layer = 0)
      }
      # the second step after t is given by the row sectors of top.list2 
      # in case t is the originating sector of more than one 2-step linkages, use a for loop
      for(u in top.list2$inter1[top.list2$origin == t]){
        if (!u %in% V(exp.graph)[layer == 1]$name) { 
          exp.graph <- exp.graph + vertex(u, layer = 1) 
        }
        # connect them with originating sectors with weight given by the direct stranding link in the S1 matrix (only if this edge does not yet exist)
        # (the if statement checks if this edge already exists on a 2-step path)
        if (!isTRUE(try(E(exp.graph, P = list(V(exp.graph)[layer==0][t], V(exp.graph)[layer==1][u]))[length == 2] %in% E(exp.graph), silent = T))) { 
          exp.graph <- exp.graph + edge(V(exp.graph)[layer==0][t], V(exp.graph)[layer==1][u], weight = S1[u,t], color = color_2, origin = t, length = 2, dashes = ifelse(are_adjacent(exp.graph,V(exp.graph)[layer==0][t], V(exp.graph)[layer==1][u]),"[15,15]", NA)) }
        # and with exposed sectors at the bottom with weight taken from the twostep_strand matrix (only if this edge with the same path does not yet exist)
        # but only if this edge with the same origin (i.e. the exact same path) does not yet exist (avoid double counting)
        if (!isTRUE(try(E(exp.graph, P = list(V(exp.graph)[layer==1][u], V(exp.graph)[layer==depth][v]))[origin == t][length == 2] %in% E(exp.graph), silent = T))) { 
          exp.graph <- exp.graph + edge(V(exp.graph)[layer==1][u], V(exp.graph)[layer==depth][v], weight = twostep_strand_fos[u,t], color = color_2, origin = t, inter1 = u, length = 2, final = TRUE)}
        }  
      }
    
    
    if (depth == 3){ 
  
    ## step 3: 
    #find the most significant 3-step linkages from the sectors in the top layer to the exposed sectors at the bottom
      
      # generate data.frame to save the m top most important 3-step linkages starting in each fossil sector and ending in v
      top.list3_all <- lapply(colnames(twostep_strand_fos)[colnames(twostep_strand_fos) != sect_focus_v], function(x){
        # create a threestep_strand matrix with originating sector i by multiplying the twostep_strand matrix row-wise with the column of i in Bt
        threestep_strand <- t(t(twostep_strand)*Bt[,paste(x)]) %>% reshape2::melt(as.is = TRUE) 
        threestep_strand <- head(threestep_strand[order(threestep_strand$value, decreasing=TRUE),], m_top) %>% # slice_max(threestep_strand, order_by = value, n = m_top) 
          rename(inter2 = Var1, inter1 = Var2) %>% relocate (inter2, .after = inter1) %>% # reformat columns
          mutate(origin = paste(x)) %>% relocate (origin, .before = inter1) %>%
          return(threestep_strand)}) %>% bind_rows()
      
      # now select top linkages overall: 
      # excluding the MINfos sector of the target country
      top.list3_all$value[top.list3_all$inter1 == sect_focus_v] <- 0 
      top.list3_all$value[top.list3_all$inter2 == sect_focus_v] <- 0 
      # exclude self-loops
      top.list3_all$value[top.list3_all$origin == top.list3_all$inter1] <- 0 
      top.list3_all$value[top.list3_all$inter1 == top.list3_all$inter2] <- 0 
      # select top stranding paths
      top.list3 <- head(top.list3_all[order(top.list3_all$value, decreasing=TRUE),], m_top) # slice_max(top.list3_all, order_by = value, n = m_top)
      
      
      # now add vertices given by of top.list3
      # add it only of the node does not yet exist in this layer
      for (a in top.list3$origin){ 
        if (!a %in% V(exp.graph)[layer == 0]$name) { 
          exp.graph <- exp.graph + vertex(a, layer = 0)
        }
        
        # add the corresponding inter1 sector
        for (b in top.list3$inter1[top.list3$origin == a]){ 
          if (!b %in% V(exp.graph)[layer == 1]$name) { 
            exp.graph <- exp.graph + vertex(b, layer = 1)}
          # add edge (if not already existing)
          # make the edge dashed in case a two-step path with this edge already exists
          if (!isTRUE(try(E(exp.graph, P = list(V(exp.graph)[layer==0][a], V(exp.graph)[layer==1][b]))[length==3] %in% E(exp.graph), silent = T))) { 
            exp.graph <- exp.graph + edge(V(exp.graph)[layer==0][a], V(exp.graph)[layer==1][b], weight = S1[b,a], origin = a, length = 3, color = color_3, dashes = ifelse(are_adjacent(exp.graph,V(exp.graph)[layer==0][a], V(exp.graph)[layer==1][b]),"[15,15]", NA))}
          
          for (c in top.list3$inter2[top.list3$origin == a & top.list3$inter1 == b]){
            if (!c %in% V(exp.graph)[layer == 2]$name) { 
              exp.graph <- exp.graph + vertex(c, layer = 2)}
            # add edge (if not already existing)
            if (!isTRUE(try(E(exp.graph, P = list(V(exp.graph)[layer==1][b], V(exp.graph)[layer==2][c]))[origin == a][length == 3] %in% E(exp.graph), silent = T))) { 
              exp.graph <- exp.graph + edge(V(exp.graph)[layer==1][b], V(exp.graph)[layer==2][c], weight = Bt[b,a]*Bt[c,b]*k_int[c], origin = a, inter1 = b, color = color_3, length = 3)}
            # and with exposed sectors at the bottom with weight taken from the top.list3 vector (only if this edge with the same path does not yet exist)
            if (!isTRUE(try(E(exp.graph, P = list(V(exp.graph)[layer==2][c], V(exp.graph)[layer==depth][v]))[origin == a][inter1 == b][length == 3] %in% E(exp.graph), silent = T))) { 
              exp.graph <- exp.graph + edge(V(exp.graph)[layer==2][c], V(exp.graph)[layer==depth][v], weight = top.list3$value[top.list3$origin == a & top.list3$inter1 == b & top.list3$inter2 == c], color = color_3, origin = a, inter1 = b, inter2 = c, length = 3, final = TRUE)}
          } 
        }
      }
    }
  }
  
  return(exp.graph)
}




# Simplification of plotting chunks -------------------------------------------------------------------------------------------


## function to simplify the plotly barchart lines

plotly_barchart <- function(Rounds_matrix, sector = sect_focus, internal_strand = T, initial_shock = T, top = 10, aggregation = "none", title = "", cols = barchart_cols){
  
  # when internal stranding is not displayed, the initial shock can't be displayed either, so set initial_shock to F and return warning
  if (internal_strand == F & initial_shock == T ) {
    initial_shock <- F
    warning ("initial_shock set to FALSE if internal_strand is defined as FALSE")
  }
  
  Rounds_mat <-  Rounds_matrix
  
  # if internal_strand = F, remove internal stranding (stranding all stranding within the sector of the shock origin)
  if (internal_strand == F) {
    Rounds_mat[grepl(sector, rownames(Rounds_mat)),] <- 0 
  }
  
  # if an aggregation argument is defined, aggregate rows according to it
  # note that an aggregation can only be conduced if the input matrix rows are not already aggregated, so stop if this is not the case
  if (!aggregation=="none" & nrow(Rounds_matrix)<100) {stop("Aggregation can only be conducted if the input matrix rows are not already aggregated and RoW is included")}
  if (aggregation == "country") {
    Rounds_mat <- sum_country %*% Rounds_mat
    rownames(Rounds_mat) <- countries
  }
  if (aggregation == "sector") {
    Rounds_mat <- sum_sec %*% Rounds_mat
    rownames(Rounds_mat) <- sectors 
  }
  
  # extract rounds 0-4 and aggregate remaining difference to the total multipliers to a "Further Rounds" column
  Rounds_plot <- cbind(Rounds_mat[,1:4], "Further_Rounds" = Rounds_mat[,"Total"]- rowSums(Rounds_mat[,1:4]))
  
  # remove ROund 0 if inital_shock = F
  if (initial_shock == F) { Rounds_plot[,1] <- 0 }
  
  # order by total stranding and take top sector as defined by the "top" argument
  Rounds_plot <- as.data.frame(Rounds_plot[order(rowSums(Rounds_plot), decreasing = T),][1:top,])
  
  # plot 
  if (initial_shock == T) { 
    
    plot_ly(Rounds_plot, x = ~Round0, y = rownames(Rounds_plot), type = 'bar', orientation = 'h', name = 'Initial shock', marker = list(color = cols["initial_shock"])) %>%
      add_trace(x = ~Round1, name = 'First round' , marker = list(color = cols["round1"])) %>%
      add_trace(x = ~Round2, name = 'Second round' , marker = list(color = cols["round2"])) %>%
      add_trace(x = ~Round3, name = 'Third round', marker = list(color = cols["round3"])) %>%
      add_trace(x = ~Further_Rounds, name = 'Further Rounds', marker = list(color = cols["further_rounds"])) %>%
      layout(xaxis = list(title = 'Stranding multiplier', titlefont = list(size=15)), 
             yaxis = list(title ="", categoryorder = "array", categoryarray = rev(rownames(Rounds_plot))),
             barmode = 'stack',
             legend = list(x = 0.6, y = 0.1, font = list(size=15)),
             title = list(text = title), font = list(size=10)) 
    
  } else {
    
    plot_ly(Rounds_plot, x = ~Round1, y = rownames(Rounds_plot), type = 'bar', orientation = 'h', name = 'First round', marker = list(color = cols["round1"])) %>%
      add_trace(x = ~Round2, name = 'Second round' , marker = list(color = cols["round2"])) %>%
      add_trace(x = ~Round3, name = 'Third round', marker = list(color = cols["round3"])) %>%
      add_trace(x = ~Further_Rounds, name = 'Further Rounds', marker = list(color = cols["further_rounds"])) %>%
      layout(xaxis = list(title = 'Stranding multiplier', titlefont = list(size=15)), 
             yaxis = list(title ="", categoryorder = "array", categoryarray = rev(rownames(Rounds_plot))),
             barmode = 'stack',
             legend = list(x = 0.6, y = 0.1, font = list(size=15)),
             title = list(text = title), font = list(size=10)) 
  } 
}



## function to contain the visNetwork layout preparation chunk

layout_network <- function(network, type = "standard", strand_rounds, edgelabel = TRUE, edgewidth_factor = 50){
  
  # the type of the input network needs to be defined (either "worldsec", "standard", or "burden_sharing")
  if(is.na(type)) { stop( "the type of the input network needs to be defined (either 'worldsec', 'standard', 'burden_sharing', or 'exposure')" )}
  
  ## transform the igraph object cn into a dataframe for visNetwork
  
  # first, append the layer names to the node names to make sure all node names are unique
  # this necessary because visNetwork only accepts unique node IDs (for the case the same sectors can appear more than once)
  V(network)$name_orig <- V(network)$name # but make a copy of th original name first
  V(network)$name <- paste0(V(network)$name,"~",V(network)$layer)
  # now transform to vis dataframe:  igraph node names are transformed to vis node IDs
  network_vis_dat <- toVisNetworkData(network)
  
  
  # in case the network is an exposure network, make sure there are no duplicate edges and sum up edges of different weight but same type between two nodes
  if (type == "exposure"){
    network_vis_dat$edges <- distinct(network_vis_dat$edges, from, to, length, weight, final, origin, inter1, .keep_all=TRUE)
    network_vis_dat$edges <- network_vis_dat$edges %>% group_by(from, to, length, color, final) %>% 
      summarise(weight = sum(weight), 
                origin = paste(origin, collapse=" & "), inter1 = paste(inter1, collapse=" & "), 
                dashes = ifelse(any(is.na(dashes)), NA, "[15,25]"), .groups = "drop")  
    # also adapt edge label color to edge color (see exposure_network function)
    network_vis_dat$edges$font.color <- network_vis_dat$edges$color
  }
  
  ## modify dataframe for node/edge-specific layout 
  
  # add total stranding from the stranding rounds to each node in the node dataframe
  if (type == "worldsec"){
    for (layer in 0:depth){
      for (sector in sectors){
        network_vis_dat$nodes$totStrand[network_vis_dat$nodes$id==paste0(sector,"~",layer)] <- strand_rounds[[layer+1]][sector,node] 
      }}
  } else if (type == "standard"){
    for (layer in 0:depth){
      for (sector in country_sec){
        network_vis_dat$nodes$totStrand[network_vis_dat$nodes$id==paste0(sector,"~",layer)] <- strand_rounds[[layer+1]][sector,node] 
      }}
  } else if (type == "burden_sharing"){
    # in the burden sharing case, the root node initial shock is the sum of all shocks to the individual MINfos sectors
    network_vis_dat$nodes$totStrand[network_vis_dat$nodes$id==paste0(node,"~",0)] <- sum(Rounds_bs[[shockdist]]$Round0)
    for (layer in 1:depth){
      for (sector in country_sec){
        network_vis_dat$nodes$totStrand[network_vis_dat$nodes$id==paste0(sector,"~",layer)] <- strand_rounds[[layer+1]][sector]
      }}
  # for exposure networks, we only add the total stranding to the exposed sectors on the bottom
  }  else if (type == "exposure"){ 
   for (sector in network_vis_dat$nodes$name_orig[network_vis_dat$nodes$layer == max(network_vis_dat$nodes$layer)]){
       # add the total external exposure to fossil stranding (i.e. only to foreign fossil sectors) to the bottom edges
        sector_Sfos_row <- S_fosCols[sector,] 
        sector_Sfos_row_ext <- sector_Sfos_row[colnames(S_fosCols) != paste0(substr(sector,1,3),"_",sect_focus)]
        network_vis_dat$nodes$totStrand[network_vis_dat$nodes$name_orig == sector & network_vis_dat$nodes$layer == max(network_vis_dat$nodes$layer)] <- sum(sector_Sfos_row_ext)
   }} 
  
  
  # setting node labels: the vis network dataframe can contain a separate "label" column. 
  # --> take the node names and add total stranding per round in separate line
  # --> the labels allow for html (or Markdown) formatting (like <b> for bold), enabling multi-layouts within labels. Note that the visNode command needs the argument "multi = TRUE" for that.
  
  if (type == "worldsec"){
    network_vis_dat$nodes$label <- paste0("<b>", network_vis_dat$nodes$name_orig,"\n", round(network_vis_dat$nodes$totStrand,3))
  } else if (type == "standard") {
    network_vis_dat$nodes$label <- paste0("<b>",sub("_","\n <b>",network_vis_dat$nodes$name_orig),"\n", ifelse(round(network_vis_dat$nodes$totStrand,3)>0, round(network_vis_dat$nodes$totStrand,3), round(network_vis_dat$nodes$totStrand,4)))
  } else if (type == "burden_sharing") {
    network_vis_dat$nodes$label <- paste0("<b>",sub("_","\n <b>",network_vis_dat$nodes$name_orig),"\n", ifelse(round(as.numeric(network_vis_dat$nodes$totStrand),3)>0, round(as.numeric(network_vis_dat$nodes$totStrand),3), round(as.numeric(network_vis_dat$nodes$totStrand),4)))
  } else if (type == "exposure") {
    network_vis_dat$nodes$label <- paste0("<b>",sub("_","\n <b>",network_vis_dat$nodes$name_orig))
    network_vis_dat$nodes$label[!is.na(network_vis_dat$nodes$totStrand)] <- paste0("<b>",sub("_","\n <b>",sub("*~.", "", network_vis_dat$nodes$id[!is.na(network_vis_dat$nodes$totStrand)])), "\n", ifelse(round(as.numeric(network_vis_dat$nodes$totStrand[!is.na(network_vis_dat$nodes$totStrand)]),3)>0, round(as.numeric(network_vis_dat$nodes$totStrand[!is.na(network_vis_dat$nodes$totStrand)]),3), round(as.numeric(network_vis_dat$nodes$totStrand[!is.na(network_vis_dat$nodes$totStrand)]),4)))
  }
  
  
  # add a country column and a corresponding column of country-specific color codes
  if (! type == "worldsec") {
    network_vis_dat$nodes$country <- sub("_.*","",network_vis_dat$nodes$id)
    country_cols <- pal_igv("default", alpha = 0.75)(44) # colors from the ggsci package
    names(country_cols) <- countries
    network_vis_dat$nodes$color <- country_cols[network_vis_dat$nodes$country]
  }
  # in the case of burden_sharing, color the top node separately
  if (type == "burden_sharing") {network_vis_dat$nodes$color[1] <- "firebrick"}
  
  # add a column containing the horizontal order of nodes in each layer (this corrects a bug in the visLayout algorithm)
  x<-c()
  for(lay in 0:(length(unique(V(network)$layer))-1)) {layorder<-1:length(V(network)[layer==lay]); x<-c(x,layorder)}
  network_vis_dat$nodes$x <- x
  
  # add stranding weights as edge labels and make egde with proportional to it
  if (edgelabel == TRUE){
    network_vis_dat$edges$label <- ifelse(round(network_vis_dat$edges$weight,3)>0, round(network_vis_dat$edges$weight,3), ifelse(round(network_vis_dat$edges$weight,4)>0, round(network_vis_dat$edges$weight,4), round(network_vis_dat$edges$weight,5)))
    network_vis_dat$edges$label[network_vis_dat$edges$label == 0] <- NA
    # for exposure networks, we only want edge labels for final edges
    if (type == "exposure"){network_vis_dat$edges$label[is.na(network_vis_dat$edges$final)] <- NA}
  }
  network_vis_dat$edges$width <- 3 + network_vis_dat$edges$weight * edgewidth_factor
  network_vis_dat$nodes$level <- network_vis_dat$nodes$layer
  
  return(network_vis_dat)
}


## simple function to contain the visNetwork plotting chunk

plot_network <- function(network, title = "", node_background = "lightgrey", node_border = "grey", edge_color = "grey", node_name_color = "black", node_value_color = "darkblue", edge_vale_color = "seagreen", stroke_color = "white", highlight_color = "gold", node_dist = 160, node_height = 90, layer_sep = 240, physics = TRUE) {
  
  visNetwork(network$nodes,network$edges, main = list(text= title, style ="font-family:Georgia;font-size:16px;text-align:center")) %>%
    visHierarchicalLayout(levelSeparation=layer_sep,
                          #nodeSpacing=250, #taken from visPhysics argument below
                          blockShifting = T,
                          edgeMinimization = T,
                          parentCentralization = T,
                          sortMethod="directed"
    ) %>%
    visNodes(shape = "box",
             #size = 200, # only applicable for shapes which have labels outside (like "dot")
             #fixed = list(x=T,y=T), # this would fix nodes in their position
             font = list(color = node_value_color, 
                         size = 22, face = "arial",  vadjust = - 5,
                         multi = T, bold = list( color = node_name_color, size = 28, vadjust = -5),
                         background = "rgba(220,220,220,0.7)"
                         #strokeWidth = 5
             ), # strokeColor = "rgba(220,220,220,0.9)"
             
             color = list(         
               background = node_background, 
               border = node_border,
               highlight = highlight_color),
             borderWidth = 2.5,
             heightConstraint = node_height, 
             widthConstraint = 80, # default undefined -> useful if all boxes should have same width
             margin = 3, # default 5
             mass = 1, # default 1: sets how fixed a node is, can be used to shift nodes manually
             physics=physics
    ) %>%
    visEdges(#arrows = "to",
      smooth = list(type = "cubicBezier", forceDirection="vertical", roundness = 0.5),
      color = list(color = edge_color, highlight = highlight_color),
      font = list(color = edge_vale_color, size =18, strokeWidth = 6, strokeColor = stroke_color, align="middle", bold=T),
      #physics = F
    )%>%
    visPhysics(hierarchicalRepulsion = list(avoidOverlap = 1, nodeDistance = node_dist, centralGravity = 0), stabilization = T) %>%
    visOptions(manipulation = F) %>%
    visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}")
}



