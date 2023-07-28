# funs
jf <- list()

## ----------------------------------------------------------------------------
#' @param adjBUGS (data$adj)
#' @param numBUGS (data$num)
mungeCARdata4stan = function(adjBUGS,numBUGS) {
  N = length(numBUGS);
  nn = numBUGS;
  N_edges = length(adjBUGS) / 2;
  node1 = vector(mode="numeric", length=N_edges);
  node2 = vector(mode="numeric", length=N_edges);
  iAdj = 0;
  iEdge = 0;
  for (i in 1:N) {
    for (j in 1:nn[i]) {
      iAdj = iAdj + 1;
      if (i < adjBUGS[iAdj]) {
        iEdge = iEdge + 1;
        node1[iEdge] = i;
        node2[iEdge] = adjBUGS[iAdj];
      }
    }
  }
  return (list("N"=N,"N_edges"=N_edges,"node1"=node1,"node2"=node2));
}


## ----------------------------------------------------------------------------
#' @param W binary contiguity matrix (must be complete)
jf$prep4MLCAR <- function(W){    
    # create sparse matrices
    W <- Matrix::Matrix(W, sparse = TRUE)
    D <- Matrix::Diagonal( x = Matrix::rowSums(W) )
    I <- Matrix::Diagonal(nrow(W))
    C <- I - D + W
    # C and W only differ by the diagonal values
    # C has -1 on off diagonals
    
    # ISSUE: Diagonal element of C is zero if area has only one neighbor
    
    # get indices for diagonals
    jt <- rstan::extract_sparse_parts(W + 5*I) # 5 is arbritary
    # 5's will only be on the diagonals
    D_id_C_w <- which(jt$w == 5) 
    # any values that are not 5 are off diagonals
    offD_id_C_w <- which(jt$w == 1)
    
    # Eigenvalues of C
    C_eigenvalues <- eigen(C)$values
    
    # get the CRS representation of C
    # add an extra 1 to all diagonals to ensure they
    # are captured by `extract_sparse_parts`
    crs <- rstan::extract_sparse_parts(C + I)
    nC_w <- length(crs$w)
    
    # Remove 1 from the diagonals 
    crs$w[D_id_C_w] <- crs$w[D_id_C_w] - 1
    
    # prepare output list
    return(
      list(C = as.matrix(C),
           C_eigenvalues = C_eigenvalues, 
           nC_w = nC_w,
           C_w = crs$w,
           C_v = crs$v,
           C_u = crs$u,
           D_id_C_w = D_id_C_w,
           offD_id_C_w = offD_id_C_w)
    )
}