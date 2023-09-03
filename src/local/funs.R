# funs
jf <- list()

## ----------------------------------------------------------------------------
jf$norm = function(x){
	(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

## ----------------------------------------------------------------------------
#' @param W binary contiguity matrix (must be complete)
#' @description similar to mungeCARdata function
# requires spdep
jf$prep4ICAR = function(W){

	BUGS_format <- listw2WB(mat2listw(W))

	N = length(BUGS_format$num);
	nn = BUGS_format$num;
	N_edges = length(BUGS_format$adj) / 2;
	node1 = vector(mode="numeric", length=N_edges);
	node2 = vector(mode="numeric", length=N_edges);
	iAdj = 0;
	iEdge = 0;
	for (i in 1:N) {
	for (j in 1:nn[i]) {
	  iAdj = iAdj + 1;
	  if (i < BUGS_format$adj[iAdj]) {
		iEdge = iEdge + 1;
		node1[iEdge] = i;
		node2[iEdge] = BUGS_format$adj[iAdj];
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

## -----------------------------------------------------------------------------
jf$scaleMarginal <- function(mat){
  # get column means
  cMeans <- apply(mat, 2, mean)
  col_n <- colnames(mat)
  mat_sc <- scale(as.numeric(as.matrix(mat)))
  # create new matrix
  out <- as.data.frame(matrix(mat_sc,nrow = nrow(mat),ncol = ncol(mat)))
  names(out) <- col_n
  # return dataframe
  return(out)
}

## -----------------------------------------------------------------------------
#' @param draws iterations by observations
#' @param model model column content (as single character)
#' @param metric metric column content (as single character)
#' @param conf_level numeric for the confidence size of the interval (defaults to 0.95)
#' @param other_data data.frame with other data to be column binded to result 
# (NOTE: must be same dimensions and same order!)
#' @param prefix character vector added to point, upper, lower, se and RSE columns (defaults to "")
#' @param addDPP logical (defaults to false)
#' @returns Dataset with posterior summaries including: 
# median point estimates, credible intervals (HDI)
# standard deviations and RSE
# More arguments can be passed to the getDPP() function using `...` (e.g. null_value)
jf$getResultsData <- function(draws, 
                           model = NULL, metric = NULL,
                           prefix = "",
                           conf_level = 0.95,
                           other_data = NULL, addDPP = FALSE, ...){
  
  # sum_func <- function(x){
  #   c(point = median(x, na.rm = T),
  #     lower = unname(HDInterval::hdi(x, credMass = conf_level)[1]),
  #     upper = unname(HDInterval::hdi(x, credMass = conf_level)[2]),
  #     se = sd(x, na.rm = T))
  # }
  # bind_rows(pblapply(asplit(draws, 2), sum_func))
  
  if(!is.null(other_data)){
    message(paste0("NOTE (not an error): Please check that the row order of ", deparse(substitute(other_data)), " matches that of the column order of ", deparse(substitute(draws))))
    if(nrow(other_data) != ncol(draws))stop(paste0("The number of columns of ", deparse(substitute(draws)), " does NOT match the number of rows of ", deparse(substitute(other_data)), ". They must match!"))
  }
  
  if(is.null(dim(draws))){
    r <- data.frame(point = draws,
                    lower = NA,
                    upper = NA,
                    se = NA) %>%
      mutate(RSE = NA) %>%
      setNames(paste0(prefix, names(.)))
    r <- bind_cols(r, other_data)
  }else{
    # Get objects
    point_in = apply(draws, 2, median, na.rm = T)
    se_in = apply(draws, 2, sd, na.rm = T)
    hd_ints = apply(draws, 2, HDInterval::hdi, credMass = conf_level)
    if(addDPP){
      DPP <- jf$getDPP(draws, ...)
      r <- data.frame(point = point_in,
                      #lower = apply(draws, 2, quantile, prob = 0.025, na.rm = T),
                      lower = hd_ints[1,],
                      #upper = apply(draws, 2, quantile, prob = 0.975, na.rm = T),
                      upper = hd_ints[2,],
                      se = se_in,
                      EP = DPP$EP,
                      compared_to_null = DPP$compared_to_null,
                      DPP = DPP$DPP,
                      DPPsig = DPP$DPP_sig) %>%
        mutate(RSE = 100 * (se/abs(point))) %>%
        setNames(paste0(prefix, names(.)))
      r <- bind_cols(r, other_data)
    }else{
      r <- data.frame(point = point_in,
                      #lower = apply(draws, 2, quantile, prob = 0.025, na.rm = T),
                      lower = hd_ints[1,],
                      #upper = apply(draws, 2, quantile, prob = 0.975, na.rm = T),
                      upper = hd_ints[2,],
                      se = se_in) %>%
        mutate(RSE = 100 * (se/abs(point))) %>%
        setNames(paste0(prefix, names(.)))
      r <- bind_cols(r, other_data)
    }
  }
  
  # Add columns if given
  if(!is.null(model) & !is.null(metric)){
    r <- r %>% 
      mutate(model = model,
             metric = metric) %>% 
      relocate(model, metric)
  }
  if(!is.null(model) & is.null(metric)){
    r <- r %>% 
      mutate(model = model) %>% 
      relocate(model)
  }
  if(is.null(model) & !is.null(metric)){
    r <- r %>% 
      mutate(metric = metric) %>% 
      relocate(metric)
  }
  
  # return objects
  rownames(r) <- NULL
  return(r)
}

## -----------------------------------------------------------------------------
#' @param summary summary list object from nimble or CB run (`fit$summary`)
#' @param regex character expression for the param subset. For vector parameters
# use `regex = "B_qr\\["`.
# To subset multiple parameters use `regex = "alpha|beta"`
# Using `-` at the start of the regex to remove those rows
#' @param set character vector for the set column
jf$getSubsetConvergence <- function(summary, regex, set){
  
  if(str_sub(regex,1,1) == "-"){
    regex = str_sub(regex, 2)
    summ <- summary %>% 
      filter(!str_detect(variable, regex))
  }else{
    summ <- summary %>% 
      filter(str_detect(variable, regex))
  }
  
  out <- data.frame(set = set,
                    n_params = nrow(summ),
                    perc_Rhatgr1.02 = 100*mean(summ$rhat > 1.02, na.rm = T),
                    max_Rhat = max(summ$rhat, na.rm = T),
                    perc_ebbelow400 = 100*mean(summ$ess_bulk < 400, na.rm = T),
                    perc_etbelow400 = 100*mean(summ$ess_tail < 400, na.rm = T),
                    min_essbulk = min(summ$ess_bulk, na.rm = T),
                    min_esstail = min(summ$ess_tail, na.rm = T))
  
  # output list
  return(out)
}

## -----------------------------------------------------------------------------
#' @import spdep and igraph
#' @param sf_data sf data with geometry and 5-digit sa2 codes
#' @param sa2_name character vector specifying the variable with 5digit sa2 codes. Default is "Sa2_5dig16". 
#' @param .forsa3 logical (defaults to FALSE) if fixing sa3 map
#' @return list with nb list, binary weight matrix and group membership list
jf$getConnectedNB <- function(sf_data, 
							sa2_name = "Sa2_5dig16", 
							.forsa3 = FALSE){

require(spdep)
require(igraph)

# create joining list
joining_list <- list(to_join = c(21088, 21091, 31527,
                                 31363, 31402, 31466, 
                                 31483, 41103, 41145, 61093, 
                                 71060, 71062, 61099),
                     join2 = list(21308, 21379, 31013, 31362,
                                  31401, 31483, c(31469,31466),
                                  41100, 41128, c(61094, 21476),
                                  71021, 71063, c(61100, 21092)))

if(.forsa3){
	joining_list <- list(to_join = c(60403, 60203),
						 join2 = c(20503, 21703))
}

# get temporary nb object
nb_original<- poly2nb(sf_data)
nb_temp <- nb_original

# get sf_data ids which we wish to mutate
id_to_join <- which(unlist(sf_data[,sa2_name] %>% st_drop_geometry()) %in% as.character(joining_list$to_join))

# if none to change then proceed with mutation of nb list
if(!is_empty(id_to_join)){
  for(i in 1:length(id_to_join)){
    # find index join2
    id_join2 <- which(unlist(sf_data[,sa2_name] %>% st_drop_geometry()) %in% as.character(unlist(joining_list$join2[i])))
    # update singles
    nb_temp[[id_to_join[i]]] <- as.integer(c(nb_temp[[id_to_join[i]]], id_join2))
    if(!is_empty(-which(nb_temp[[id_to_join[i]]] == 0))){
      # remove zeros
      nb_temp[[id_to_join[i]]] <- nb_temp[[id_to_join[i]]][-which(nb_temp[[id_to_join[i]]] == 0)]
    }
  } 
}

# ensure nb object is symmetric
nb_out <- make.sym.nb(nb_temp)

# check connectedness
W <- nb2mat(nb_out, style = "B", zero.policy = TRUE)
gg <- graph.adjacency(W)
clu <- components(gg)
cc <- igraph::groups(clu)
message("There are ", length(cc), " unique groups of neighbours!")

# return the nb object
return(list(nb = nb_out, 
            W = W,
            group_membership = cc))

}

## -----------------------------------------------------------------------------
#' @import spdep and igraph
#' @param sf_data sf data with geometry and 5-digit sa2 codes
#' @param sa2_name character vector specifying the variable with 5digit sa2 codes. Default is "Sa2_5dig16". 
#' @param .forsa3 logical (defaults to FALSE) if fixing sa3 map
#' @return list with nb list, binary weight matrix and group membership list
jf$getConnectedNB2 <- function(sf_data, 
                            sa2_name = "Sa2_5dig16", 
                            .forsa3 = FALSE){
  
  require(spdep)
  require(igraph)
  
  # create joining list
  joining_list <- list(to_join = c(21088, 21091, 31527,
                                   31363, 31402, 31466, 
                                   31483, 41145, 61093, 
                                   71060, 71062, 61099),
                       join2 = list(21308, 21379, 31013, 
                                    31362, 31401, 31483, 
                                    c(31469,31466), 41128, c(61094, 21476),
                                    71021, 71063, c(61100, 21092)))
  
  if(.forsa3){
    joining_list <- list(to_join = c(60403, 60203),
                         join2 = c(20503, 21703))
  }
  
  
  # get temporary nb object
  nb_original<- poly2nb(sf_data)
  nb_temp <- nb_original
  
  # get correct ordering from which
  jwhich <- function(full, sub){
    out <- as.character(NA)
    for(i in 1:length(sub)){
      temp <- unname(unlist(which(full == as.character(sub[i]))))
      out[i] <- ifelse(length(temp) == 0, NA, temp)
    }
    return(out)
  }
  
  # get sf_data ids which we wish to mutate
  sa2_names_vec <- unlist(sf_data[,sa2_name] %>% st_drop_geometry())
  id_to_join <- as.integer(jwhich(sa2_names_vec, joining_list$to_join))
  #id_to_join <- which(sa2_names_vec %in% as.character(joining_list$to_join))
  
  # if none to change then proceed with mutation of nb list
  if(!is_empty(id_to_join)){
    for(i in 1:length(id_to_join)){
      # find index join2
      id_join2 <- which(sa2_names_vec %in% as.character(unlist(joining_list$join2[i])))
      # update singles
      newlist <- as.integer(c(nb_temp[[id_to_join[i]]], id_join2))
      nb_temp[[id_to_join[i]]] <- newlist[!duplicated(newlist)]
      if(!is_empty(-which(nb_temp[[id_to_join[i]]] == 0))){
        # remove zeros
        nb_temp[[id_to_join[i]]] <- nb_temp[[id_to_join[i]]][-which(nb_temp[[id_to_join[i]]] == 0)]
      }
    } 
  }
  
  # ensure nb object is symmetric
  nb_out <- make.sym.nb(nb_temp)
  
  # check connectedness
  W <- nb2mat(nb_out, style = "B", zero.policy = TRUE)
  gg <- graph.adjacency(W)
  clu <- components(gg)
  cc <- igraph::groups(clu)
  message("There are ", length(cc), " unique groups of neighbours!")
  
  # return the nb object
  return(list(nb = nb_out, 
              W = W,
              group_membership = cc))
  
}

## ----------------------------------------------------------------------------
jf$connectW <- function(sf_data){
  
  # get original W
  W <- nb2mat(poly2nb(sf_data), style = "B", zero.policy = TRUE)
  
  if(!any(names(sf_data) %in% "Sa2_name16")){
    message("Must have column called Sa2_name16")
    break
  }

# set changes 
new_assigns <- data.frame(area1 = c("King Island", "King Island",
                                    "Flinders and Cape Barren Islands", "Flinders and Cape Barren Islands",
                                    "Bribie Island",
                                    "Palm Island",
                                    "Magnetic Island",
                                    "Redland Islands",
                                    "Phillip Island",
                                    "French Island",
                                    "Kangaroo Island",
                                    "Tiwi Islands",
                                    "Anindilyakwa",
                                    "Torres Strait Islands"),
                          area2 = c("Otway", "North West",
                                    "Wilsons Promontory", "Scottsdale - Bridport",
                                    "Beachmere - Sandstone Point",
                                    "Ingham Region",
                                    "Belgian Gardens - Pallarenda",
                                    "Jacobs Well - Alberton",
                                    "Wonthaggi - Inverloch",
                                    "Wonthaggi - Inverloch",
                                    "Yankalilla",
                                    "Koolpinyah",
                                    "Gulf",
                                    "Torres"))

W_working <- W
for(i in 1:nrow(new_assigns)){
  W_working[sf_data$Sa2_name16 == new_assigns$area1[i],sf_data$Sa2_name16 == new_assigns$area2[i]] <- 1
  W_working[sf_data$Sa2_name16 == new_assigns$area2[i],sf_data$Sa2_name16 == new_assigns$area1[i]] <- 1
}

# check connectedness
gg <- igraph::graph.adjacency(W_working)
clu <- igraph::components(gg)
cc <- igraph::groups(clu)
message("There are ", length(cc), " unique groups of neighbours!")

# return the nb object
return(list(W = W_working,
            group_membership = cc))

}

