# create Product

## function ## -----------------------------------------------------------------
custom_sort <- function(names) {
  # Split the column names into parts
  parts <- strsplit(names, "_")
  
  # Extract the main identifier (e.g., Index1, Scores1)
  identifiers <- sapply(parts, function(x) x[1])
  
  # Extract the suffix (e.g., point, lower)
  suffixes <- sapply(parts, function(x) ifelse(length(x) > 1, paste(x[-1], collapse = "_"), ""))
  
  # Define the order of suffixes
  suffix_order <- c("point", "lower", "upper", 
                    "Prob_PercentileAbove80", 
                    "Prob_PercentileAbove95", 
                    "Prob_PercentileAbove99")
  
  # Create a factor with levels in the desired order
  suffixes_factor <- factor(suffixes, levels = suffix_order)
  
  # Order by identifiers and then by suffixes
  ordered <- order(identifiers, suffixes_factor)
  
  names[ordered]
}
## -----------------------------------------------------------------------------

# scores
foo <- function(x){
cur_list[[paste0("summ_latent", x)]]$raww %>% 
  dplyr::select(point, lower, upper) %>% 
  mutate(type = paste0("Scores", x),
         SA2_2016 = cur_list$data$census$SA2) %>% 
  relocate(SA2_2016, type)
}

scores <- bind_rows(lapply(1:4, foo))

# indices
foo <- function(x){
  cur_list[[paste0("summ_latent", x)]]$perc %>% 
    dplyr::select(point, lower, upper) %>% 
    mutate(type = paste0("Index", x),
           SA2_2016 = cur_list$data$census$SA2) %>% 
    relocate(SA2_2016, type)
}
index <- bind_rows(lapply(1:4, foo))

# probabilities
foo <- function(x){
data.frame(Prob_PercentileAbove80 = cur_list$probs[[paste0("latent", x)]]$perc80,
           Prob_PercentileAbove95 = cur_list$probs[[paste0("latent", x)]]$perc95,
           Prob_PercentileAbove99 = cur_list$probs[[paste0("latent", x)]]$perc99,
           type = paste0("Index", x),
           SA2_2016 = cur_list$data$census$SA2) 
    
}
probs <- bind_rows(lapply(1:4, foo))%>% 
    pivot_wider(names_from = type, 
                values_from = c(Prob_PercentileAbove80,
                                Prob_PercentileAbove95,
                                Prob_PercentileAbove99),
                names_glue = "{type}_{.value}")

# combine
Product <- bind_rows(scores,index) %>% 
  pivot_wider(names_from = type, 
              values_from = c(point, lower, upper),
              names_glue = "{type}_{.value}") %>% 
  left_join(.,probs) %>% 
  dplyr::select("SA2_2016", all_of(custom_sort(names(.))))

# Output to csv
Product %>% 
  write.csv(., "HDCIAProduct.csv")

