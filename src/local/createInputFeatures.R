# create Input Features

pt <- cbind(cur_list$data$map$Sa2_main16, cur_list$data$y) %>% 
  setNames(c("SA2_2016", 
          "Risky alcohol consumption",
          "Current smoking", 
          'Inadequate physical activity', 
          "Inadequate diet", 
          "Overweight/obese"))

sd <- cbind(cur_list$data$map$Sa2_main16, cur_list$data$y_sd) %>% 
  setNames(c("SA2_2016", 
             "Risky alcohol consumption SD",
             "Current smoking SD", 
             'Inadequate physical activity SD', 
             "Inadequate diet SD", 
             "Overweight/obese SD"))

# Output to csv
left_join(pt, sd) %>% 
  write.csv(., "InputFeatures.csv")

