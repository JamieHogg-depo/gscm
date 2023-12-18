# Indexes vs SEIFA

quant1 <- jf$getResultsData(t(apply(latent_draws[,,1], 1, ggplot2::cut_number, n = 10, labels = FALSE)))
quant2 <- jf$getResultsData(t(apply(latent_draws[,,2], 1, ggplot2::cut_number, n = 10, labels = FALSE)))
quant3 <- jf$getResultsData(t(apply(z_comb, 1, ggplot2::cut_number, n = 10, labels = FALSE)))
quant4 <- jf$getResultsData(t(apply(z_erp_comb, 1, ggplot2::cut_number, n = 10, labels = FALSE)))

# Accuracy
sum(diag(table(as.factor(11-as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant1$point)))/2221
sum(diag(table(as.factor(11-as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant2$point)))/2221
sum(diag(table(as.factor(11-as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant3$point)))/2221
sum(diag(table(as.factor(11-as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant4$point)))/2221

# Accuracy
sum(diag(table(as.factor(as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant1$point)))/2221
sum(diag(table(as.factor(as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant2$point)))/2221
sum(diag(table(as.factor(as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant3$point)))/2221
sum(diag(table(as.factor(as.numeric(cur_list$data$census$ABS_irsd_decile_nation_complete)), quant4$point)))/2221

summary(lm(cur_list$summ_latent1$raww$point ~ cur_list$data$census$ra_sa2))$adj.r.squared
summary(lm(cur_list$summ_latent2$raww$point ~ cur_list$data$census$ra_sa2))$adj.r.squared
summary(lm(cur_list$summ_latent3$raww$point ~ cur_list$data$census$ra_sa2))$adj.r.squared
summary(lm(cur_list$summ_latent4$raww$point ~ cur_list$data$census$ra_sa2))$adj.r.squared

