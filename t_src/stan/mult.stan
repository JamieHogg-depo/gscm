data {
	int<lower=1> K;
	int<lower=1> N;
	matrix[N,K] y; 
	matrix[N,K] y_sd;
	vector[K] zero;
	vector<lower=0>[K] L_sigma;
}
parameters {
	cholesky_factor_corr[K] L_Omega;
	matrix[N, K] mu;
}
transformed parameters{
	matrix[K, K] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
}
model{
	L_Omega ~ lkj_corr_cholesky(4);
	for(n in 1:N){
		for(k in 1:K){
			y[n,k] ~ normal( mu[n,k], y_sd[n,k] );
		}
		mu[n,] ~ multi_normal_cholesky(zero, L_Sigma);	
	}
}
generated quantities {
	matrix[K, K] cc = L_Sigma * L_Sigma';
	vector[N] log_lik;
	for(n in 1:N) log_lik[n] = multi_normal_prec_lpdf( y[n,] | mu[n,], diag_matrix( 1 ./ to_vector( square(y_sd[n,]) ) ) );
}

