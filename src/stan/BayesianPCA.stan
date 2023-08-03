data{ // https://www.cs.helsinki.fi/u/sakaya/tutorial/code/pca.R
	int<lower=0> N; // Number of samples
	int<lower=0> K; // The original dimension
	int<lower=0> L; // The latent dimension
	matrix[N, K] Y; // The data matrix
}
parameters{
	matrix[N, L] Z; // The latent matrix
	matrix[K, L] W; // The weight matrix
	real<lower=0> tau; // Noise term 
	vector<lower=0>[L] alpha; // ARD prior
}
transformed parameters{
	vector<lower=0>[L] t_alpha;
	real<lower=0> t_tau;
	t_alpha = inv(sqrt(alpha));
	t_tau = inv(sqrt(tau));
}
model {
	tau ~ gamma(1,1);			
	to_vector(Z) ~ normal(0,1);
	alpha ~ gamma(1e-3,1e-3);				
	for(l in 1:L) W[,l] ~ normal(0, t_alpha[l]);
	to_vector(Y) ~ normal(to_vector(Z*W'), t_tau);
}

