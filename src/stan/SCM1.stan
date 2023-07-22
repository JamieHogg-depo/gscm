data{
	int M; // sample size
	matrix[M,2] Y; // data matrix of order [N,S]
	matrix[M,2] sd_mat;
}
parameters{
	//Parameters
	vector[2] alpha; 		// intercepts
	real log_lambda; 		// loadings
	vector[M] z; 
	//matrix[M, 2] Z_e; 		// standard normals for feature-specific
	vector[M] Z_e1; 
	vector[M] Z_e2;
	//Hyperparameters
	real<lower=0> sigma_e1; 
	real<lower=0> sigma_e2; 
	// vector<lower=0>[2] sigma_e;
	real<lower=0> sigma_z;
}
transformed parameters{
	matrix[M, 2] mu;
	real lambda = exp(log_lambda);
	mu[,1] = alpha[1] + lambda*z*sigma_z + Z_e1 * sigma_e1;
	//mu[,1] = alpha[1] + lambda*z*sigma_z + Z_e[,1] * sigma_e1;
	mu[,2] = alpha[2] + (1/lambda)*z*sigma_z + Z_e2 * sigma_e2;
	//mu[,2] = alpha[2] + (1/lambda)*z*sigma_z + Z_e[,2] * sigma_e2;
	// lambda[1:2] = log_lambda;
	// lambda[3] = - sum(log_lambda);
	
}
model{
	// Hyperpriors
	sigma_e1 ~ std_normal(); 
	sigma_e2 ~ std_normal();
	sigma_z ~ std_normal();
	// Priors
	alpha ~ std_normal();
	z ~ std_normal();
	Z_e1 ~ std_normal();
	Z_e2 ~ std_normal();
	log_lambda ~ normal( 0,sqrt(0.412) ); // based on P0375
	// Likelihood
		to_vector(Y) ~ normal( to_vector(mu), to_vector(sd_mat) );
}


