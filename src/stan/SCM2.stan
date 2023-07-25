functions{
/**
* Log probability density of the leroux conditional autoregressive (LCAR) model
* @param x vector of random effects
* @param rho spatial dependence parameter
* @param sigma standard deviation of LCAR
* @param C_w Sparse representation of C
* @param C_v Column indices for values in C
* @param C_u Row starting indices for values in C
* @param offD_id_C_w indexes for off diagonal terms
* @param D_id_C_w indexes for diagonal terms - length M
* @param C_eigenvalues eigenvalues for C
* @param M number of areas
**
@return Log probability density
*/
real LCAR_lpdf(
    vector x,               
    real rho,                   
    real sigma,              
    vector C_w , 
    int [] C_v , 
    int [] C_u , 
    int [] offD_id_C_w ,        
    int [] D_id_C_w ,       
    vector C_eigenvalues,       
    int M                   
    ) {                 
        vector[M] ldet_C;
        vector [ num_elements(C_w) ] ImrhoC;
        vector[M] A_S;
        // Multiple off-diagonal elements by rho
        ImrhoC [ offD_id_C_w ] = - rho * C_w[ offD_id_C_w ];
        // Calculate diagonal elements of ImrhoC
        ImrhoC [ D_id_C_w ] = 1 - rho * C_w[ D_id_C_w ];
        A_S = csr_matrix_times_vector( M, M, ImrhoC, C_v, C_u, x );
        ldet_C = log1m( rho * C_eigenvalues );
        return -0.5 * ( 
        M*log( 2 * pi() ) 
        - ( M * log(1/square(sigma)) + sum( ldet_C ) ) 
        + 1/square(sigma) * dot_product(x, A_S) 
        );
}
}
data{
	int N; 					// sample size
	int K;					// Number of features
	matrix[N,K] Y; 			// data matrix of order [N,S]
	matrix[N,K] sd_mat;		// standard deviations of Y
}
transformed data{
	vector[N*K] Y_v = to_vector(Y);
	vector[N*K] sd_mat_v = to_vector(sd_mat);
}
parameters{
	//Parameters
	vector[K] alpha; 				// intercepts
	vector[K-1] log_lambda; 		// loadings
	vector[N] Z_z; 					// shared component
	matrix[N, K] Z_epsilon; 		// feature-specific component
	//Hyperparameters
	vector<lower=0>[K] sigma_e;		// sd for feature-specifc
	real<lower=0> sigma_z;			// sd for shared component
}
transformed parameters{
	matrix[N, K] mu;
	vector[K] lambda;
	matrix[N, K] epsilon;
	vector[N] z = Z_z*sigma_z;
	lambda[1:K-1] = exp(log_lambda); // sum_k log(lambda) = 0
	lambda[K] = exp( - sum(log_lambda) );
	for(k in 1:K){
		epsilon[,k] = Z_epsilon[,k] * sigma_e[k];
		mu[,k] = alpha[k] + lambda[k]*z + epsilon[,k];
	}
}
model{
	// Hyperpriors
		sigma_e ~ std_normal(); 
		sigma_z ~ std_normal();
	// Priors
		alpha ~ std_normal();
		Z_z ~ std_normal();
		// `to_vector(.)` stacks by columns
		to_vector(Z_epsilon) ~ std_normal();
		log_lambda ~ normal( 0,sqrt(0.412) ); // based on P0375
	// Likelihood
		Y_v ~ normal( to_vector(mu), sd_mat_v );
}
generated quantities {
  real log_lik[N*K];
  {
	vector[N*K] mu_v = to_vector(mu);
    for (nk in 1:N*K){
		log_lik[nk] = normal_lpdf( Y_v[nk] | mu_v[nk], sd_mat_v[nk]);
    }
  }
}


