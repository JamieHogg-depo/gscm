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
**
To use: LCAR_lpdf( x | rho, sigma, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, M );
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
real ICAR_lpdf(vector phi, int N, int[] node1, int[] node2) {
  return -0.5 * dot_self(phi[node1] - phi[node2]) 
  + normal_lpdf(sum(phi) | 0, 0.001 * N);
}
}
data{
	int N; 					// sample size
	int K;					// Number of features
	matrix[N,K] Y; 			// data matrix of order [N,S]
	matrix[N,K] sd_mat;		// standard deviations of Y
	
	// Spatial components for Leroux prior
	vector[N] C_eigenvalues;
	int nC_w;
	vector[nC_w] C_w; 
	int C_v[nC_w]; 
	int C_u[N+1]; 
	int offD_id_C_w[nC_w - N];		// indexes for off diagonal terms
	int D_id_C_w[N]; 				// indexes for diagonal terms - length M
	
	// Spatial components for ICAR prior
	int<lower=0> N_edges;
	int<lower=1, upper=N> node1[N_edges]; 
	int<lower=1, upper=N> node2[N_edges];
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
	vector<lower=0,upper=1>[K+1] rho;
}
transformed parameters{
	matrix[N, K] mu;
	//matrix[N, K] unit_scale_residual;
	vector[K] lambda;
	matrix[N, K] epsilon;
	vector[N] z = Z_z*sigma_z;
	lambda[1:K-1] = exp(log_lambda); // sum_k log(lambda) = 0
	lambda[K] = exp( - sum(log_lambda) );
	for(k in 1:K){
		epsilon[,k] = Z_epsilon[,k] * sigma_e[k];
		mu[,k] = alpha[k] + lambda[k]*z + epsilon[,k];
		//unit_scale_residual[,k] = (Y[,k] - alpha[k] - lambda[k]*z)/sigma_e[k];
	}
}
model{ // `to_vector(.)` stacks by columns
	// Hyperpriors
		sigma_e ~ std_normal(); 
		sigma_z ~ std_normal();
		rho ~ std_normal();
		
	// Priors
		alpha ~ std_normal();
		log_lambda ~ normal( 0,sqrt(0.412) ); // based on P0375
		
	// Latent variables
		// Z_z ~ std_normal();
		// Z_z ~ LCAR( rho[K+1], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
		Z_z ~ ICAR( N, node1, node2);
		//for(k in 1:K){
			// Z_epsilon[,k] ~ LCAR( rho[k], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
			//Z_epsilon[,k] ~ ICAR( N, node1, node2);
		//}
		to_vector(Z_epsilon) ~ std_normal();
		// to_vector(unit_scale_residual) ~ std_normal();
		
	// Likelihood
		Y_v ~ normal( to_vector(mu), sd_mat_v );
		//for(k in 1:K){
		//	Y[,k] ~ normal( alpha[k] + lambda[k]*z, sigma_e[k] );
		//}
		
}
generated quantities {
	real log_lik[N*K];
	//matrix[N,K] out;
	{
		vector[N*K] mu_v = to_vector(mu);
		for (nk in 1:N*K){
			log_lik[nk] = normal_lpdf( Y_v[nk] | mu_v[nk], sd_mat_v[nk]);
		}
	}
	//for(k in 1:K){
		//out[,k] = (mu[,k] - alpha[k] - lambda[k]*z)/sigma_e[k]; // EXACTLY equal to Z_epsilon
		// with sd_mat_v = 0, then mu = Y
		// replace mu with Y in above
	//}
}


