functions{
/**
* Log probability density of the leroux conditional autoregressive (LCAR) model
* @param x vector of random effects
* @param rho spatial dependence parameter
* @param sigma standard deviation
* @param C_w Sparse representation of C
* @param C_v Column indices for values in C
* @param C_u Row starting indices for values in C
* @param offD_id_C_w indexes for off diagonal terms
* @param D_id_C_w indexes for diagonal terms - length M
* @param C_eigenvalues eigenvalues for C
* @param N number of areas
**
@return Log probability density
**
To use: LCAR_lpdf( x | rho, sigma, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
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
    int N                   
    ) {                 
        vector[N] ldet_C;
        vector [ num_elements(C_w) ] ImrhoC;
        vector[N] A_S;
        // Multiple off-diagonal elements by rho
        ImrhoC [ offD_id_C_w ] = - rho * C_w[ offD_id_C_w ];
        // Calculate diagonal elements of ImrhoC
        ImrhoC [ D_id_C_w ] = 1 - rho * C_w[ D_id_C_w ];
        A_S = csr_matrix_times_vector( N, N, ImrhoC, C_v, C_u, x );
        ldet_C = log1m( rho * C_eigenvalues );
        return -0.5 * ( 
        N*log( 2 * pi() ) 
        - ( N * log(1/square(sigma)) + sum( ldet_C ) ) 
        + 1/square(sigma) * dot_product(x, A_S) 
        );
}
/** 
 Log probability density of the icar conditional autoregressive (ICAR) model
 Using 2019 paper `Bayesian Hierarchical Spatial Models: Implementing the 
 Besag York Mollié Model in Stan` implementation
`
* @param x vector of random effects
* @param N number of areas
**
@return Log probability density
**
To use: ICAR_lpdf( x | N, node1, node2 );
*/
real ICAR_lpdf(vector x, int N, int[] node1, int[] node2) {
  return -0.5 * dot_self(x[node1] - x[node2]) 
  + normal_lpdf(sum(x) | 0, 0.001 * N);
}
}

data {
// data
	int<lower=1> N;                	// number of observations
	vector[N] Y;				// vectorised data matrix of order [N,K] 
	vector[N] Y_sd;				// vectorised standard deviations of Y

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
parameters {    
	// shared
	vector[N] z;								// standard normal latent factors
	real<lower=0> psi;							//  vector of std
	real<lower=0,upper=0.99> rho; 				// SA parameter for shared
}
model {
	// variance priors
	psi ~ gamma( 2,1 ); // gamma( 2, 1 ) -> 0.0047 of the density is below 0.1
	
	// spatial autocorrelation priors
	rho ~ uniform( 0, 0.99 ); 
	
	// shared latent factors - unit scale
	target += LCAR_lpdf( z | rho, 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N ); 
	target += normal_lpdf( sum(z) | 0, 0.001 * N );
	//target += student_t_lpdf( z | 2, 0, 1);
	
	// Likelihood - measurement error model
	Y ~ normal( psi*z, Y_sd );
}
generated quantities {
	real log_lik[N];
	vector[N] yrep;
	for (n in 1:N){
		yrep[n] = normal_rng( psi*z[n], Y_sd[n] ); 
		log_lik[n] = normal_lpdf( Y[n] | psi*z[n], Y_sd[n] );
	}
}


