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
int<lower=1> K;                	// number of features
matrix[N,K] Y;                 	// data matrix of order [N,K]
matrix[N,K] Y_sd;				// standard deviations of Y

// model selection
int<lower=1> L;              	// number of latent dimensions
int shared_latent_rho_fixed;	// rho = 0: stand norm, rho = 1: ICAR
int specific_latent_rho_fixed;	// rho = 0: stand norm, rho = 1: ICAR


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
transformed data {
// vectorised versions of input data
vector[N*K] Y_v = to_vector(Y);
vector[N*K] Y_sd_v = to_vector(Y_sd);
// number of non-zero entries
int<lower=1> M;       
M <- L*(K-L)+ L*(L-1)/2;   
}
parameters {    
	// mean vector
	vector[K] alpha;
	
	// feature-specific
	matrix[N,K] Z_epsilon;						// standard normal latent factors
	vector<lower=0>[K] sigma; 					// vector of std
	vector<lower=0,upper=0.99>[K] rho_epsilon;	// SA parameter for feature-specific
	
	// Loading matrix
	vector[M] Lambda_ld;   						// lower diagonal elements of Lambda
	
	// shared
	matrix[N,L] Z_z;							// standard normal latent factors
	vector<lower=0>[L] psi;						//  vector of std
	vector<lower=0,upper=0.99>[L] rho_z; 		// SA parameter for shared
}
transformed parameters{
	cholesky_factor_cov[K,L] Lambda;  	// factor loadings matrix 
	matrix[N,L] z;   					// shared latent factors
	matrix[N,K] epsilon;   			  	// feature-specific latent factors
	matrix[N, K] mu;					// mean vector
	{
	int idx1 = 0;
	int idx2 = 0; 
	real zero; 
	zero <- 0;
	for(i in 1:K){
		for(j in (i+1):L){
		  idx1 <- idx1 + 1;
		  Lambda[i,j] <- zero; 			// constrain the upper triangular elements to zero 
		}
	}
	for (j in 1:L) {
		Lambda[j,j] <- 1.0; 			// constrain the diagonal elements to zero 
		for (i in (j+1):K) {
		  idx2 <- idx2 + 1;
		  Lambda[i,j] <- Lambda_ld[idx2];
		} 
	}
	}

	// non-mean centered parameterisation 
	// for shared latent factors
	for(l in 1:L) z[,l] = Z_z[,l] * psi[l];
	
	for(k in 1:K){
		// non-mean centered parameterisation
		// for feature-specific latent factors
		epsilon[,k] = Z_epsilon[,k] * sigma[k];
		// mean vector
		mu[,k] = alpha[k] + z*Lambda[k,]' + epsilon[,k];
	}
}
model {
	// generic priors
	Lambda_ld ~ std_normal();
	alpha ~ std_normal();
	
	// variance priors
	sigma ~ gamma(2,1); // 0.0047 of the density is below 0.1
	psi ~ std_normal();
	
	// spatial autocorrelation priors
	rho_z ~ uniform( 0,1 ); 
	rho_epsilon ~ uniform( 0,1 );
	
	// shared latent factors - unit scale
	for(l in 1:L){
		if(shared_latent_rho_fixed == 1)
			target += ICAR_lpdf( Z_z[,l] | N, node1, node2 ); 
		else if(shared_latent_rho_fixed == 0)
			target += std_normal_lpdf( Z_z[,l] ); 
		else{
			target += LCAR_lpdf( Z_z[,l] | rho_z[l], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N ); 
			target += normal_lpdf( sum(Z_z[,l]) | 0, 0.001 * N );
		}
	}
	
	// feature specific latent factors - unit scale
	for(k in 1:K){
		if(specific_latent_rho_fixed == 1)
			target += ICAR_lpdf( Z_epsilon[,k] | N, node1, node2 ); 
		else if(specific_latent_rho_fixed == 0)
			target += std_normal_lpdf( Z_epsilon[,k] ); 
		else{
			target += LCAR_lpdf( Z_epsilon[,k] | rho_epsilon[k], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
			target += normal_lpdf( sum(Z_epsilon[,k]) | 0, 0.001 * N );
		}
	}
	
	// Likelihood - measurement error model
	Y_v ~ normal( to_vector(mu), Y_sd_v );
}
generated quantities {
	real log_lik[N*K];
	{
		for (nk in 1:N*K){
			log_lik[nk] = normal_lpdf( Y_v[nk] | to_vector(mu)[nk], Y_sd_v[nk]);
		}
	}
}


