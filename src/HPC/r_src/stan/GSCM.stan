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
/**
* Create constrained factor loading matrix
* @param K number of input features
* @param L number of latent factors
* @param Lambda_d diagonal elements of Lambda
* @param Lambda_ld lower diagonal elements of Lambda
* @param latent_var_fixed whether diagonals should be 1 or just positive
*/
matrix FL(int K, int L, vector Lambda_d, vector Lambda_ld, int latent_var_fixed){
	matrix[K,L] Lambda;
	int idx1 = 0;
	int idx2 = 0; 
	real zero; 
	zero = 0;
	for(i in 1:K){
		for(j in (i+1):L){
		  idx1 = idx1 + 1;
		  Lambda[i,j] = zero; 			// constrain the upper triangular elements to zero 
		}
	}
	for (j in 1:L) {
		if(latent_var_fixed == 1) Lambda[j,j] = Lambda_d[j];
		if(latent_var_fixed == 0) Lambda[j,j] = 1.0; 				// constrain the diagonal elements to zero
		for (i in (j+1):K) {
		  idx2 = idx2 + 1;
		  Lambda[i,j] = Lambda_ld[idx2];
		} 
	}
	return Lambda;
}
}

data {
// data
	int<lower=1> N;                	// number of observations
	int<lower=1> K;                	// number of features
	int<lower=1> M;					// number of lower diagonal values in Lambda
	vector[N*K] Y_v;				// vectorised data matrix of order [N,K] 
	vector[N*K] Y_sd_v;				// vectorised standard deviations of Y

// model specification
	int<lower=1> L;              	// number of latent dimensions
	int shared_latent_rho_fixed;	// rho = 0: stand norm, rho = 1: ICAR
	int specific_latent_rho_fixed;	// rho = 0: stand norm, rho = 1: ICAR
	real<lower=0> me0_std; 			// standard deviation of likelihood when me = 0
	int me;							// me = 1: including Y_sd, me = 0: use me0_std
	int beta_sa_prior; 				// 1: use beta priors, 0: uniform prior
	int gamma_var_prior; 			// 1: use gamma priors, 0: normal prior
	real<lower=0> gamma_a;			// a parameter for gamma priors on std
	real<lower=0> gamma_b;			// b parameter for gamma priors on std
	int latent_var_fixed; 			// If 1 then variance of latent factors is 1
	int scale_data;					// If 1 then input data is mean zero and alpha is dropped
	vector[K] sp_ind;				// vector that includes or excludes specific-random effects

// Spatial components for Leroux prior
	vector[N] C_eigenvalues;
	int nC_w;
	vector[nC_w] C_w; 
	int C_v[nC_w]; 
	int C_u[N+1]; 
	int offD_id_C_w[nC_w - N];		// indexes for off diagonal terms
	int D_id_C_w[N]; 				// indexes for diagonal terms - length M
	vector[K] kappa_flag; 			// indicator for which specific prior for each element
	// kappa_flag = -1 -> ICAR 
	// kappa_flag = -0.5 -> LCAR with estimated SA parameter
	// kappa_flag = 0 -> IID prior
	// kappa_flag > 0 -> fixed SA parameter

// Spatial components for ICAR prior
	int<lower=0> N_edges;
	int<lower=1, upper=N> node1[N_edges]; 
	int<lower=1, upper=N> node2[N_edges];
}
//transformed data {
// number of non-zero entries
//	int<lower=1> M;       
//	M = L*(K-L)+ L*(L-1)/2;   
//}
parameters {    
	// mean vector
	vector[K] alpha;
	//vector<lower=0>[N*K] chi_draw;
	
	// feature-specific
	matrix[N,K] Z_epsilon;						// standard normal latent factors
	vector<lower=0>[K] sigma; 					// vector of std
	vector<lower=0,upper=0.99>[K] kappa;		// SA parameter for feature-specific
	
	// Loading matrix
	vector[M] Lambda_ld;   						// lower diagonal elements of Lambda
	vector<lower=0>[L] Lambda_d;   				// diagonal elements of Lambda
	
	// shared
	matrix[N,L] Z_z;							// standard normal latent factors
	vector<lower=0>[L] psi;						//  vector of std
	vector<lower=0,upper=0.99>[L] rho; 			// SA parameter for shared
}
transformed parameters{
	matrix[N,L] z;   							// shared latent factors
	matrix[N,K] epsilon;   			  			// feature-specific latent factors
	matrix[N, K] mu;							// mean vector
	
	// factor loadings matrix
	cholesky_factor_cov[K,L] Lambda = FL(K, L, Lambda_d, Lambda_ld, latent_var_fixed); 

	// data uncertainty
	//vector[N*K] d_sd = sqrt( (2 * square(Y_sd_v)) ./ chi_draw );

	// non-mean centered parameterisation 
	// for shared latent factors
	if(latent_var_fixed == 1){
		for(l in 1:L) z[,l] = Z_z[,l]; // set latent variance to 1
	}
	else{
		for(l in 1:L) z[,l] = Z_z[,l] * psi[l];
	}
	
	// non-mean centered parameterisation 
	// for feature-specific factors
	for(k in 1:K){
		// non-mean centered parameterisation
		// for feature-specific latent factors
		epsilon[,k] = sp_ind[k] * Z_epsilon[,k] * sigma[k];
		// mean vector
		if(scale_data == 1) mu[,k] = z*Lambda[k,]' + epsilon[,k];
		if(scale_data == 0) mu[,k] = alpha[k] + z*Lambda[k,]' + epsilon[,k];
	}
}
model {
	// generic priors
	target += std_normal_lpdf( Lambda_ld );
	target += std_normal_lpdf( Lambda_d );
	target += std_normal_lpdf( alpha );
	//chi_draw ~ chi_square(2);
	
	// variance priors
	if(gamma_var_prior == 1){
		target += gamma_lpdf( sigma | gamma_a, gamma_b ); // gamma( 2, 1 ) -> 0.0047 of the density is below 0.1 -> 0.74 above 1
		target += gamma_lpdf( psi | gamma_a, gamma_b );	// gamma( 2,3 ) -> 0.037 of the density is below 0.1 -> 0.2 above 1
	}
	if(gamma_var_prior == 0){
		target += std_normal_lpdf( sigma );
		target += std_normal_lpdf( psi );
	}
	
	// spatial autocorrelation priors
	if(beta_sa_prior == 1){
		target += beta_lpdf(rho | 6,2); // 0.9375 of density above 0.5
		target += beta_lpdf(kappa | 6,2);
	}
	if(beta_sa_prior == 0){
		target += uniform_lpdf( rho | 0, 0.99 ); 
		target += uniform_lpdf( kappa | 0, 0.99 );
	}
	
	// shared latent factors - unit scale
	for(l in 1:L){
		if(shared_latent_rho_fixed == 1)
			target += ICAR_lpdf( Z_z[,l] | N, node1, node2 ); 
		else if(shared_latent_rho_fixed == 0){
			target += std_normal_lpdf( Z_z[,l] ); 
		}
		else{
			target += LCAR_lpdf( Z_z[,l] | rho[l], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N ); 
			target += normal_lpdf( sum(Z_z[,l]) | 0, 0.001 * N );
		}
	}
	
	// feature specific latent factors - unit scale
	for(k in 1:K){
		if(kappa_flag[k] == -1)
			target += ICAR_lpdf( Z_epsilon[,k] | N, node1, node2 ); 
		else if(kappa_flag[k] == 0)
			target += std_normal_lpdf( Z_epsilon[,k] );
		else if(kappa_flag[k] == -0.5){
			target += LCAR_lpdf( Z_epsilon[,k] | kappa[k], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
			target += normal_lpdf( sum(Z_epsilon[,k]) | 0, 0.001 * N );
		}
		else{
			target += LCAR_lpdf( Z_epsilon[,k] | kappa_flag[k], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
			target += normal_lpdf( sum(Z_epsilon[,k]) | 0, 0.001 * N );
		}
		
/* 		if(specific_latent_rho_fixed == 1)
			
		else if(specific_latent_rho_fixed == 0){
			target += std_normal_lpdf( Z_epsilon[,k] ); 
		}
		else if(specific_latent_rho_fixed == 2){
			target += LCAR_lpdf( Z_epsilon[,k] | kappa[k], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
			target += normal_lpdf( sum(Z_epsilon[,k]) | 0, 0.001 * N );
		}
		else{
			if(kappa_fixed[k] == -1){
				target += LCAR_lpdf( Z_epsilon[,k] | kappa[k], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
				target += normal_lpdf( sum(Z_epsilon[,k]) | 0, 0.001 * N );
			}
			if(kappa_fixed[k] != -1){
				target += LCAR_lpdf( Z_epsilon[,k] | kappa_fixed[k], 1, C_w, C_v, C_u, offD_id_C_w, D_id_C_w, C_eigenvalues, N );
				target += normal_lpdf( sum(Z_epsilon[,k]) | 0, 0.001 * N );
			}
		} */
	}
	
	// Likelihood - measurement error model
	if(me == 1){
		target += normal_lpdf( Y_v | to_vector(mu), Y_sd_v );
	}
	if(me == 0){
		target += normal_lpdf( Y_v | to_vector(mu), me0_std );
	}
}
generated quantities {
	real log_lik[N*K];
	matrix[N,K] yrep;
	if(me == 1){
		yrep = to_matrix( normal_rng( to_vector(mu), Y_sd_v ), N, K );
	}
	if(me == 0){ 
		yrep = to_matrix( normal_rng( to_vector(mu), me0_std ), N, K );
	}
	{	
		for (nk in 1:N*K){
			if(me == 1) log_lik[nk] = normal_lpdf( Y_v[nk] | to_vector(mu)[nk], Y_sd_v[nk] );
			if(me == 0) log_lik[nk] = normal_lpdf( Y_v[nk] | to_vector(mu)[nk], me0_std );
		}
	}
}


