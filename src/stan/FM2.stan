data {
  int<lower=1> N;                // number of observations
  int<lower=1> K;                // number of features
  matrix[N,K] Y;                 // data matrix of order [N,K]
  int<lower=1> L;              // number of latent dimensions 
}
transformed data {
  int<lower=1> M;
  vector[K] mu;
  M  = L*(K-L)+ L*(L-1)/2;  // number of non-zero loadings
}
parameters {    
  vector[M] Lambda_t;   // lower diagonal elements of Lambda
  vector<lower=0>[L] Lambda_d;   // lower diagonal elements of Lambda
  vector<lower=0>[K] psi;         // vector of variances
  real<lower=0>   mu_psi;
  real<lower=0>  sigma_psi;
  real mu_lt;
  real<lower=0>  sigma_lt;
  matrix[N,L] z;
}
transformed parameters{
  cholesky_factor_cov[K,L] Lambda;  //lower triangular factor loadings Matrix 
  cov_matrix[K] Q;   //Covariance mat
{
  int idx1 = 0;
  int idx2 = 0;
  real zero = 0; 
  for(i in 1:K){
    for(j in (i+1):L){
      idx1 = idx1 + 1;
      Lambda[i,j] = zero; //constrain the upper triangular elements to zero 
    }
  }
  for (j in 1:L) {
      Lambda[j,j] = Lambda_d[j];
    for (i in (j+1):K) {
      idx2 = idx2 + 1;
      Lambda[i,j] = Lambda_t[idx2];
    } 
  }
} 
Q = Lambda*Lambda'+diag_matrix(psi); 
}
model {
// the hyperpriors 
	mu_psi ~ cauchy(0, 1);
	sigma_psi ~ cauchy(0,1);
	mu_lt ~ cauchy(0, 1);
	sigma_lt ~ cauchy(0,1);
// the priors 
	Lambda_d ~ cauchy(0,3);
	Lambda_t ~ cauchy(mu_lt,sigma_lt);
	psi ~ cauchy(mu_psi,sigma_psi);
// latent
	to_vector(z) ~ std_normal();	
//The likelihood
	for( j in 1:N)
	// Y[j] ~ multi_normal(mu,Q);
		Y[j] ~ multi_normal( Lambda * z[j]', diag_matrix(psi) ); 	
}