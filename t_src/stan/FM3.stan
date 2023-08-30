data {
  int<lower=1> N;                // number of observations
  int<lower=1> K;                // number of features
  matrix[N,K] Y;                 // data matrix of order [N,K]
  matrix[N,K] sd_mat;		// standard deviations of Y
  int<lower=1> L;              // number of latent dimensions 
}
transformed data {
	int<lower=1> M;
	M  = L*(K-L)+ L*(L-1)/2;  // number of non-zero loadings
	vector[N*K] Y_v = to_vector(Y);
	vector[N*K] sd_mat_v = to_vector(sd_mat);
}
parameters {    
	vector[M] Lambda_t;   		  // lower diagonal elements of Lambda
	vector<lower=0>[L] Lambda_d;    // diagonal elements of Lambda
	vector<lower=0>[K] psi;         // vector of variances
	vector[K] alpha;       // vector of means
	matrix[N,L] z;
	matrix[N,K] mu;
}
transformed parameters{
  cholesky_factor_cov[K,L] Lambda;  //lower triangular factor loadings Matrix 
{
  int idx1 = 0;
  int idx2 = 0;
  real zero = 0; 
  for(k in 1:K){
    for(l in (k+1):L){
      idx1 = idx1 + 1;
      Lambda[k,l] = zero; //constrain the upper triangular elements to zero 
    }
  }
  for (l in 1:L) {
      Lambda[l,l] = 1.0; //Lambda_d[l]; // diagonal elements
    for (k in (l+1):K) {
      idx2 = idx2 + 1;
      Lambda[k,l] = Lambda_t[idx2];
    } 
  }
} 
}
model {
// the priors 
	Lambda_d ~ std_normal();
	Lambda_t ~ std_normal();
	psi ~ std_normal();
	alpha ~ std_normal();
// latent
	to_vector(z) ~ std_normal();	
//The likelihood
	Y_v ~ normal( to_vector(mu), sd_mat_v );
	for(n in 1:N){
		for(k in 1:K){
			mu[n,k] ~ normal( alpha[k] + Lambda[k,] * z[n,]', psi[k] ); 	
		}
	}
	// Y[j] ~ multi_normal(mu,Q);
		
}