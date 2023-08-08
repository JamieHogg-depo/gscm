data { //https://rfarouni.github.io/assets/projects/BayesianFactorAnalysis/BayesianFactorAnalysis.html
  int<lower=1> N;                // number of 
  int<lower=1> K;                // number of 
  matrix[N,K] Y;                 // data matrix of order [N,K]
  int<lower=1> L;              	// number of latent dimensions 
}
transformed data {
  int<lower=1> M;
  M  <- L*(K-L)+ L*(L-1)/2;  // number of non-zero loadings
  vector[L] zero_v = rep_vector(0.0, L); 
}
parameters {    
  vector[M] Lambda_t;   // lower diagonal elements of Lambda
  vector<lower=0>[L] Lambda_d;   // lower diagonal elements of Lambda
  vector<lower=0>[K] psi;         // vector of variances
  vector[K] alpha;
  matrix[N,L] z;
  vector<lower=0>[L] sigma_z;
}
transformed parameters{
  cholesky_factor_cov[K,L] Lambda;  //lower triangular factor loadings Matrix 
  cov_matrix[K] Q;   //Covariance mat
{
  int idx1 = 0;
  int idx2 = 0; 
  real zero; 
  zero <- 0;
  for(i in 1:K){
    for(j in (i+1):L){
      idx1 <- idx1 + 1;
      Lambda[i,j] <- zero; //constrain the upper triangular elements to zero 
    }
  }
  for (j in 1:L) {
      Lambda[j,j] <- 1.0; //Lambda_d[j];
    for (i in (j+1):K) {
      idx2 <- idx2 + 1;
      Lambda[i,j] <- Lambda_t[idx2];
    } 
  }
} 
Q = Lambda * Lambda' + diag_matrix(psi); 
}
model {
	// the priors 
	Lambda_d ~ std_normal();
	Lambda_t ~ std_normal();
	psi ~ std_normal();
	sigma_z ~ std_normal();
	alpha ~ std_normal();
	//for(l in 1:L){
	//	z[,l] ~ normal( 0, sigma_z[l] );
	//}
	//to_vector(z) ~ std_normal();
	//The likelihood
	for(n in 1:N){
		//Y[j] ~ multi_normal( alpha, Q );
		z[n] ~ multi_normal( zero_v, diag_matrix(sigma_z) ); 
		for(k in 1:K) Y[n,k] ~ normal( alpha[k] + Lambda[k,] * z[n]', psi[k] );
	}
}


