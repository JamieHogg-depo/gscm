data {
	int<lower=0> N; 						// number of areas
	int<lower=0> K;							// number of features
	matrix[N,K] Y; 							// total number of areas
}
parameters {
	vector[K] alpha;
	vector[N] fi;
	vector[K-1] lambda_n;
	vector<lower=0>[K] Sigma;
}
transformed parameters{
	vector[K] lambda;
	lambda[1:K-1] = lambda_n;
	lambda[K] = 0 - sum(lambda_n);
}
model{
	for(n in 1:N){
		Y[n,] ~ normal(alpha + lambda * fi[n], Sigma);
	}
	
	fi ~ std_normal();
	alpha ~ std_normal();
	lambda_n ~ std_normal();
	//sum(fi) ~ normal(0,0.001);
	Sigma ~ std_normal();
}






