data {
	int<lower=0> N; 						// number of areas
	int<lower=0> K;							// number of features
	matrix[N,K] Y; 							// total number of areas
}
parameters {
	vector[K] alpha;
	vector[N] fi;
	vector[K] lambda;
	vector<lower=0>[K] Sigma;
}
model{
	for(n in 1:N){
		Y[n,] ~ normal(alpha + lambda * fi[n], Sigma);
	}
	
	fi ~ std_normal();
	alpha ~ std_normal();
	lambda ~ std_normal();
	sum(fi) ~ normal(0,0.001);
	Sigma ~ std_normal();
}






