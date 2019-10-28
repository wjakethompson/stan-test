data {
  int I;
  int J;
  int N;
  int C;
  int A;
  int<lower=1,upper=I> ii[N];
  int<lower=1,upper=J> jj[N];
  int<lower=0,upper=1> y[N];
  int<lower=1,upper=N> s[J];
  int<lower=1,upper=I> l[J]; 
  matrix[C,A] Alpha;
}
parameters {
  simplex[C] Vc;
  
  real intercept[I];
  real<lower=0> maineffect[I];
}
transformed parameters {
  matrix[I,C] pi;
  
  for (i in 1:I) {
    for (c in 1:C) {
      pi[i,c] = inv_logit(intercept[i] + ((c - 1) * maineffect[c]));
    }
  }
}
model {
  vector[C] contributionsC;
  // vector[I] contributionsI;

  intercept ~ normal(0, 15);
  maineffect ~ normal(0, 15);
  Vc ~ dirichlet(rep_vector(1.0, C));
  
  for (j in 1:J) {
    for (c in 1:C) {
      vector[l[j]] contributionsI;
      for (m in 1:l[j]) {
        int i = ii[s[j] + m - 1];
        contributionsI[m] = bernoulli_lpmf(y[s[j] + m - 1] | pi[i,c]);
      }
      contributionsC[c] = log(Vc[c]) + sum(contributionsI);
    }
    target += log_sum_exp(contributionsC);
  }
}

