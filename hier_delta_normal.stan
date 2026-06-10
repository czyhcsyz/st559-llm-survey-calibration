data {
  int<lower=1> J;
  vector[J] delta_hat;
  vector<lower=0>[J] se_delta;
  int<lower=1, upper=J> target_idx;
  real llm_gap_target;
}

parameters {
  real mu;
  real<lower=0> sigma_delta;
  vector[J] z;
}

transformed parameters {
  vector[J] delta;
  delta = mu + sigma_delta * z;
}

model {
  mu ~ normal(0, 10);
  sigma_delta ~ normal(0, 10);
  z ~ normal(0, 1);
  delta_hat ~ normal(delta, se_delta);
}

generated quantities {
  vector[J] delta_hat_rep;
  real psi_target;

  for (j in 1:J) {
    delta_hat_rep[j] = normal_rng(delta[j], se_delta[j]);
  }
  psi_target = llm_gap_target + delta[target_idx];
}
