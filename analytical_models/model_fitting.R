fit_naive_aggregate_model <- function(analyst_df) {
  analyst_df %>%
    mutate(price_bin = round(offered_price)) %>%
    group_by(price_bin) %>%
    summarize(
      purchase_rate = mean(purchase),
      .groups = "drop"
    )
}

fit_misspecified_model <- function(analyst_df) {
  model <- lm(
    purchase ~ offered_price,
    data = analyst_df
  )

  list(
    model = model,
    alpha_hat = unname(coef(model)[["(Intercept)"]]),
    beta_hat = unname(coef(model)[["offered_price"]])
  )
}

summarize_customer_conversion <- function(analyst_df) {
  analyst_df %>%
    group_by(customer_id) %>%
    summarize(
      observed_conversion = mean(purchase),
      n = n(),
      .groups = "drop"
    )
}

hierarchical_demand_model_string <- function() {
  "
model {

  for (i in 1:N_obs) {

    purchase[i] ~ dbern(p[i])

    logit(p[i]) <-
      alpha[customer[i]] +
      beta[customer[i]] * price[i]
  }

  for (j in 1:N_customers) {

    alpha[j] ~ dnorm(mu_alpha, tau_alpha)

    beta[j] ~ dnorm(mu_beta, tau_beta)
  }

  mu_alpha ~ dnorm(0,0.01)
  mu_beta ~ dnorm(0,0.01)

  sigma_alpha ~ dunif(0,5)
  sigma_beta ~ dunif(0,5)

  tau_alpha <- pow(sigma_alpha,-2)
  tau_beta <- pow(sigma_beta,-2)

}
"
}

fit_hierarchical_bayesian_model <- function(
    analyst_df,
    N_customers = length(unique(analyst_df$customer_id)),
    model_string = hierarchical_demand_model_string(),
    n.chains = 4,
    n.adapt = 2000,
    n.update = 4000,
    n.iter = 8000) {
  jags_data <- list(
    purchase = analyst_df$purchase,
    price = analyst_df$offered_price,
    customer = analyst_df$customer_id,
    N_obs = nrow(analyst_df),
    N_customers = N_customers
  )

  jags_model <- jags.model(
    textConnection(model_string),
    data = jags_data,
    n.chains = n.chains,
    n.adapt = n.adapt
  )

  update(jags_model, n.update)

  posterior_samples <- coda.samples(
    jags_model,
    variable.names = c(
      "mu_alpha",
      "mu_beta"
    ),
    n.iter = n.iter
  )

  posterior_matrix <- as.matrix(posterior_samples)

  mu_alpha_post <- mean(
    posterior_matrix[, "mu_alpha"]
  )

  mu_beta_post <- mean(
    posterior_matrix[, "mu_beta"]
  )

  list(
    jags_data = jags_data,
    jags_model = jags_model,
    posterior_samples = posterior_samples,
    posterior_matrix = posterior_matrix,
    mu_alpha_post = mu_alpha_post,
    mu_beta_post = mu_beta_post
  )
}

summarize_inferred_parameters <- function(
    analyst_df,
    bayes_fit,
    misspecified_fit) {
  pooled_fit <- glm(
    purchase ~ offered_price,
    data = analyst_df,
    family = binomial()
  )

  tibble(
    model = c(
      "Naive Aggregate",
      "Hierarchical Bayesian",
      "Misspecified Linear"
    ),
    inferred_alpha = c(
      unname(coef(pooled_fit)[["(Intercept)"]]),
      bayes_fit$mu_alpha_post,
      misspecified_fit$alpha_hat
    ),
    inferred_beta = c(
      unname(coef(pooled_fit)[["offered_price"]]),
      bayes_fit$mu_beta_post,
      misspecified_fit$beta_hat
    )
  )
}
