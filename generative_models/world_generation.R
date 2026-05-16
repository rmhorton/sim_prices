default_segment_proportions <- function() {
  c(
    Budget = 0.5,
    Mainstream = 0.35,
    Premium = 0.15
  )
}

default_price_elasticity_parameters <- function() {
  list(
    Budget = list(alpha_mean = 0.8, alpha_sd = 0.5, beta_mean = -0.85, beta_sd = 0.12),
    Mainstream = list(alpha_mean = 1.6, alpha_sd = 0.5, beta_mean = -0.45, beta_sd = 0.10),
    Premium = list(alpha_mean = 2.5, alpha_sd = 0.5, beta_mean = -0.15, beta_sd = 0.08)
  )
}

default_lifetime_value_parameters <- function() {
  list(
    Budget = list(mean = 100, sd = 20),
    Mainstream = list(mean = 350, sd = 40),
    Premium = list(mean = 1200, sd = 150)
  )
}

default_promotional_effects <- function() {
  list(
    enabled = TRUE,
    probability = 0.2,
    logit_lift_mean = 0.25,
    logit_lift_sd = 0.10
  )
}

segment_parameter <- function(segment, params, field, default = 0) {
  value <- params[[segment]][[field]]

  if (is.null(value)) {
    default
  } else {
    value
  }
}

generate_true_demand_curve <- function(customer_df, price_grid) {
  crossing(
    customer_id = customer_df$customer_id,
    price = price_grid
  ) %>%
    left_join(customer_df, by = "customer_id") %>%
    mutate(
      purchase_probability = plogis(true_alpha + true_beta * price)
    )
}

generate_customer_world <- function(
    N_customers = 400,
    seed = 2028,
    segment_proportions = default_segment_proportions(),
    price_elasticity_parameters = default_price_elasticity_parameters(),
    lifetime_value_parameters = default_lifetime_value_parameters()) {
  set.seed(seed)

  segment_proportions <- unlist(segment_proportions)
  segment_names <- names(segment_proportions)

  customer_df <- tibble(
    customer_id = 1:N_customers
  )

  customer_df <- customer_df %>%
    mutate(
      latent_segment =
        sample(
          segment_names,
          N_customers,
          replace = TRUE,
          prob = segment_proportions
        )
    )

  customer_df <- customer_df %>%
    rowwise() %>%
    mutate(
      true_alpha = rnorm(
        1,
        segment_parameter(latent_segment, price_elasticity_parameters, "alpha_mean"),
        segment_parameter(latent_segment, price_elasticity_parameters, "alpha_sd")
      ),
      true_beta = rnorm(
        1,
        segment_parameter(latent_segment, price_elasticity_parameters, "beta_mean"),
        segment_parameter(latent_segment, price_elasticity_parameters, "beta_sd")
      ),
      true_willingness_to_pay = if_else(
        true_beta == 0,
        Inf,
        -true_alpha / true_beta
      ),
      lifetime_value = rnorm(
        1,
        segment_parameter(latent_segment, lifetime_value_parameters, "mean", 100),
        segment_parameter(latent_segment, lifetime_value_parameters, "sd", 20)
      )
    ) %>%
    ungroup()

  customer_df
}

simulate_purchase_opportunities <- function(
    customer_df,
    N_offers = 8,
    offer_price_range = list(min = 2, max = 30),
    inventory_limit = Inf,
    noise_levels = list(price_noise_sd = 0, purchase_noise_sd = 0),
    demand_shock_settings = list(
      enabled = FALSE,
      probability = 0,
      logit_shift_mean = 0,
      logit_shift_sd = 0
    ),
    promotional_effects = default_promotional_effects()) {
  offer_df <- crossing(
    customer_id = customer_df$customer_id,
    offer_id = 1:N_offers
  )

  offer_df <- offer_df %>%
    left_join(customer_df, by = "customer_id")

  offer_df <- offer_df %>%
    mutate(
      offered_price =
        round(
          pmax(
            0.01,
            runif(n(), offer_price_range$min, offer_price_range$max) +
              rnorm(n(), 0, noise_levels$price_noise_sd)
          ),
          2
        )
    )

  offer_df <- offer_df %>%
    mutate(
      promotion_active =
        promotional_effects$enabled &
          runif(n()) < promotional_effects$probability,

      promotional_logit_lift =
        if_else(
          promotion_active,
          rnorm(
            n(),
            promotional_effects$logit_lift_mean,
            promotional_effects$logit_lift_sd
          ),
          0
        ),

      demand_shock =
        if_else(
          demand_shock_settings$enabled &
            runif(n()) < demand_shock_settings$probability,
          rnorm(
            n(),
            demand_shock_settings$logit_shift_mean,
            demand_shock_settings$logit_shift_sd
          ),
          0
        ),

      logit_p =
        true_alpha +
        true_beta * offered_price +
        demand_shock +
        promotional_logit_lift,

      purchase_probability =
        pmin(
          1,
          pmax(
            0,
            plogis(logit_p) +
              rnorm(n(), 0, noise_levels$purchase_noise_sd)
          )
        )
    )

  offer_df <- offer_df %>%
    mutate(
      purchase =
        rbinom(
          n(),
          1,
          purchase_probability
        )
    )

  offer_df <- offer_df %>%
    arrange(offer_id, customer_id) %>%
    mutate(
      inventory_start = inventory_limit,
      cumulative_sales_before = lag(cumsum(purchase), default = 0),
      inventory_remaining_before = pmax(inventory_start - cumulative_sales_before, 0),
      fulfilled_purchase = if_else(inventory_remaining_before > 0, purchase, 0),
      inventory_remaining_after = pmax(
        inventory_remaining_before - fulfilled_purchase,
        0
      )
    )

  offer_df
}

observe_analyst_data <- function(offer_df) {
  offer_df %>%
    select(
      customer_id,
      offer_id,
      offered_price,
      promotion_active,
      purchase = fulfilled_purchase
    )
}
