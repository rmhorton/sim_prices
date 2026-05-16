generate_customer_world <- function(N_customers = 400, seed = 2028) {
  set.seed(seed)

  customer_df <- tibble(
    customer_id = 1:N_customers
  )

  customer_df <- customer_df %>%
    mutate(
      latent_segment =
        sample(
          c("Budget", "Mainstream", "Premium"),
          N_customers,
          replace = TRUE,
          prob = c(0.5, 0.35, 0.15)
        )
    )

  customer_df <- customer_df %>%
    mutate(
      true_alpha =
        case_when(
          latent_segment == "Budget" ~ rnorm(n(), 0.8, 0.5),
          latent_segment == "Mainstream" ~ rnorm(n(), 1.6, 0.5),
          latent_segment == "Premium" ~ rnorm(n(), 2.5, 0.5)
        ),

      true_beta =
        case_when(
          latent_segment == "Budget" ~ rnorm(n(), -0.85, 0.12),
          latent_segment == "Mainstream" ~ rnorm(n(), -0.45, 0.10),
          latent_segment == "Premium" ~ rnorm(n(), -0.15, 0.08)
        ),

      lifetime_value =
        case_when(
          latent_segment == "Budget" ~ rnorm(n(), 100, 20),
          latent_segment == "Mainstream" ~ rnorm(n(), 350, 40),
          latent_segment == "Premium" ~ rnorm(n(), 1200, 150)
        )
    )

  customer_df
}

simulate_purchase_opportunities <- function(customer_df, N_offers = 8) {
  offer_df <- crossing(
    customer_id = customer_df$customer_id,
    offer_id = 1:N_offers
  )

  offer_df <- offer_df %>%
    left_join(customer_df, by = "customer_id")

  offer_df <- offer_df %>%
    mutate(
      offered_price =
        round(runif(n(), 2, 30), 2)
    )

  offer_df <- offer_df %>%
    mutate(
      logit_p =
        true_alpha +
        true_beta * offered_price,

      purchase_probability =
        plogis(logit_p)
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

  offer_df
}

observe_analyst_data <- function(offer_df) {
  offer_df %>%
    select(
      customer_id,
      offered_price,
      purchase
    )
}
