default_replenishment_settings <- function() {
  list(
    enabled = FALSE,
    quantity = 0,
    interval = Inf
  )
}

default_inventory_behavior <- function() {
  list(
    enabled = TRUE,
    scarcity_logit_lift = 0.4,
    abundance_logit_penalty = 0.15
  )
}

dynamic_demand_shock <- function(demand_shock_settings, n) {
  if (!demand_shock_settings$enabled) {
    return(rep(0, n))
  }

  if_else(
    runif(n) < demand_shock_settings$probability,
    rnorm(
      n,
      demand_shock_settings$logit_shift_mean,
      demand_shock_settings$logit_shift_sd
    ),
    0
  )
}

inventory_behavior_lift <- function(
    inventory,
    initial_inventory,
    inventory_behavior) {
  if (!inventory_behavior$enabled || initial_inventory <= 0) {
    return(0)
  }

  inventory_share <- inventory / initial_inventory

  if (inventory_share < 0.25) {
    inventory_behavior$scarcity_logit_lift * (0.25 - inventory_share) / 0.25
  } else if (inventory_share > 0.75) {
    -inventory_behavior$abundance_logit_penalty *
      (inventory_share - 0.75) / 0.25
  } else {
    0
  }
}

apply_replenishment <- function(inventory, time_step, replenishment_settings) {
  if (!replenishment_settings$enabled || is.infinite(replenishment_settings$interval)) {
    return(inventory)
  }

  if (time_step > 1 && time_step %% replenishment_settings$interval == 0) {
    inventory + replenishment_settings$quantity
  } else {
    inventory
  }
}

simulate_dynamic_inventory_policy <- function(
    customer_df,
    policy_name,
    policy_fn,
    horizon,
    initial_inventory,
    price_grid,
    unit_cost,
    demand_shock_settings = list(
      enabled = FALSE,
      probability = 0,
      logit_shift_mean = 0,
      logit_shift_sd = 0
    ),
    replenishment_settings = default_replenishment_settings(),
    inventory_behavior = default_inventory_behavior(),
    seed = 2028) {
  set.seed(seed)

  inventory <- initial_inventory
  history <- vector("list", horizon)

  for (time_step in seq_len(horizon)) {
    inventory <- apply_replenishment(
      inventory,
      time_step,
      replenishment_settings
    )

    state <- list(
      time_step = time_step,
      horizon = horizon,
      inventory = inventory,
      initial_inventory = initial_inventory,
      price_grid = price_grid
    )

    price <- policy_fn(state)
    shock <- dynamic_demand_shock(demand_shock_settings, nrow(customer_df))
    inventory_lift <- inventory_behavior_lift(
      inventory,
      initial_inventory,
      inventory_behavior
    )
    purchase_probability <- plogis(
      customer_df$true_alpha +
        customer_df$true_beta * price +
        shock +
        inventory_lift
    )
    unconstrained_demand <- rbinom(
      nrow(customer_df),
      1,
      purchase_probability
    )
    sales <- min(sum(unconstrained_demand), inventory)
    stockout <- sum(unconstrained_demand) > inventory
    profit <- (price - unit_cost) * sales
    inventory_after <- max(inventory - sales, 0)

    history[[time_step]] <- tibble(
      policy = policy_name,
      time_step = time_step,
      price = price,
      inventory_start = inventory,
      inventory_end = inventory_after,
      expected_purchase_probability = mean(purchase_probability),
      unconstrained_demand = sum(unconstrained_demand),
      sales = sales,
      stockout = stockout,
      profit = profit
    )

    inventory <- inventory_after
  }

  bind_rows(history) %>%
    mutate(
      cumulative_profit = cumsum(profit),
      cumulative_sales = cumsum(sales),
      stockout_frequency = cummean(stockout)
    )
}

simulate_dynamic_pricing_policies <- function(
    customer_df,
    policies,
    horizon,
    initial_inventory,
    price_grid,
    unit_cost,
    demand_shock_settings = list(
      enabled = FALSE,
      probability = 0,
      logit_shift_mean = 0,
      logit_shift_sd = 0
    ),
    replenishment_settings = default_replenishment_settings(),
    inventory_behavior = default_inventory_behavior(),
    seed = 2028) {
  bind_rows(
    lapply(
      seq_along(policies),
      function(policy_idx) {
        simulate_dynamic_inventory_policy(
          customer_df = customer_df,
          policy_name = names(policies)[[policy_idx]],
          policy_fn = policies[[policy_idx]],
          horizon = horizon,
          initial_inventory = initial_inventory,
          price_grid = price_grid,
          unit_cost = unit_cost,
          demand_shock_settings = demand_shock_settings,
          replenishment_settings = replenishment_settings,
          inventory_behavior = inventory_behavior,
          seed = seed + policy_idx
        )
      }
    )
  )
}
