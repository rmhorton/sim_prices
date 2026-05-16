lookup_expected_profit <- function(estimated_profit_df, price) {
  approx(
    x = estimated_profit_df$price,
    y = estimated_profit_df$expected_profit,
    xout = price,
    rule = 2
  )$y
}

select_greedy_price <- function(estimated_profit_df) {
  estimated_profit_df$price[
    which.max(estimated_profit_df$expected_profit)
  ]
}

make_static_pricing_policy <- function(price) {
  force(price)

  function(state) {
    price
  }
}

make_greedy_revenue_policy <- function(estimated_profit_df) {
  greedy_price <- select_greedy_price(estimated_profit_df)

  function(state) {
    greedy_price
  }
}

make_inventory_aware_pricing_policy <- function(estimated_profit_df) {
  greedy_price <- select_greedy_price(estimated_profit_df)
  price_grid <- estimated_profit_df$price
  price_span <- max(price_grid) - min(price_grid)

  function(state) {
    time_remaining <- max(state$horizon - state$time_step + 1, 1)
    target_inventory <- state$initial_inventory * time_remaining / state$horizon
    scarcity <- (target_inventory - state$inventory) / state$initial_inventory
    adjusted_price <- greedy_price + scarcity * 0.35 * price_span

    price_grid[which.min(abs(price_grid - adjusted_price))]
  }
}

make_conservative_inventory_policy <- function(estimated_profit_df) {
  price_grid <- estimated_profit_df$price
  conservative_price <- quantile(price_grid, 0.75, names = FALSE)
  greedy_price <- select_greedy_price(estimated_profit_df)

  function(state) {
    elapsed_share <- state$time_step / state$horizon
    inventory_share <- state$inventory / state$initial_inventory

    if (inventory_share < (1 - elapsed_share)) {
      price_grid[which.min(abs(price_grid - conservative_price))]
    } else {
      greedy_price
    }
  }
}

build_dynamic_pricing_policies <- function(
    estimated_profit_df,
    static_price = select_greedy_price(estimated_profit_df)) {
  list(
    "Static Pricing" = make_static_pricing_policy(static_price),
    "Inventory-Aware Pricing" = make_inventory_aware_pricing_policy(
      estimated_profit_df
    ),
    "Greedy Revenue Maximization" = make_greedy_revenue_policy(
      estimated_profit_df
    ),
    "Conservative Inventory Preservation" = make_conservative_inventory_policy(
      estimated_profit_df
    )
  )
}
