run_auction <- function(benefit, epsilon = 1e-3) {
  n <- nrow(benefit)
  
  # Initialize prices and assignments
  prices <- rep(0, n)
  owner_of <- rep(NA, n)     # object -> agent
  assigned_object <- rep(NA, n)  # agent -> object
  
  while (any(is.na(assigned_object))) {
    for (i in 1:n) {
      if (!is.na(assigned_object[i])) next  # already assigned
      
      # Compute utilities (benefit - price)
      utilities <- benefit[i, ] - prices
      
      # Get top two utility values and best object
      j_star <- which.max(utilities)
      u_star <- utilities[j_star]
      utilities[j_star] <- -Inf
      u_second <- max(utilities)
      
      # Compute bid increment
      bid_increment <- u_star - u_second + epsilon
      
      # Raise price
      prices[j_star] <- prices[j_star] + bid_increment
      
      # If object was owned, unassign previous owner
      previous_owner <- owner_of[j_star]
      if (!is.na(previous_owner)) {
        assigned_object[previous_owner] <- NA
      }
      
      # Assign object j_star to agent i
      assigned_object[i] <- j_star
      owner_of[j_star] <- i
    }
  }
  
  # Return result as a named list
  list(
    assignment = assigned_object,
    total_benefit = sum(benefit[cbind(1:n, assigned_object)]),
    prices = prices
  )
}
