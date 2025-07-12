# Load required libraries
set.seed(123)

# Define unary and binary operators


expn = function(x) exp(-abs(x))
cbrt = function(x) sign(x) * abs(x)^(1/3)
cube = function(x) x^3
logs = function(x) log(pmax(x, 1e-6))

unary_ops <- list(
  sin = function(x) sin(x),
  expn = function(x) exp(-abs(x)),
  logs = function(x) log(pmax(x, 1e-6)),  # to avoid log(0)
  cbrt = function(x) sign(x) * abs(x)^(1/3),
  cube = function(x) x^3
)

binary_ops <- list(
  `+` = function(x, y) x + y,
  `*` = function(x, y) x * y
)

# Generate random expressions recursively
generate_tree <- function(vars, depth = 1, max_depth = sample(2:4, 1)) {
  if (depth >= max_depth || runif(1) < 0.3) {
    # Terminal node (just a variable)
    var_name <- sample(vars, 1)
    return(list(type = "var", name = var_name))
  }
  
  if (runif(1) < 0.5) {
    # Unary operator
    op_name <- sample(names(unary_ops), 1)
    child <- generate_tree(vars, depth + 1, max_depth)
    return(list(type = "unary", op = op_name, child = child))
  } else {
    # Binary operator
    op_name <- sample(names(binary_ops), 1)
    left <- generate_tree(vars, depth + 1, max_depth)
    right <- generate_tree(vars, depth + 1, max_depth)
    return(list(type = "binary", op = op_name, left = left, right = right))
  }
}

# Evaluate expression tree on data
evaluate_tree <- function(tree, data) {
  if (tree$type == "var") {
    return(data[[tree$name]])
  } else if (tree$type == "unary") {
    op_func <- unary_ops[[tree$op]]
    return(op_func(evaluate_tree(tree$child, data)))
  } else if (tree$type == "binary") {
    op_func <- binary_ops[[tree$op]]
    return(op_func(evaluate_tree(tree$left, data), evaluate_tree(tree$right, data)))
  }
}

# Convert tree to human-readable expression
tree_to_string <- function(tree) {
  if (tree$type == "var") {
    return(tree$name)
  } else if (tree$type == "unary") {
    return(paste0(tree$op, "(", tree_to_string(tree$child), ")"))
  } else if (tree$type == "binary") {
    return(paste0("(", tree_to_string(tree$left), " ", tree$op, " ", tree_to_string(tree$right), ")"))
  }
}

vary_tree <- function(tree, vars,
                      mutation_rate = 0.3,
                      drop_var_prob = 0.2,
                      replace_var_with_subtree_prob = 0.2,
                      max_depth = 5,
                      depth = 1) {
  if (is.null(tree)) return(NULL)
  
  # Terminal node
  if (tree$type == "var") {
    rand <- runif(1)
    if (rand < drop_var_prob) {
      return(NULL)  # Drop this variable
    } else if (rand < drop_var_prob + replace_var_with_subtree_prob) {
      # Replace with a simple subtree of depth 1
      return(generate_tree(vars, depth = depth, max_depth = depth + 1))
    } else if (rand < drop_var_prob + replace_var_with_subtree_prob + mutation_rate) {
      # Replace with another variable
      return(list(type = "var", name = sample(vars, 1)))
    } else {
      return(tree)  # No change
    }
  }
  
  # Unary node
  if (tree$type == "unary") {
    mutate_type <- sample(c("op", "child"), 1)
    new_child <- vary_tree(tree$child, vars,
                           mutation_rate,
                           drop_var_prob,
                           replace_var_with_subtree_prob,
                           max_depth, depth + 1)
    if (is.null(new_child)) return(NULL)
    
    if (mutate_type == "op" && runif(1) < mutation_rate) {
      new_op <- sample(setdiff(names(unary_ops), tree$op), 1)
      return(list(type = "unary", op = new_op, child = new_child))
    } else {
      return(list(type = "unary", op = tree$op, child = new_child))
    }
  }
  
  # Binary node
  if (tree$type == "binary") {
    mutate_type <- sample(c("op", "left", "right", "subtree"), 1)
    new_left <- vary_tree(tree$left, vars,
                          mutation_rate,
                          drop_var_prob,
                          replace_var_with_subtree_prob,
                          max_depth, depth + 1)
    new_right <- vary_tree(tree$right, vars,
                           mutation_rate,
                           drop_var_prob,
                           replace_var_with_subtree_prob,
                           max_depth, depth + 1)
    
    if (is.null(new_left) && is.null(new_right)) return(NULL)
    if (is.null(new_left)) return(new_right)
    if (is.null(new_right)) return(new_left)
    
    if (mutate_type == "op" && runif(1) < mutation_rate) {
      new_op <- sample(setdiff(names(binary_ops), tree$op), 1)
      return(list(type = "binary", op = new_op, left = new_left, right = new_right))
    } else if (mutate_type == "subtree" && runif(1) < mutation_rate) {
      return(generate_tree(vars, depth = depth, max_depth = max_depth))
    } else {
      return(list(type = "binary", op = tree$op, left = new_left, right = new_right))
    }
  }
  
  return(tree)
}


# Symbolic regression: fit N trees and return best one
symbolic_regression <- function(data, target, n_trees = 100, n_conv = 10, n_mut = 100, newgen_rate=1, verbose=F, calibrate=T, crit_R2=0.999981) {
  best_tree <- NULL
  best_mse <- Inf
  last_mse <- Inf
  vars <- setdiff(names(data), target)
  var_y <- var(data[[target]])
  
  for (i in 1:n_trees) {
    # generate whole new tree
    if(i == 1 | runif(1)<newgen_rate) {
      tree <- generate_tree(vars)
      mut=FALSE
    } else {
      tree <- vary_tree(best_tree, vars, mutation_rate=1)
      mut=TRUE
    }
    
    
    pred <- tryCatch(evaluate_tree(tree, data), error = function(e) rep(Inf, nrow(data)))
    if (any(!is.finite(pred))) next
    
    
    if(!calibrate) mse <- mean((data[[target]] - pred)^2, na.rm=T) 
    else mse <- var(lm(data[[target]]~pred-1)$residuals, na.rm=T)
    
    if(is.na(mse)) mse <- Inf
    
    rsq <- max(1 - mse / var_y, 0)
    newgen_rate <- 1-rsq
    
    mse_parent <- mse
    tree_parent <- tree
    
    if (mse < best_mse) {
      if(verbose) cat(ifelse(mut,"Mutated best tree ","New best tree "),i,": ", tree_to_string(tree), " Rsq: ", rsq, "\n")
      last_mse <- best_mse
      best_mse <- mse
      best_tree <- tree
      i_tree <- i
    }
    
    if((1-best_mse/var_y >= crit_R2) | (i-i_tree >= n_conv)) break
    
    # generate mutation if new tree is at least better than last_mse
    
    if(FALSE & (mse_parent < last_mse & n_mut>1)){
      for(j in 1:n_mut) {
        tree <- vary_tree(tree, vars, mutation_rate=1)
  
        pred <- tryCatch(evaluate_tree(tree, data), error = function(e) rep(Inf, nrow(data)))
        if (any(!is.finite(pred))) next
        
        
        if(!calibrate) mse <- mean((data[[target]] - pred)^2, na.rm=T) 
        else mse <- var(lm(data[[target]]~pred-1)$residuals, na.rm=T)
        
        if(is.na(mse)) mse <- Inf
        
        if (mse < best_mse) {
          if(verbose) cat("New best tree ",i,"M",j,": ", tree_to_string(tree), " MSE: ", mse, "\n")
          last_mse <- best_mse
          best_mse <- mse
          best_tree <- tree
        }
      }
    }
    
  }
  
  list(tree = best_tree, mse = best_mse, expression = tree_to_string(best_tree))
}


plot_tree_auto_legend <- function(tree, x = 0, y = 0, level_gap = 1.5, sibling_gap = 1, scale = 1) {
  var_names <- unique(collect_vars(tree))
  var_map <- setNames(seq_along(var_names), var_names)
  
  plot.new()
  plot.window(xlim = c(-10, 10), ylim = c(-1, 10))
  draw_node(tree, x, y, level_gap, sibling_gap, scale, var_map)
  
  legend_labels <- paste0(var_map, " = ", names(var_map))
  legend("topright", legend = legend_labels, bty = "n")
}

collect_vars <- function(tree) {
  if (is.null(tree)) return(character(0))
  if (tree$type == "var") return(tree$name)
  if (tree$type == "unary") return(collect_vars(tree$child))
  if (tree$type == "binary") return(c(collect_vars(tree$left), collect_vars(tree$right)))
  return(character(0))
}

draw_node <- function(node, x, y, level_gap, sibling_gap, scale, var_map) {
  if (is.null(node)) return(invisible())
  
  label <- switch(node$type,
                  "var" = {
                    num <- var_map[[node$name]]
                    if (!is.null(num)) as.character(num) else node$name
                  },
                  "unary" = node$op,
                  "binary" = node$op,
                  "?")
  
  draw_circle_with_label(x, y, label)
  
  if (node$type == "unary" && !is.null(node$child)) {
    child_x <- x
    child_y <- y - level_gap
    segments(x, y - 0.5 * scale, child_x, child_y + 0.5 * scale)
    draw_node(node$child, child_x, child_y, level_gap, sibling_gap, scale, var_map)
    
  } else if (node$type == "binary") {
    left_x <- x - sibling_gap / 2
    right_x <- x + sibling_gap / 2
    child_y <- y - level_gap
    
    if (!is.null(node$left)) {
      segments(x, y - 0.5 * scale, left_x, child_y + 0.5 * scale)
      draw_node(node$left, left_x, child_y, level_gap, sibling_gap * 0.8, scale, var_map)
    }
    if (!is.null(node$right)) {
      segments(x, y - 0.5 * scale, right_x, child_y + 0.5 * scale)
      draw_node(node$right, right_x, child_y, level_gap, sibling_gap * 0.8, scale, var_map)
    }
  }
}

draw_circle_with_label <- function(x, y, label, r = 0.4) {
  symbols(x, y, circles = r, inches = FALSE, add = TRUE, fg = "black", bg = "white")
  text(x, y, labels = label, cex = 1.2)
}
