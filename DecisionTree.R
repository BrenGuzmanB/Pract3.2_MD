# Función para construir un árbol de decisión de regresión
build_regression_tree <- function(X, y, max_depth, min_samples_leaf) {
  n <- nrow(X)
  p <- ncol(X)
  
  X <- apply(X, 2, as.character)
  
  # Verificar condiciones de paro
  if (max_depth == 0 || n < 2 * min_samples_leaf) {
    # Crear un nodo hoja
    leaf_node <- list(node_type = "leaf", mean_value = mean(y))
    return(leaf_node)
  }
  
  # Inicializar variables
  best_sdr <- -Inf
  best_split <- NULL
  
  # Loop sobre todas las características y valores para encontrar la mejor división
  for (j in 1:p) {
    feature_values <- sort(unique(X[, j]))
    for (threshold in feature_values) {
      left_indices <- X[, j] <= threshold
      right_indices <- !left_indices
      
      left_y <- y[left_indices]
      right_y <- y[right_indices]
      
      if (length(left_y) < min_samples_leaf || length(right_y) < min_samples_leaf) {
        # Saltar si la división no cumple con la condición de cantidad mínima de instancias en una hoja
        next
      }
      
      # Calcular SDR para la división actual
      current_sdr <- calculate_sdr(y, left_y, right_y)
      
      # Actualizar la mejor división si es necesario
      if (current_sdr > best_sdr) {
        best_sdr <- current_sdr
        best_split <- list(feature = j, threshold = threshold,
                           left_indices = left_indices, right_indices = right_indices)
      }
    }
  }
  
  # Verificar si no se encontró una división que mejore SDR
  if (is.null(best_split)) {
    leaf_node <- list(node_type = "leaf", mean_value = mean(y))
    return(leaf_node)
  }
  
  # Construir nodos izquierdo y derecho de manera recursiva
  left_tree <- build_regression_tree(X[best_split$left_indices, , drop = FALSE],
                                     y[best_split$left_indices],
                                     max_depth - 1, min_samples_leaf)
  right_tree <- build_regression_tree(X[best_split$right_indices, , drop = FALSE],
                                      y[best_split$right_indices],
                                      max_depth - 1, min_samples_leaf)
  
  # Crear nodo de decisión
  decision_node <- list(node_type = "decision", feature = best_split$feature,
                        threshold = best_split$threshold, left = left_tree, right = right_tree)
  
  return(decision_node)
}

# Función para calcular la reducción de desviación estándar (SDR)
calculate_sdr <- function(y, left_y, right_y) {
  sdr = sd(y) - (length(left_y) / length(y) * sd(left_y)) - (length(right_y) / length(y) * sd(right_y))
  return(sdr)
}

# Función para calcular el coeficiente de variabilidad
calculate_coefficient_of_variability <- function(y) {
  coefficient_of_variability = sd(y) / mean(y)
  return(coefficient_of_variability)
}


