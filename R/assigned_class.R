
# ------------------------------ assigned_class() ------------------------------


# This function creates a new data.frame that includes the modal class
# assignment dummy variables

#' Assign Modal Class
#'
#' Adds the modal class assignment to a data frame based on highest posterior probability.
#'
#' @param data A data frame containing posterior class probabilities.
#' @param post.prob A character vector of column names corresponding to the posterior probabilities for each class.
#'
#' @return A data frame with a new column \code{ass_class} indicating the assigned class for each row.
#' @export


assigned_class <- function(data, post.prob) {
  # number of classes
  n_class <- length(post.prob)

  # get highest posterior probability per row
  data$max_post <- colMax(data[, post.prob])

  # create modal class assignment variable class1 to classn_class with modal class assignment
  for (i in 1:nrow(data)) {
    for (s in 1:n_class) {
      if (data$max_post[i] == data[i, post.prob[s]]) {
        data[i, "ass_class"] <- s
      }
    }
  }
  return(data)
}


# helper: colMax function
colMax <- function (data) {
  do.call(pmax, data)
}
