
# ------------------------------ assigned_class() ------------------------------

# This function creates a new data.frame that includes the modal class
# assignment dummy variables

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
