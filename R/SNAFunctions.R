################################################################################################
# functions

# Calculate centrality scores for binary networks
net.stats <- function(y) {
  # calculate degree centrality
  dg <- as.matrix(sna::degree(y, gmode = "graph"))
  # calculate and scale eigenvector centrality
  eg <- as.matrix(sna::evcent(y))
  eg <- sqrt((eg^2) * length(eg))
  # calculate betweenness centrality
  bw <- sna::betweenness(y, gmode = "graph")
  # combine centrality scores into matrix
  output <- cbind(dg, eg, bw)
  rownames(output) <- xnames$ProjectNumber
  colnames(output) <- c("dg", "eg", "bw")
  return(output)
}  # return results of this function

# Calculate centrality scores for weighted networks (similarity matrices)
net.stats.wt <- function(y) {
  # calculate weighted degree as the sum of weights - 1
  dg.wt <- as.matrix(rowSums(y) - 1)
  # calculate weighted eigenvector centrality and rescale
  eg.wt <- as.matrix(sna::evcent(y))
  eg.wt <- sqrt((eg.wt^2) * length(eg.wt))
  output <- cbind(dg.wt, eg.wt)
  rownames(output) <- xnames
  colnames(output) <- c("dg.wt", "eg.wt")
  return(output)
}  # return results of this function

# calculate Brainerd Robinson similarity
BRMatrix <- function(x) {
  rd <- dim(x)[1]
  results <- matrix(0,rd,rd)
  rownames(results) <- rownames(x)
  colnames(results) <- rownames(x)
  x <- prop.table(as.matrix(x),1)*100
  for (s1 in 1:rd) {
    for (s2 in 1:rd) {
      x1Temp <- as.numeric(x[s1, ])
      x2Temp <- as.numeric(x[s2, ])
      results[s1,s2] <- 200 - (sum(abs(x1Temp - x2Temp)))}}
  results <- (results/200)
  return(results)
}

