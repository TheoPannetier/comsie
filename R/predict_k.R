predict_k <- function(siga, sigk) {
  return(0.056 * siga ^(-1.4) + (5.5 * siga ^(-0.89) - 4.1 * siga^(-0.5)) * sigk)
}
