
# Construction de la table
life_table <- funtion(tx,output_dir){
  lt.mx = function (nmx, sex = "female", age = c(0, seq(5, 85, 5)),
                    nax = NULL)
  {
    if (is.null(nax)) {
      nax <- c(1.6, rep(2.5, length(age)-1)) # a modifier
    }
    else {
      nax = nax
    }
    n <- c(diff(age), 999)
    nqx <- (n * nmx)/(1 + (n - nax) * nmx)
    nqx <- c(nqx[-(length(nqx))], 1)
    for (i in 1:length(nqx)) {
      if (nqx[i] > 1)
        nqx[i] <- 1
    }
    nage <- length(age)
    nqx <- round(nqx, 4)
    npx <- 1 - nqx
    l0 = 1e+06
    lx <- round(cumprod(c(l0, npx)))
    ndx <- -diff(lx)
    lxpn <- lx[-1]
    nLx <- n * lxpn + ndx * nax
    Tx <- rev(cumsum(rev(nLx)))
    lx <- lx[1:length(age)]
    ex <- Tx/lx
    lt <- cbind(Age = age, nax = round(nax, 3), nmx = round(nmx,
                                                            4), nqx = round(nqx, 4), npx = round(npx, 4), ndx = ndx,
                lx = lx, nLx = round(nLx), Tx = round(Tx), ex = round(ex,
                                                                      2))
    #lt <- lt[lt[, 6] != 0, ]
    e0 <- lt[1, 10]
    lt.45q15 <- 1 - (lx[age == 60]/lx[age == 15])
    lt.5q0 <- 1 - (lx[age == 5]/lx[age == 0])
    return(list(e0 = e0, lt.5q0 = lt.5q0, lt.45q15 = lt.45q15,
                lt = lt))
  }

  # Calcul de l'esp?rance de vie
  e0 = apply(taux, c(2,3), function(x) lt.mx(x, sex = 'female')$lt[1,"ex"])
  colnames(e0) = c("Male","Female")
  e0 = data.frame(rownames(e0), e0)
  colnames(e0)[colnames(e0)=="rownames.e0."]="Code"
  e0 <- e0[,1:3]
  print(e0)
  write.csv(e0, paste0(output_dir,'Life Expectancy at Birth.csv'), row.names = FALSE)



}
