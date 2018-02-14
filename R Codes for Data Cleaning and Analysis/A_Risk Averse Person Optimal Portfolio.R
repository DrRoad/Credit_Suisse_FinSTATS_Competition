#### OPTMIZATION #######



# For Risk averse person


library(quadprog)

Anew = rbind(A,rep(0,18))
Anew = cbind(Anew,rep(0,19))
Anew

Dmat = Anew
dvec = c(0)
Amat = matrix()
solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)



