
N <- 10
risk <- data.frame(Simulation=1:N,
                   Rounds=NA,
                   An=NA,
                   Dn=NA)
for (i in 1:N){

An <- 189 #Assume An >= 2
Dn <-  100#Assume Dn >= 1

k <- 0
while (An > 1 & Dn > 0){

if (An >= 4){
  Ar <- sample(1:6, 3, TRUE)
} else {
  Ar <- sample(1:6, An-1, TRUE)
}

if (Dn >=2){
  Dr <-sample(1:6, 2, TRUE)
} else {
  Dr <- sample(1:6, 1, TRUE)
}

LAr <-length(Ar)
LDr <-length(Dr)

Lan <-min(LAr, LDr)

Ar <-sort(Ar, decreasing=TRUE)[1:Lan]
Dr <-sort(Dr, decreasing=TRUE)[1:Lan]

ALose <- Lan - sum(Ar > Dr)
DLose <- Lan - ALose

An <-An - ALose
Dn <-Dn - DLose

k <- k+1
cat("Round ",k,": An = ", An, "\t| Dn = ", Dn, "\n")

}

risk[i,2:4] <- cbind(k, An, Dn)

cat("\n\n New Simulation:\n")

#At last round, 
#if (An > 1){Succses}
#if (An = 1){Failure}

}
risk

