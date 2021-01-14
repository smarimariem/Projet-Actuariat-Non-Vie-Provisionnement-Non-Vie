install.packages("openxlsx")
library("openxlsx")
install.packages("ChainLadder")
library("ChainLadder")

# **************************** Triangle des  reglements  : La methode macK & bootstrap  ********************** #

my_data_reg <- read.xlsx("D:/5 DS/actuariat non vie/Projet/Triangle3.xlsx",sheet = "REG", colNames = TRUE,  rowNames = TRUE)
my_data_reg
tri_reg <- as.triangle(as.matrix(my_data_reg))

# Triangle des cumules
tri_reg_cumul = tri_reg
n_row = nrow(tri_reg_cumul)
for( i in 1: 9){
  
  for( j in 2:n_row ) {
    tri_reg_cumul[i,j] = tri_reg_cumul[i,j-1] + tri_reg_cumul[i,j]
  }
  n_row = n_row -1
}

# La methode macK
tri_mack_reg = MackChainLadder(tri_reg_cumul, est.sigma="Mack")

tri_mack_reg
plot(tri_mack_reg)

# La methode bootstrap

tri_boot_reg = BootChainLadder(tri_reg_cumul, R=999, process.distr="gamma")

tri_boot_reg
plot(tri_boot_reg)

# **************************** Triangle des Ouvertures  : La methode macK & bootstrap  ********************** #


my_data <- read.xlsx("D:/5 DS/actuariat non vie/Projet/Triangle3.xlsx", sheet = "NB",colNames = TRUE,  rowNames = TRUE)
my_data

# Triangle des cumules
tri_nb <- as.triangle(as.matrix(my_data))
tri_nb_cumul = tri_nb
n_row = nrow(tri_nb_cumul)
for( i in 1: 9){

    for( j in 2:n_row ) {
	tri_nb_cumul[i,j] = tri_nb_cumul[i,j-1] + tri_nb_cumul[i,j]
	}
	n_row = n_row -1
	}
# La methode macK


tri_mack_nb = MackChainLadder(tri_nb_cumul, est.sigma="Mack")
tri_mack_nb
plot(tri_mack_nb)


# La methode bootstrap
tri_boot_nb = BootChainLadder(tri_nb_cumul, R=999, process.distr="gamma")
tri_boot_nb
plot(tri_boot_nb)
