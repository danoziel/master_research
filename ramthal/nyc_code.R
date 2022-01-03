install.packages('RColorBrewer')


devtools::install_github('danoziel/RColorBrewer')

if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load 
installr

# using the package:
updateR()