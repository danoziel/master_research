# 'data.table' ----
https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
  
# Install from CRAN
install.packages('data.table')

# Install Dev version from Gitlab
install.packages("data.table", repos="https://Rdatatable.gitlab.io/data.table")
data.table::update.dev.pkg()

אם ברצונך לחזור לגרסת CRAN בצע:
  
  # Remove and Reinstall
  remove.packages("data.table")
install.packages("data.table")
# ----
library(data.table)

# reload data
data("mtcars")
head(mtcars)
class(mtcars)

mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
class(mtcars_dt)

# 6. Filtering rows based on conditions ----

mtcars_dt2 <- mtcars_dt[cyl==6 & gear==4, ]
mtcars_dt2 <-
mtcars_dt[, 1]
mtcars_dt2 <-
mtcars_dt[, 1, with=F]

mtcars_dt[, .(mpg, cyl, gear)]
mtcars_dt[ ,list(mpg, cyl, gear)]

drop_cols <- c("mpg", "cyl", "gear")
mtcars_dt[, !drop_cols, with=FALSE]

drop_rows <- c(1, 3, 5)
mtcars_dt[!drop_rows,]


########## 10. How to rename columns -----

# Jain j_dt  ----

j_dt <- ji_261122$
j_dt$carname <- rownames(j_dt)
j_dt <- as.data.table(j_dt)
class(jdt)

j_dt2 <- 
  j_dt[cropx == "Red_gram(Pigeon pea)",]

j_dt2 <- 
  j_dt[,'farmer_name']





