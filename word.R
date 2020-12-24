library(officer)
library(dplyr)

sample_doc <- read_docx()

sample_doc <- sample_doc %>% body_add_par("This is the first paragraph") 
sample_doc <- sample_doc %>% body_add_par("This is the second paragraph")
sample_doc <- sample_doc %>% body_add_par("This is the third paragraph")

# create sample data frame
df <- data.frame(a = 1:10, b = 11:20, c= 21:30)

# add table containing the data frame's contents
sample_doc <- sample_doc %>% body_add_table(df, style = "table_template")

set.seed(0)

# create a temp file
src <- tempfile(fileext = "unnamed.png")

# create PNG object
png(filename = src, width = 4, height = 4, units = 'in', res = 400)

# create plot
plot(sample(100, 10))

# save PNG file
dev.off()

# add PNG image to Word document
sample_doc <- sample_doc %>% body_add_img(src = src, width = 4, height = 4, style = "centered")

print(sample_doc, target = "sample_file.docx")