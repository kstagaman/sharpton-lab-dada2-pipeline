# generate_template_data.R

template.code <- readLines(con = "dada2_processing.R")
save(template.code, file = "data/template_code.rda")
