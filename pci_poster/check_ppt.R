library(officer)
ppt <- read_pptx("poster_template.pptx")
layout_info <- layout_properties(ppt, layout = "Title Slide", master = "Office Theme")
print(layout_info$ph_label)
