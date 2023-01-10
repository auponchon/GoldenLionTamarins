library(pdftables)
library(here)
file<-list.files(here("Data","RawData"),pattern=".pdf",recursive=T,full.names=T)

convert_pdf(input_file=file[1],
            format="csv",
            api_key="4j1nv8o75k8s")
get_remaining("4j1nv8o75k8s")


library(pdftools)
library(stringr)
library(xlsx)

tx <- pdf_data(file[1])
tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)

write.xlsx(tx3, file=here("outputs","March_2007_R.xlsx"))
