source("~/Data_Extraction/COVID_intext_ChatGPT.R")
source("~/Data_Extraction/COVID_Table.R")


############# Table Extraction ###########################
#### Reading PDFs for table extraction ####################
########### Put your articles with PDF format using Table Extraction method in one folder ##################
pdf.table.vector <- list.files("~/Data_Extraction/Table_Extraction_PDF_folder", full.names = T)
pdf.table.vector <- pdf.table.vector[grep("pdf", pdf.table.vector, ignore.case = T)]
pdf.table.vector <- pdf.table.vector[!grepl("pdftools", pdf.table.vector)]
pdf.table.vector





table_extraction <- vector()

for(table.i in c(1:length(pdf.table.vector))) { ######## Use the index for articles for table extraction
  print(table.i)
  print(pdf.table.vector[table.i])
  table_extraction <-  rbind(table_extraction, pdf.extraction(src = pdf.table.vector[table.i]))
}


######## Results for Table Extraction Method

table_extraction



############# Inline Extraction ###########################
#### Reading PDFs for inline text extraction ####################
####### Put your articles with PDF format using In-line text Extraction method in one folder ########
#############  different from the one using Table extraction   ##############
pdf.inline.vector <- list.files("~/Data_Extraction/Inline_Extraction_PDF_folder", full.names = T)
pdf.inline.vector <- pdf.inline.vector[grep("pdf", pdf.inline.vector, ignore.case = T)]
pdf.inline.vector <- pdf.inline.vector[!grepl("pdftools", pdf.inline.vector)]
pdf.inline.vector



inline_extraction <- vector()

for(inline.i in c(1:length(pdf.inline.vector))) {
  print(inline.i)
  print(pdf.inline.vector[inline.i])
  inline_extraction <-  rbind(inline_extraction, intext.extraction(src = pdf.inline.vector[inline.i]))
}



######## Results for In-line Extraction Method
inline_extraction





########### Combine Table data extraction result and 
########### in-line text extraction result together
########### create the data frame and generate the meta-analysis 

mined.rob2 <- inline_extraction

######### Data Cleaning by hand if there are any unwanted trials
mined.rob2[3,] <- c("Ivaschenko 2020",2,40, "AVIFAVIR")
mined.rob3 <- mined.rob2[-4,]



######## Combine table extraction result and inline extraction result ##########
mined.rob_new <- rbind(table_extraction[,-2],mined.rob3)
mined.rob_new[,4] <- str_extract(mined.rob_new[,4],r"{[A-Z]*[a-z]*\+*/*[A-Z]*[a-z]*}")


row.names(mined.rob_new) <- mined.rob_new[,1]
Trt_key <- "Favipira|favipira|FPV|AVIFAVIR|treatment|Treatment"

mine.rob_Trt_s <- grep(Trt_key,mined.rob_new[,4])
mine.rob_Con <- data.frame(mined.rob_new[-mine.rob_Trt_s,])
mine.rob_Trt <- data.frame(mined.rob_new[mine.rob_Trt_s,])

test_data <- merge(x = mine.rob_Trt,y = mine.rob_Con,by = 'row.names',all = T)
test_data[,c(3,4,7,8)] <- sapply(test_data[,c(3,4,7,8)], as.numeric)



# Result of Combining Table extraction and inline-text extraction table
test_data[,-1]


######### Loading required package for Meta-Analysis ##########
library(meta)


meta1<-metabin(event.e = test_data$Events.x, n.e = test_data$Total.x, 
               event.c = test_data$Events.y, n.c = test_data$Total.y,
               studlab=test_data$Reference.x, sm="RR",comb.fixed = FALSE, comb.random = TRUE, data=test_data,RR.Cochrane = TRUE)
summary(meta1)


forest(meta1)
