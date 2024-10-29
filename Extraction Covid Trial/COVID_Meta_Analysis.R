########### Combine Table data extraction result and 
########### in-line text extraction result together
########### create the data fram and generate the meta-analysis 

mined.rob2 <- mined.rob1

######### Data Cleaning by hand if there are any unwanted trials
mined.rob2[3,] <- c("Ivaschenko 2020(not find)",2,40, "AVIFAVIR")
mined.rob3 <- mined.rob2[-4,]
mined.rob_new <- rbind(mined.rob[,-2],mined.rob3)
row.names(mined.rob_new) <- mined.rob_new[,1]
Trt_key <- "Favipira|favipira|FPV|AVIFAVIR|treatment|Treatment"

mine.rob_Trt_s <- grep(Trt_key,mined.rob_new[,4])
mine.rob_Con <- data.frame(mined.rob_new[-mine.rob_Trt_s,])
mine.rob_Trt <- data.frame(mined.rob_new[mine.rob_Trt_s,])

test_data <- merge(x = mine.rob_Trt,y = mine.rob_Con,by = 'row.names',all = T)
test_data[,c(3,4,7,8)] <- sapply(test_data[,c(3,4,7,8)], as.numeric)

test_data[,-1]
library(meta)


meta1<-metabin(event.e = test_data$Events.x, n.e = test_data$Total.x, 
               event.c = test_data$Events.y, n.c = test_data$Total.y,
               studlab=test_data$Reference.x, sm="RR", comb.fixed = T, comb.random = F, data=test_data)
summary(meta1)
forest.meta(meta1, col.diamond = "blue", overall.hetstat=F)
forest.meta(meta1, col.diamond = "blue", overall.hetstat=F,leftlabs = "Article",
            leftcols = c("X1","studlab","event.e","n.e","event.c","n.c"))

forest(meta1)
