# new script because the original text files not saved rename other script as data import or combine...####
library(tidyverse)
 

load( file="../biodata_hackathon2019/data/LD_data.RData")


# making base R more tidy... cuz I'mmmmmmma dinosaur!!!

ceu_subset <- ceu_subset %>% mutate(rsnum =  paste(ceu_subset$X4,"|",ceu_subset$X5,sep=""),
                                     pos  =  paste(ceu_subset$X1,"|",ceu_subset$X2,sep="")
                                   )


jpt_subset <- jpt_subset %>% mutate(rsnum =  paste(jpt_subset$X4,"|",jpt_subset$X5,sep=""),
                                      pos =  paste(jpt_subset$X1,"|",jpt_subset$X2,sep="")
                                    )

str(ceu_subset) 
str(jpt_subset) 

# now behold the power of BASE R! 
rsnum <- setdiff(ceu_subset$rsnum, jpt_subset$rsnum) 
  pos <- setdiff(ceu_subset$pos,   jpt_subset$pos) 

rowstoadd <- data.frame(rsnum, pos, stringsAsFactors = FALSE)
head(dftest <- bind_rows(jpt_subset,rowstoadd))
   
wtf <- strsplit(dftest$rsnum, "|", fixed = TRUE)
rsdf <- do.call(rbind.data.frame, wtf )
names(rsdf) <- c("rs1","rs2")
rsdf$rs1 <- as.character(rsdf$rs1)
rsdf$rs2 <- as.character(rsdf$rs2)

pos2 <- strsplit(dftest$pos, "|", fixed = TRUE)
posdf <- do.call(rbind.data.frame, pos2 )
names(posdf) <- c("pos1","pos2")

rsdf$pos1 <- as.character(posdf$pos1)
rsdf$pos2 <- as.character(posdf$pos2)


tail(dftest2 <- bind_cols(dftest,thedf))

dftest2 <- dftest2 %>% mutate( X1 = ifelse(is.na(X1), pos1, X1),
                               X2 = ifelse(is.na(X2), pos1, X2),
                               X4 = ifelse(is.na(X4), rs1,  X4),
                               X5 = ifelse(is.na(X5), rs2,  X5)  )

tail(dftest2) 
# Just need to impute missing with whatever.... ZEROs? 
# That's all folks... needs to be cleaned up a bit and I am sure a tidyverse solution would be more elegant. 
# Cheers! 

