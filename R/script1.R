ld_chr6 <- read_table2("github/biodata_hackathon2019/ld_chr6_JPT.txt/ld_chr6_JPT.txt", 
                       col_names = FALSE)


jpt <- ld_chr6

ceu <- read_table2("github/biodata_hackathon2019/ld_chr6_CEU.txt/ld_chr6_CEU.txt", 
               col_names = FALSE)

# random selection of base pairs

snp_list <- ceu_subset <- ceu %>%
  filter(between(X2,17963381,17993381)) %>%
  pull(X4)
snp_list <- unique(snp_list)

jpt_subset <- jpt %>%
  filter(X4 %in% snp_list & X5 %in% snp_list) %>%
  select(X1, X2, X4, X5, X7)

ceu_subset <- ceu %>%
  filter(X4 %in% snp_list & X5 %in% snp_list) %>%
  select(X1, X2, X4, X5, X7)


ceu_subset$rsnum <- paste(ceu_subset$X4,"|",ceu_subset$X5,sep="")
jpt_subset$rsnum  <- paste(jpt_subset$X4,"|",jpt_subset$X5,sep="")
length(ceu_subset$rsnum)
length(jpt_subset$rsnum)

ceu_subset$pos <- paste(ceu_subset$X1,"|",ceu_subset$X2,sep="")
jpt_subset$pos <- paste(jpt_subset$X1,"|",jpt_subset$X2,sep="")

rsnum <- setdiff(ceu_subset$rsnum, jpt_subset$rsnum)
names(rsnum) <- "rsnum";


pos <- setdiff(ceu_subset$pos, jpt_subset$pos)
names(pos) <- "pos";

str(ceu_subset)
str(jpt_subset)

rowstoadd <- data.frame(rsnum,pos)
str(rowstoadd)
head(dftest <- bind_rows(jpt_subset,rowstoadd))

dim(dftest)


tail(dftest)

save(jpt_subset, ceu_subset, file="github/biodata_hackathon2019/data/LD_data.RData")
