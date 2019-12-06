dual_matrix <- function(dataset1, dataset2, snp1, snp2,
                        pos1, pos2, ld){
  
  # create variables for merging and completing cases
  dataset1 <- dataset1 %>% 
    dplyr::mutate(rsnum =  paste(dataset1[[snp1]],"|",dataset1[[snp2]],sep=""),
                  pos  =  paste(dataset1[[pos1]],"|",dataset1[[pos2]],sep=""))
  
  
  dataset2 <- dataset2 %>% 
    dplyr::mutate(rsnum =  paste(dataset2[[snp1]],"|",dataset2[[snp2]],sep=""),
                  pos =  paste(dataset2[[pos1]],"|",dataset2[[pos2]],sep=""))
  
  # pulling snps only in one dataset or the other
  rsnum <- setdiff(dataset1[[rsnum]], dataset2[[rsnum]]) 
  pos <- setdiff(dataset1[[pos]], dataset2[[pos]]) 
  
  # create data table
  rowstoadd <- data.frame(rsnum, pos, stringsAsFactors = FALSE)
  dftest <- bind_rows(dataset1,rowstoadd)
  dftestv2 <- bind_rows(dataset2,rowstoadd)
  
  # create dataframe with annotations  
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
  
  # create complete datasets
  dftest2 <- dftest2 %>% 
    dplyr::mutate( X1 = ifelse(is.na(X1), pos1, X1),
                   X2 = ifelse(is.na(X2), pos1, X2),
                   X4 = ifelse(is.na(X4), rs1,  X4),
                   X5 = ifelse(is.na(X5), rs2,  X5)  )

  # merge in LD values from other population 
  bad_snp <- c(setdiff(dftest2$pos1, dftest2$pos2),
               setdiff(dftest2$pos2, dftest2$pos1))
  
  dftest3 <- dftest2 %>% 
    # filter(pos1 %in% snpint & pos2 %in% snpint) %>%
    dplyr::mutate(pos1 = as.numeric(pos1)) %>%
    dplyr::mutate(pos2 = as.numeric(pos2)) %>%
    left_join(dftestv2 %>%
                select(pos, other_ld = {{ld}}), by=c("pos")) %>%
    #filter(!(pos1 %in% bad_snp) & !(pos2 %in% bad_snp)) %>%
    dplyr::mutate(ld1 = {{ld}}) %>%
    dplyr::mutate(ld1 = replace_na(ld1, 0)) %>%
    dplyr::mutate(other_ld = replace_na(other_ld, 0)) %>%
    arrange(pos1,pos2) %>%
    dplyr::mutate(pos1 = as_factor(pos1)) %>%
    dplyr::mutate(pos2 = as_factor(pos2)) 
  
  # make two data matrices for plotting
  data1 <- dftest3 %>%
    dplyr::select(pos1,pos2,ld1) %>%
    dplyr::pivot_wider(names_from = pos2,
                values_from = ld1,
                values_fill = list(ld1 = 0)) %>%
    magrittr::set_rownames(.$pos1) %>%
    dplryr::select(-pos1) %>%
    as.matrix
  
  data2 <- dftest3 %>%
    dplyr::select(pos1,pos2,other_ld) %>%
    dplyr::pivot_wider(names_from = pos2,
                values_from = other_ld,
                values_fill = list(other_ld = 0)) %>%
    magrittr::set_rownames(.$pos1) %>%
    dplyr::select(-pos1) %>%
    as.matrix
  
  data1 <-  data1[!(row.names(data1) %in% bad_snp),!(colnames(data1) %in% bad_snp)]
  data2 <-  data2[!(row.names(data2) %in% bad_snp),!(colnames(data2) %in% bad_snp)]
  
  plot_data <- matrix(NA, nrow = nrow(data1), ncol = ncol(data1))
  plot_data[upper.tri(plot_data)] <- data1[upper.tri(data1, diag = FALSE)]
  plot_data[lower.tri(plot_data)] <- data2[upper.tri(data2, diag = FALSE)]
  row.names(plot_data) <- row.names(data1)
  colnames(plot_data) <- colnames(data1)
  
}