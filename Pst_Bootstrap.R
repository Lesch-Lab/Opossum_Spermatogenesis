#column number will change based on number of samples (here 11 and 13 are used)

y <- zFPKM_table
y <- cbind(y, Pst= pst_table$pst)
y.p <- y
y.p[,11]<-replace(y[,11],T,0)

for(i in 1:1000){
  nos<-sample(species)
  pst_new <- apply(zFPKM_table, 1, Pst, fixed=nos)
  pst_new <- as.data.frame(pst_new)
  pst_new_1 <- as.data.frame(t(pst_new))
  pst_new_1$V1[is.nan(pst_new_1$V1)] <-NA
  temp1 <- as.numeric(unlist(pst_new_1[,1]))
  temp2 <- which(temp1>y[,11] | is.na(temp1))
  y.p[temp2,11]<-y.p[temp2,11]+1
}

y.p <-y.p[,11]/1000
#takes the column that count the number of times the random Pst was higher than the real and divides by 1000 to get % of times higher

y <- cbind(y, p_val=y.p)
#adds the pvalue column to the larger data table


#this part adds a column, sets all values to zero, then makes it 1 if the pvalue is less than 0.05
y$sig <- y$p_val
y[,13]<-replace(y[,13],T,0)
temp3 <-which(y$p_val < 0.05)
y[temp3,13]<-y[temp3,13]+1
sum(y$sig) #shows number signifcant (unadjusted)

#Benjamini-Hochberg adjustment to the pval
y$BH <- p.adjust(y$p_val, method = "BH")

#combine table with original Pst to get vW and vB included
#filter for significance
y_sig <- subset(y, BH <= 0.05 & vB >= 3.5)
