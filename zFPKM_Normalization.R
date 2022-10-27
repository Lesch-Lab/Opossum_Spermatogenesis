#Normalizing using zFPKM

#zFPKM function
#Modified from S Uebbing et al : Divergence in gene expression within and between two closely related flycatcher species. Molecular Ecology 25:2015—2028. (https://github.com/severinEvo/gene_expression)

z_fpkm<-function(i){
  my<-density(i,na.rm=T)$x[which.max(density(i,na.rm=T)$y)]
  U<-mean(i[i>my],na.rm=T)
  sigma<-(U-my)*(.5*pi)^.5
  z<-(i-my)/sigma
  z[z< -3]<-NA
  return(z)
}


##format data for normalization

table$FPKM_sample <- table$FPKM_sample + 0.00001 #add negligible amount to prevent loss of zero values with log step
table$logFPKM_sample <- log2(table$FPKM_sample) #adds a column with log transformed values
table$zFPKM_sample <- z_fpkm(table$logFPKM_sample) #runs function to calculate zFPKM using log transformed values
