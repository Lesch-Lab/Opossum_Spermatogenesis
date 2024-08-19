# Opossum_Spermatogenesis
This is a collection of scripts for analyzing and comparing RNA-seq data collected from multiple species corresponding to analyses described in Marshall et al., Dev Cell 2024. For file paths and file names, many have been listed as "pathto", "filename", or "OutFileName" which should be replaced based on file names and path structure for each user. Some scripts contain specific file names (example "UpMm.txt") that should be substituted to represent user data. 

## AlignmentHISAT_Stringtie
RNA-sequencing alignment and counting was performed using HISAT and StringTie along with SAMtools. Input is species-specific HISAT index and RNA-seq fastq files.

## SingleCell_Jupyter.ipynb
Single cell RNA-seq data was processed and visualized using Scanpy and other python programs. 

## zFPKM_Normalization.R
zFPKM is a method of normalizing gene expression data developed by Hart et al. [1]. This method allows for normalization within samples for comparative analyses across libriraries, experiments, and species. This requires FPKM expression values for input. I modified code available from Uebbing et al. [2] 
 
[1] Hart et al. 2013: Finding the active genes in deep RNA-seq gene expression studies. BMC Genomics 14:778.

[2] Uebbing et al. 2016: Divergence in gene expression within and between two closely related flycatcher species. Mol Ecol 25, 2015–2028. https://github.com/severinEvo/gene_expression/blob/master/zFPKM.R

## Pst_Calc.R
Pst is a comparison of within-population variance and between-population variance to compare quantitative differences, in this case normalized gene expression levels. In this study, it is used to identify differentially expressed genes between mouse and opossum. Methodology is from Antoniazza et al [3] and code modified from Uebbing et al [2].

Data input was a matrix of zFPKM normalized expression values by gene (row) for each individual (column) and a corresponding list of species classification for each individual. 

[2] Uebbing et al. 2016. Divergence in gene expression within and between two closely related flycatcher species. Mol Ecol 25, 2015–2028. https://github.com/severinEvo/gene_expression/blob/master/Pst.R

[3] Antoniazza et al. 2010. Local adaptation maintains clinal variation in melanin-based coloration of European barn owls (Tyto alba). Evolution 64, 1944–1954

## Pst_Bootstrap.R
This includes the code run on a computing cluster to perform a bootstrap analysis for the Pst calculations and the following statistical calculations and filtering. 

## OpossumDESeqFirstWave.Rmd
R notebook of detailed differential gene expression analysis of data from first wave spermatogenesis developmental time series in opossum testis. This is a modification of the DESeq2 manual instructions (https://bioconductor.org/packages/release/bioc/html/DESeq2.html). 

## PhyloDivergence.Rmd
This is an R Noteboook for six-species comparisons of transcriptomic divergence including:
•	Spearman’s correlations
•	creating and plotting trees based on transcriptional divergence
•	bootstrapping trees for node distribution as well as branch length
•	statistical analysis

Data input includes gene IDs for orthologs across species, gene lists for genes with conserved expression, higher expression in each group, zFPKM normalized counts for each sample (column) by gene (row).

## PhastCons_PhyloP.Rmd
R and linux scripts to calculate Phastcons and PhyloP conservation scores for promoter regions of gene groups of interest.
