#!/bin/sh

ANGSDDIR="/HerbariumStructure/ANGSD/output/herbarium/beagles"
ANGSD="/Software/angsd/angsd"
GENOME="/Genome/GCA_030408175.1_UTM_Trep_v1.0_genomic.fna.fai"
BAMLIST="/HerbariumStructure/file_lists/herbarium/G1_bam_list.txt"
BAMDIR="/HerbariumStructure/aligned/processed/herbarium/nuclear"
GENOMEREF="/Genome/GCA_030408175.1_UTM_Trep_v1.0_genomic.fna"

$ANGSD -ref $GENOMEREF -fai $GENOME -bam $BAMLIST -SNP_pval 1e-6 -doMajorMinor 4 -minQ 20 -minMapQ 30 -doPost 1 -GL 1 -doGlf 2 -minMaf 0.05 -doCounts 1 -doMaf 2 -P 8 -out $ANGSDDIR/G1_herb
