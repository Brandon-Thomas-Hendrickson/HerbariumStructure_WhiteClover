#!/bin/sh

#SBATCH --account loni_trpopgen03
#SBATCH --partition workq
#SBATCH --job-name Heterozygosity
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH -t 3-00:00:00

# Output directory for ANGSD results
output_dir="/work/calicraw/Projects/HerbariumStructure/heterozygosity/output"

# Create a BAM list file
bam_list="/work/calicraw/Projects/HerbariumStructure/file_lists/herbarium_GLUE_spain/bam_list_Full.txt"

ANGSD="/work/calicraw/Software/angsd/angsd"

# Run ANGSD to calculate individual heterozygosity
$ANGSD -bam "$bam_list" -out "$output_dir/heterozygosity" -anc /work/calicraw/T_ANCESTRAL/T_ancestral.fa -doSaf 1 -GL 1 -doCounts 1 -P 8

# Calculate thetas and heterozygosity
/work/calicraw/Software/angsd/misc/realSFS saf2theta "$output_dir/heterozygosity.saf.idx" 

# Extract individual heterozygosity
/work/calicraw/Software/angsd/misc/thetaStat do_stat "$output_dir/heterozygosity.thetas.idx" -win 5000 -step 1000
