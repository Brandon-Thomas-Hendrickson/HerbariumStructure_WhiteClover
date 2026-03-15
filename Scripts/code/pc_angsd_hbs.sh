#!/bin/bash
#SBATCH --account loni_trpopgen03
#SBATCH --partition checkpt
#SBATCH --job-name Herb_BWA
#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH -t 2-00:00:00

cd /work/calicraw/Projects

/work/calicraw/Software/angsd/angsd \
-nThreads 8 \
-bam /work/calicraw/Projects/hb7a1_location_bam_list.txt \
-r CP125842.1:50000-1350000 \
-out hb7a1_beagle \
-GL 2 \
-doMajorMinor 1 \
-doCounts 1 \
-doGLF 2 \
-SNP_pval 1e-6 \
-doMaf 2 \
-doGeno -1 \
-doPost 1 \
-minMapQ 30 \
-minQ 20 \
-minMaf 0.05

/work/calicraw/Software/angsd/angsd \
-nThreads 8 \
-bam /work/calicraw/Projects/hb7a2_location_bam_list.txt \
-r CP125842.1:3850000-8450000 \
-out hb7a2_beagle \
-GL 2 \
-doMajorMinor 1 \
-doCounts 1 \
-doGLF 2 \
-SNP_pval 1e-6 \
-doMaf 2 \
-doGeno -1 \
-doPost 1 \
-minMapQ 30 \
-minQ 20 \
-minMaf 0.05

/work/calicraw/Software/angsd/angsd \
-nThreads 8 \
-bam /work/calicraw/Projects/hb7b_location_bam_list.txt \
-r CP125842.1:50850000-54450000 \
-out hb7b_beagle \
-GL 2 \
-doMajorMinor 1 \
-doCounts 1 \
-doGLF 2 \
-SNP_pval 1e-6 \
-doMaf 2 \
-doGeno -1 \
-doPost 1 \
-minMapQ 30 \
-minQ 20 \
-minMaf 0.05

/work/calicraw/Software/angsd/angsd \
-nThreads 8 \
-bam /work/calicraw/Projects/hb9_location_bam_list.txt \
-r CP125844.1:3950000-5050000 \
-out hb9_beagle \
-GL 2 \
-doMajorMinor 1 \
-doCounts 1 \
-doGLF 2 \
-SNP_pval 1e-6 \
-doMaf 2 \
-doGeno -1 \
-doPost 1 \
-minMapQ 30 \
-minQ 20 \
-minMaf 0.05

/work/calicraw/Software/angsd/angsd \
-nThreads 8 \
-bam /work/calicraw/Projects/hb13_location_bam_list.txt \
-r CP125848.1:50000-1750000 \
-out hb13_beagle \
-GL 2 \
-doMajorMinor 1 \
-doCounts 1 \
-doGLF 2 \
-SNP_pval 1e-6 \
-doMaf 2 \
-doGeno -1 \
-doPost 1 \
-minMapQ 30 \
-minQ 20 \
-minMaf 0.05

pcangsd -t 8 -b hb7a1_beagle.beagle.gz -o hb7a1_cov
pcangsd -t 8 -b hb7a2_beagle.beagle.gz -o hb7a2_cov
pcangsd -t 8 -b hb7b_beagle.beagle.gz -o hb7b_cov
pcangsd -t 8 -b hb9_beagle.beagle.gz -o hb9_cov
pcangsd -t 8 -b hb13_beagle.beagle.gz -o hb13_cov
