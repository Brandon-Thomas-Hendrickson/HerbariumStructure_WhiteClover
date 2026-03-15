#!/bin/bash

#SBATCH --account loni_trpopgen03
#SBATCH --partition checkpt
#SBATCH --job-name Herb_PCA
#SBATCH --nodes=1
#SBATCH --ntasks=24
#SBATCH -t 3-00:00:00

ANGSDDIR="/work/calicraw/Projects/HerbariumStructure/ANGSD/output/herbarium_GLUE_spain/beagles"

pcangsd --beagle $ANGSDDIR/Herbarium_GLUE_SPAIN_FOURFOLDT2_updated_cleaned.beagle.gz --threads 24 --out /work/calicraw/Projects/HerbariumStructure/pcangsd/herb_GLUE_SPAIN_filtered_MAF05 --maf 0.05 --iter 200 > /work/calicraw/Projects/HerbariumStructure/pcangsd/pc_herb_GLUE_SPAIN_progMAF05.txt 2>&1

ADMIXDIR="/work/calicraw/Projects/HerbariumStructure/ngs_admix"

#Run NGSadmix
for K in {1..9}; do
  for seed in 21 1995 7142023 3169147 1964; do
        /work/calicraw/Software/angsd/misc/NGSadmix -likes $ANGSDDIR/Herbarium_GLUE_SPAIN_FOURFOLDT2_updated_cleaned.beagle.gz -K $K -P 48 -o $ADMIXDIR/ADMIX_HGS${K}.${seed} -minMaf 0.05 -seed $seed 
  done
done

#From the log output of NGSadmix, extract the K and the log likelihood
for K in {1..9}; do
  for seed in 21 1995 7142023 3169147 1964; do
    grep -A 1 "best like=" $ADMIXDIR/ADMIX_HGS${K}.${seed}.log | awk -v K=$K -v seed=$seed '{print K, seed, $2}'  
  done
done > $ADMIXDIR/K_likelihood.txt

sed -i 's/like=//g' $ADMIXDIR/K__HGS_likelihood.txt

awk '{print $1,$3}' $ADMIXDIR/K_HGS_likelihood.txt > $ADMIXDIR/clumppak_HGS_ready.txt
