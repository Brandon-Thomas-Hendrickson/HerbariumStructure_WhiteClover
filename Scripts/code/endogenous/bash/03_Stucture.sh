#!/bin/sh

WORKDIR="/HerbariumStructure/ANGSD/output/herbarium_GLUE_spain/beagles"

cd $WORKDIR

zcat G1_all.beagle.gz | head -n 1 | gzip > combined_all.beagle.gz

for file in G{1..8}_all.beagle.gz; do
  zcat "$file" | tail -n +2 | gzip -c >> combined_all.beagle.gz
done

zcat combined_all.beagle.gz | awk 'NR==FNR {a[$1]; next} $1 in a' /HerbariumStructure/file_lists/herbarium/markers_fourfolddegen.txt - | gzip > combined_all_FourFold.beagle.gz

pcangsd --beagle combined_all_FourFold.beagle.gz --threads 24 --out /HerbariumStructure/pcangsd/all_MAF05 --maf 0.05 --iter 1000

ADMIXDIR="/HerbariumStructure/ngs_admix"

#Run NGSadmix
for K in {1..9}; do
  for seed in 21 1995 7142023 3169147 1964; do
        /Software/angsd/misc/NGSadmix -likes combined_all_FourFold.beagle.gz -K $K -P 48 -o $ADMIXDIR/ADMIX_ALL${K}.${seed} -minMaf 0.05 -seed $seed
  done
done

#From the log output of NGSadmix, extract the K and the log likelihood
for K in {1..9}; do
  for seed in 21 1995 7142023 3169147 1964; do
    grep -A 1 "best like=" $ADMIXDIR/ADMIX_ALL${K}.${seed}.log | awk -v K=$K -v seed=$seed '{print K, seed, $2}'
  done
done > $ADMIXDIR/K_likelihood.txt

sed -i 's/like=//g' $ADMIXDIR/K__ALL_likelihood.txt

awk '{print $1,$3}' $ADMIXDIR/K_ALL_likelihood.txt > $ADMIXDIR/clumppak_ALL_ready.txt
