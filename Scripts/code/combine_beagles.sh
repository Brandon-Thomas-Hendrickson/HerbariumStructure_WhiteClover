#!/bin/sh

#SBATCH --account loni_trpopgen03
#SBATCH --partition workq
#SBATCH --job-name G1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH -t 3-00:00:00

WORKDIR="/work/calicraw/Projects/HerbariumStructure/ANGSD/output/herbarium/beagles"

cd $WORKDIR

zcat G1_herb.beagle.gz | head -n 1 | gzip > combined_file.beagle.gz

for file in G{1..8}_herb.beagle.gz; do
  zcat "$file" | tail -n +2 | gzip -c >> combined_file.beagle.gz
done
