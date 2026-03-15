#!/bin/sh

#SBATCH --account loni_trpopgen03
#SBATCH --partition workq
#SBATCH --job-name bamstats
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH -t 3-00:00:00

SAMTOOLS="/work/calicraw/Software/samtools-1.21/samtools"
PROCDIR="/work/calicraw/Projects/HerbariumStructure/aligned/processed/herbarium"
CHLORODIR="/work/calicraw/Projects/HerbariumStructure/aligned/processed/herbarium/chloroplast"
MITODIR="/work/calicraw/Projects/HerbariumStructure/aligned/processed/herbarium/mitochondria"
NUCDIR="/work/calicraw/Projects/HerbariumStructure/aligned/processed/herbarium/nuclear"
BAMLIST="/work/calicraw/Projects/HerbariumStructure/aligned/herbarium_bam_list.txt"

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125853.1" > $CHLORODIR/${i%_marked.bam}'_Chloroplast.bam'; done 

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125852.1" > $MITODIR/${i%_marked.bam}'_Mitochondria.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125836.1" "CP125837.1" > $NUCDIR/${i%_marked.bam}'_C1.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125838.1" "CP125839.1" > $NUCDIR/${i%_marked.bam}'_C2.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125840.1" "CP125841.1" > $NUCDIR/${i%_marked.bam}'_C3.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125842.1" "CP125843.1" > $NUCDIR/${i%_marked.bam}'_C4.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125844.1" "CP125845.1" > $NUCDIR/${i%_marked.bam}'_C5.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125846.1" "CP125847.1" > $NUCDIR/${i%_marked.bam}'_C6.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125848.1" "CP125849.1" > $NUCDIR/${i%_marked.bam}'_C7.bam'; done

for i in $(cat $BAMLIST); do $SAMTOOLS view -b $PROCDIR/$i "CP125850.1" "CP125851.1" > $NUCDIR/${i%_marked.bam}'_C8.bam'; done
