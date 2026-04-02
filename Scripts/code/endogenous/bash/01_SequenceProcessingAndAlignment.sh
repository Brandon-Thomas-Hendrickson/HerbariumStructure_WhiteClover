#!/bin/sh

source activate alignment_conda

FASTQCDIR=/HerbariumStructure/sequences/fastQC/herbarium
FASTP=Software/fastp
RAW=/HerbariumStructure/sequences/raw_herbarium
TRIMDIR=/HerbariumStructure/sequences/trimmed
FASTPDIR=/HerbariumStructure/sequences/fastp_report
CONTGENOME=Genome/all_genera_index
GENOME=Genome/GCA_030408175.1_UTM_Trep_v1.0_genomic.fna
ALIGNDIR=/HerbariumStructure/aligned/raw/herbarium
DECONDIR=/HerbariumStructure/sequences/clean
CONTDIR=/HerbariumMicrobes/Aligned/exogenous/herbarium/raw_bam
PROCDIR=/HerbariumStructure/aligned/processed/herbarium
S1=_1.fq.gz
S2=_2.fq.gz

#Run FastQC
for i in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt);
do fastqc $RAW/$i$S1 -o $FASTQCDIR/;
fastqc $RAW/$i$S2 -o $FASTQCDIR/;
done

#Trim Herbarium Sequence Reads
for i in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt);
do $FASTP -i $RAW/$i$S1 -I $RAW/$i$S2 -o $TRIMDIR/$i$S1 -O $TRIMDIR/$i$S2 --cut_right --dedup -h $FASTPDIR/$i'.html' -g -w 16;
done

#Run Decontamination Protocol
for i in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt);
do bowtie2 -p 16 -x $CONTGENOME -1 $TRIMDIR/$i$S1 -2 $TRIMDIR/$i$S2 
--un-conc-gz $DECONDIR/$i'_CLEAN' > $ALIGNDIR/$i'FUN_BAC_REMOVED'.sam;
Software/samtools-1.21/samtools view -hbS -F 4 $ALIGNDIR/$i'FUN_BAC_REMOVED.sam' > $CONTDIR/$i'_CONTAMINATED_SEQUENCES.bam';
rm $ALIGNDIR/$i'FUN_BAC_REMOVED.sam';
mv $DECONDIR/$i'_CLEAN.1' $DECONDIR/$i'_1.fq.gz';
mv $DECONDIR/$i'_CLEAN.2' $DECONDIR/$i'_2.fq.gz';
done 

#Run Alignment
for FILE in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt);
do bwa mem -t 16 $GENOME $DECONDIR/$FILE$S1 $DECONDIR/$FILE$S2 > $ALIGNDIR/$FILE'.sam';
Software/samtools-1.21/samtools view -bS $ALIGNDIR/$FILE'.sam' | Software/samtools-1.21/samtools sort -@ 48 -o $ALIGNDIR/$FILE'.bam';
rm $ALIGNDIR/$FILE'.sam';
done

#Group Sort the Bam Files
for FILE in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt);
do Software/samtools-1.21/samtools sort -n -@ 48 $ALIGNDIR/$FILE'.bam' -o $ALIGNDIR/$FILE'_grouped.bam';
done

#Run Fixmate on bamfiles 
for FILE in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt);
do 
  Software/samtools-1.21/samtools fixmate -@ 48 -m $ALIGNDIR/$FILE'_grouped.bam' $ALIGNDIR/$FILE'_fixmate.bam';
done

#Sort by Coordinate
for FILE in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt);
do Software/samtools-1.21/samtools sort -@ 48 -o $ALIGNDIR/$FILE'_fixmatesorted.bam' $ALIGNDIR/$FILE'_fixmate.bam';
done

#Mark Duplicates
for FILE in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt); 
do Software/samtools-1.21/samtools markdup -@ 48 $ALIGNDIR/$FILE'_fixmatesorted.bam' $PROCDIR/$FILE'_marked.bam'; 
done

# Index Bam Files
for FILE in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt); do
  if [ -f $PROCDIR/$FILE'_marked.bam' ]; then
    Software/samtools-1.21/samtools index -@ 48 -b $PROCDIR/$FILE'_marked.bam'
  fi
done

# Remove intermediate files only if the final bam file exists
for FILE in $(cat /HerbariumStructure/file_lists/herbarium/herbarium_list.txt); do
  if [ -f $PROCDIR/$FILE'_marked.bam' ]; then
    rm $ALIGNDIR/$FILE'_grouped.bam'
    rm $ALIGNDIR/$FILE'_fixmate.bam'
    rm $ALIGNDIR/$FILE'_fixmatesorted.bam'
  fi
done
