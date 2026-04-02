#!/bin/bash

FILESDIR="/work/calicraw/Projects/HerbariumStructure/file_lists/GLUE"
# File containing sample names and downsampling percentages
DOWNSAMPLE_FILE="downsampling_chloroplast_GLUE.txt"
BAMDIR="/work/calicraw/Projects/HerbariumMicrobes/Aligned/endogenous/GLUE/processed_bam/chloroplast/select"
# Directory for the downsampled BAM files
DOWNSAMPLED_DIR="/work/calicraw/Projects/HerbariumMicrobes/Aligned/endogenous/GLUE/processed_bam/chloroplast/downsampled"

# Loop through each line in the file
while read -r SAMPLE PERCENT; do
    # Extract the sample name without the directory or extension
    BASENAME=$(basename "$SAMPLE" .bam)

    # Run /work/calicraw/Software/samtools-1.21/samtools to downsample the BAM file
    /work/calicraw/Software/samtools-1.21/samtools view -s $PERCENT -b $BAMDIR/$SAMPLE > "$DOWNSAMPLED_DIR/${BASENAME}_downsampled.bam"

    echo "Downsampled $SAMPLE to $PERCENT and saved as ${BASENAME}_downsampled.bam"
done < $FILESDIR/$DOWNSAMPLE_FILE

