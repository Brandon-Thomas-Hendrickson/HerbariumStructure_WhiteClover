#!/bin/sh

source /miniconda3/etc/profile.d/conda.sh
conda activate alignment_conda

# Define the reference genome
REFERENCE="/Genome/GCA_030408175.1_UTM_Trep_v1.0_genomic.fna"

# Output VCF file for all samples
COMBINED_VCF="/HerbariumStructure/aligned/chloroplast/variants/chloroplast_variants.vcf"

#BAM files directory
BAM_DIR="/HerbariumStructure/aligned/processed/chloroplast_GLUE_spain_Herb"

cd /HerbariumStructure/aligned
# Create a list of sorted BAM files
ls $BAM_DIR/*.bam > bam_list.txt

# Call variants for all BAM files with haploid ploidy
bcftools mpileup -f $REFERENCE -b bam_list.txt -Ou -o temp.bcf --threads 16 -d 300
bcftools call -mv -Ov -o $COMBINED_VCF --ploidy 1 temp.bcf


# Filter the VCF file
FILTERED_VCF="/HerbariumStructure/aligned/chloroplast/variants/chloroplast_filtered_variants.vcf"
bcftools filter -i 'QUAL > 20' $COMBINED_VCF -o $FILTERED_VCF

# Compress the filtered VCF file
bgzip -c $FILTERED_VCF > ${FILTERED_VCF}.gz

# Index the compressed VCF file
tabix -p vcf ${FILTERED_VCF}.gz
