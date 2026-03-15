#!/bin/sh

#SBATCH --account loni_trpopgen03
#SBATCH --partition workq
#SBATCH --job-name Chloroplast_haplo
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH -t 3-00:00:00

source /home/calicraw/miniconda3/etc/profile.d/conda.sh
conda activate alignment_conda

# Define the reference genome
REFERENCE="/work/calicraw/Genome/chloroplast_genome.fna"

# Output VCF file for all samples
COMBINED_VCF="/work/calicraw/Projects/HerbariumStructure/aligned/chloroplast/variants/herbarium_chloroplast_variants.vcf"

#BAM files directory
BAM_DIR="/work/calicraw/Projects/HerbariumStructure/aligned/processed/herbarium/chloroplast"

cd /work/calicraw/Projects/HerbariumStructure/aligned
# Create a list of sorted BAM files
ls $BAM_DIR/*.bam > bam_list.txt

# Call variants for all BAM files with haploid ploidy
bcftools mpileup -f $REFERENCE -b bam_list.txt --ploidy 1 | bcftools call -mv -Ov -o $COMBINED_VCF

# Filter the VCF file
FILTERED_VCF="/work/calicraw/Projects/HerbariumStructure/aligned/chloroplast/variants/herbarium_chloroplast_filtered_variants.vcf"
bcftools filter -i 'QUAL > 30' $COMBINED_VCF -o $FILTERED_VCF

# Generate haplotypes in FASTA format
HAPLOTYPE_FASTA="/work/calicraw/Projects/HerbariumStructure/aligned/chloroplast/haplotypes/herbarium_chloroplast_haplotypes.fasta"
bcftools consensus -f $REFERENCE $FILTERED_VCF > $HAPLOTYPE_FASTA

module load R

Rscript /work/calicraw/Projects/HerbariumStructure/code/chloroplast/pegas_run.r

