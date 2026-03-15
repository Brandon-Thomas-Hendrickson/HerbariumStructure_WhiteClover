install.packages("pegas")
library(pegas)

# Read haplotypes
haplotypes <- read.dna("/work/calicraw/Projects/HerbariumStructure/aligned/haplotypes/herbarium_chloroplast_haplotypes.fasta", format = "fasta")

# Identify haplotypes
haps <- haplotype(haplotypes)

# Generate haplotype network
haploNet <- haploNet(haps)

# Save the plot as a PDF in the same directory as the haplotypes file
pdf("/work/calicraw/Projects/HerbariumStructure/aligned/haplotypes/haplotype_network.pdf")
plot(haploNet, size = attr(haploNet, "freq"))
dev.off()
