rm(list =ls())
library(ape)
setwd("/home/eeb177-student/Desktop/eeb-177/project/sandbox/WHRN-Harm-PDZD7_comparisson/")



##################################
#defining a function to generate the trees
##################################
make_gene_tree_pdf <- function(gene_tree_file, gene_annotation_file, title, output_file_name){
  gene_tree <- read.tree(gene_tree_file)
  
  gene_annotations <- read.csv(gene_annotation_file, 
                               header =FALSE, 
                               stringsAsFactors = FALSE)
  names(gene_annotations) <- c("GeneID", "OrgName", "Annotation", "Group", "CombinedName", "Genus_Gene")
  rownames(gene_annotations) <- gene_annotations$GeneID
  
  order <- match(gene_tree$tip.label, rownames(gene_annotations))
  gene_annotations <- gene_annotations[,][order,]
  
  
  gene_tree$tip.label <- gene_annotations$Genus_Gene
  # Rename tips of Tree to increase Readability
  
  pdf(output_file_name, width = 9, height = 5)
  #create a pdf output for the graph
  par(mar=c(5.1, 4.1, 4.1, 8), xpd =TRUE) #this gives extra space to the right to add a legend
  
  plot(gene_tree, cex = 0.5, label.offset = 0.1)
  # decrease size of labels a bit
  
  title(title)
  #add a title
  add.scale.bar(x = 0, y = -0.1, lwd = 2, lcol = "black")
  #add a scale bar! this adds it in the margin outside the figure
  
  # Color the tips to indicate which lineage the organism belongs to
  
  gene_annotations$Group <- as.factor(gene_annotations$Group)
  
  #start by setting the Group to factor
  lineages <- unique(gene_annotations$Group)
  cols <- rainbow(n = length(lineages))
  #color vector for legend
  colvec <- cols[gene_annotations$Group]
  #color vector for tree
  tiplabels(pch = 19, col = colvec)
  
  
  # Add a legend!
  legend(x = "bottomright", 
         inset= c(-.1, 0), #this offsets the legend so that it is not covering up any of the names
         #lwd = 0, 
         pch = 19 , 
         legend = levels(lineages), 
         col = cols, 
         cex = 0.5) # resize the key
  dev.off()
}

####### make the tree
make_gene_tree_pdf(gene_tree_file = "RAxML_bestTree.WHRN_family1",
                   gene_annotation_file = "annotations_tree_nonredundant_WHRN_harm_PDZD7.txt",
                   title = "Whirlin-Harmonin-PDZD7 family tree",
                   output_file_name = "WHRN_harmonin_PDZD7_tree_figure.pdf")


Whirlin_family_tree = read.tree("/home/eeb177-student/Desktop/eeb-177/project/sandbox/WHRN-Harm-PDZD7_comparisson/RAxML_bestTree.WHRN_family1")
plot(Whirlin_family_tree)

