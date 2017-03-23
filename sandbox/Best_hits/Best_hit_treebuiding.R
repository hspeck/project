
rm(list =ls())
library(ape)
setwd("/home/eeb177-student/Desktop/eeb-177/project/sandbox/Best_hits")



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


#######
# I'm using the shell to gather the files of interest and make a doc containing their names
#for annotaitons can get list with 
# ls | grep "^annotations" > tree_list_annotation_files.txt

#below gives us the files for the tree annotations
#ls raxml_tree_files/ > tree_list_raxml_files.txt
#########


#now we have the file, let's read it in so we can automate the tree building

index_of_files <- read.csv("/home/eeb177-student/Desktop/eeb-177/project/sandbox/Best_hits/tree_building_best_hits_index_of_files", 
                           header=FALSE, 
                           stringsAsFactors = FALSE)
names(index_of_files) <- c("annotation_files", "tree_files", "figure_title", "output_file_name_pdf", "output_file_name_jpg")



#### Now we actually make the files, PDFS to begin

for(gene in(1:nrow(index_of_files))){
  make_gene_tree_pdf(index_of_files$tree_files[gene], 
                     index_of_files$annotation_files[gene],
                     index_of_files$figure_title[gene],
                     index_of_files$output_file_name_pdf[gene])
}

