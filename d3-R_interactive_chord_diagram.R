# d3/R Chord Diagram of White House Petition Data
# Taken from: http://data-steve.github.io/d3-r-chord-diagram-of-white-house-petitions-data/
# Implemented by: H. Achicanoy
# Date: 2016

if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("mattflor/chorddiag")
pacman::p_load(dplyr, magrittr, ggplot2, tidyr, curl)

curl::curl_download(
  "https://github.com/yoni/r_we_the_people/blob/master/data/petitions.RData?raw=true"
  , destfile="C:/Users/haachicanoy/Desktop/petitions.RData" )
load("C:/Users/haachicanoy/Desktop/petitions.RData")

# recover tag names and ids
p <- petitions   # save some typing
ids_names <- rbind(    
  p[, c("issues1.id", "issues1.name")] %>% setNames(c("ids", "names"))
  , p[, c("issues2.id", "issues2.name")] %>% setNames(c("ids", "names"))
  , p[, c("issues3.id", "issues3.name")]%>% setNames(c("ids", "names"))
) %>%
  unique() %>% na.omit()

# get only petitions with multi-tags
tag_count <- p %>%              
  select(id, issues1.id, issues2.id, issues3.id) %>%
  tidyr::gather(order, cats, -id) %>%
  filter(!is.na(cats)) %>%
  mutate(order = tidyr::extract_numeric(order)) %>%
  left_join(ids_names, by=c("cats"="ids"))

xtab_tag <- tag_count %>%
  count(names) %>%
  arrange(desc(n))

xtab_tag %>%
  ggplot2::ggplot(aes(x=factor(names,levels=names),y=n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.margin = unit(c(10,10,20,20), "mm"),
        plot.title = element_text(size = 20))  +
  ggtitle("Distribution of All Tags") +
  labs(x = "petition categories",
       y = "category counts")

# list of tags
tags <- sort(unique(ids_names$names))

# matrix to hold counts
mat <- matrix(0,nrow=nrow(tag_count),ncol=length(tags))
colnames(mat) <- tags

# get columns with tags from dataframe
p_id_nam <- p %>%
  select(contains(".name")) %>%
  mutate(issues1.name= ifelse(is.na(issues1.name), issues.name, issues1.name)) %>%
  mutate_each(funs(ifelse(is.na(.), "", .)), starts_with("issues"))

# make matrix
for (i in seq_along(tags)) {
  for (j in c(1,2,3)){ # 1,2,3 are columns I want
    mat[,i] <- as.numeric(tags[i]==p_id_nam[[j]]) +  mat[,i]
    is.na(mat[,i]) <- 0
  }
}

adjmat <- t(mat) %*% mat

# set number of colors needed
colorCount <- length(tags)

# makes function to create palette
getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

# manage use of diagonal cells in adj_mat
remove_diags <- function(mat, rm.lower = TRUE, ...) {
  diag(mat) <- 0
  if (isTRUE(rm.lower)) mat[lower.tri(mat)] <- 0
  mat
}

# ## order plot layering by smallest to largest so larges are on top
ord <- order(rowSums(remove_diags(adjmat, FALSE)))

# with the diags means there's a return
chorddiag::chorddiag(adjmat[ord, ord], margin = 150, showTicks =FALSE
                     , groupnameFontsize = 8  # have to shrink font for web viewing
                     , groupnamePadding = 5
                     , groupThickness = .05
                     , chordedgeColor = "gray90"
                     , groupColors = getPalette(colorCount))

# without the diags means there's NOT return
chorddiag::chorddiag(remove_diags(adjmat[ord, ord], FALSE), margin = 150, showTicks =FALSE
                     , groupnameFontsize = 8       
                     , groupnamePadding = 5
                     , groupThickness = .05
                     , chordedgeColor = "gray90"
                     , groupColors =getPalette(colorCount))

save.image(file = 'd3-R_interactive_chord_diagramImage.RData')
