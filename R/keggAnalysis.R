library(fgsea)

do_kegg_analysis <- function(genes_, scores_, species_, title_) {
    species_code <- switch(species_, mouse = 'Mm', human = 'Hs')
    species_code_long <-
      switch(species_, mouse = 'mmu', human = 'hsa')

    print('mappedkeys(KEGGPATHID2EXTID) = ')
    print(mappedkeys(KEGGPATHID2EXTID))
    kegg_pathways <-
      mappedkeys(KEGGPATHID2EXTID) %>% str_subset(species_code_long) %>% str_replace(sprintf('^%s(.*)$', species_code_long), '\\1')
    print('kegg_pathways = ')
    print(kegg_pathways)
    kegg_names <-
      KEGGPATHID2NAME[kegg_pathways] %>% as.list %>% simplify
    kegg_to_eg <-
      KEGGPATHID2EXTID[kegg_pathways %>% {
        sprintf('%s%s', species_code_long, .)
      }] %>% as.list
    names(kegg_to_eg) <- kegg_names

    print('kegg_to_eg = ')
    print(kegg_to_eg)

    SYMBOL2EG <-
      eval(parse(text = sprintf(
        'org.%s.egSYMBOL2EG', species_code
      )))

    names(scores_) <- genes_

    genes <- intersect(genes_, mappedkeys(SYMBOL2EG))

    scores_ <- scores_[genes]

    gene_entrez <-
      genes %>% SYMBOL2EG[.] %>% as.list %>% map( ~ .[1]) %>% simplify

    names(scores_) <- gene_entrez

    dput(genes)

    print('scores_ = ')
    print(scores_)

    fgseaRes <- fgsea(kegg_to_eg, scores_, nperm = 10000)

    ggdat <-
      fgseaRes %>% as.data.frame %>% as_data_frame %>% arrange(-abs(NES)) %>% head(20) %>% mutate(pathway =
                                                                                                    fct_inorder(pathway))
    print('ggdat = ')
    print(ggdat)

    ggplot(ggdat) +
      geom_point(aes(
        x = pathway,
        y = abs(NES),
        size = size
      )) +
      labs(title = paste("KEGG:", title_),
           x = 'Pathway',
           y = 'Absolute Normalized Enrichment Score') +
      scale_size_continuous(name = 'Size of\nthe pathway') +
      theme_grey(base_size = 20) +
      theme(axis.text.x = element_text(angle = -30, hjust = 0),
            plot.margin = margin(l = 50))
}
