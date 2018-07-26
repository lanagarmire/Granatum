library(fgsea)

do_geneOntology_analysis <- function(genes_, scores_, species_, title_) {
    species_code <- switch(species_, mouse = 'Mm', human = 'Hs')

    SYMBOL2EG <-
      eval(parse(text = sprintf(
        'org.%s.egSYMBOL2EG', species_code
      )))
    GO2ALLEGS <-
      eval(parse(text = sprintf(
        'org.%s.egGO2ALLEGS', species_code
      )))

    names(scores_) <- genes_

    genes <- intersect(genes_, mappedkeys(SYMBOL2EG))

    scores_ <- scores_[genes]

    print(length(genes))

    gene_entrez <-
      genes %>% SYMBOL2EG[.] %>% as.list %>% map( ~ .[1]) %>% simplify

    print(length(gene_entrez))

    names(scores_) <- gene_entrez

    go_terms <-
      intersect(mappedkeys(GO2ALLEGS), mappedkeys(GOTERM))
    go_to_eg <- GO2ALLEGS[go_terms] %>% as.list
    names(go_to_eg) <-
      names(go_to_eg) %>% GOTERM[.] %>% as.list %>% map( ~ .@Term) %>% simplify
    fgseaRes <-
      fgsea(
        go_to_eg,
        scores_,
        nperm = 10000,
        minSize = 50,
        maxSize = 500
      )
    ggdat <-
      fgseaRes %>% as.data.frame %>% as_data_frame %>% arrange(-abs(NES)) %>% head(20) %>% mutate(pathway =
                                                                                                    fct_inorder(pathway))
    ggplot(ggdat) +
      geom_point(aes(
        x = pathway,
        y = abs(NES),
        size = size
      )) +
      labs(title = paste("GO:", title_),
           x = 'Gene set',
           y = 'Absolute Normalized Enrichment Score') +
      scale_size_continuous(name = 'Gene set\nsize') +
      theme_grey(base_size = 20) +
      theme(axis.text.x = element_text(angle = -30, hjust = 0),
            plot.margin = margin(l = 50))
}
