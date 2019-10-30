test_that("List genomes test", {

        if(exists("bbConfig",envir = biobtreeREnv)){
                remove("bbConfig",envir = biobtreeREnv)
        }

        res<- bbListGenomes("ensembl")

        expect_true('homo_sapiens' %in% res)
        expect_true('mus_musculus' %in% res)
        expect_true('xenopus_tropicalis' %in% res)
        expect_true('xiphophorus_maculatus' %in% res)

        res<- bbListGenomes("ensembl_fungi")

        expect_true('ashbya_gossypii' %in% res)
        expect_true('saccharomyces_cerevisiae' %in% res)
        expect_true('pyrenophora_teres' %in% res)
        expect_true('ustilago_maydis' %in% res)

        res<- bbListGenomes("ensembl_plants")

        expect_true('arabidopsis_thaliana' %in% res)
        expect_true('oryza_sativa' %in% res)
        expect_true('cynara_cardunculus' %in% res)
        expect_true('zea_mays' %in% res)

        res<- bbListGenomes("ensembl_protists")

        expect_true('phytophthora_parasitica' %in% res)
        expect_true('entamoeba_histolytica' %in% res)
        expect_true('pythium_ultimum' %in% res)
        expect_true('trypanosoma_brucei' %in% res)

        res<- bbListGenomes("ensembl_bacteria")

        expect_true('salmonella_enterica' %in% res)
        expect_true('dissulfuribacter_thermophilus' %in% res)
        expect_true('corynebacterium_pseudotuberculosis' %in% res)
        expect_true('yersinia_rohdei' %in% res)

})

test_that("List datasets test ", {

        if(exists("bbConfig",envir = biobtreeREnv)){
                remove("bbConfig",envir = biobtreeREnv)
        }

        res<- bbListDatasets()

        expect_true('uniprot' %in% res$id)
        expect_true('hgnc' %in% res$id)
        expect_true('entrez' %in% res$id)
        expect_true('ensembl' %in% res$id)
        expect_true('taxchild' %in% res$id)
        expect_true('transcript' %in% res$id)
        expect_true('exon' %in% res$id)

})

test_that("List attr test", {

        if(exists("bbConfig",envir = biobtreeREnv)){
                remove("bbConfig",envir = biobtreeREnv)
        }

        res<-bbListAttrs("hgnc")
        expect_true(length(res)>=6)
        expect_true('locus_group' %in% res)

        res<-bbListAttrs("ensembl")
        expect_true(length(res)>=9)
        expect_true('seq_region_name' %in% res)

        res<-bbListAttrs("transcript")
        expect_true(length(res)>=7)
        expect_true('start' %in% res)

        res<-bbListAttrs("exon")
        expect_true(length(res)>=4)
        expect_true('start' %in% res)

})
