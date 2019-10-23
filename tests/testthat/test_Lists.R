test_that("List genomes test", {

    tryCatch({

        bbDir<-tempdir()

        clearGeneratedFiles(bbDir)
        setwd(bbDir)

        args<-testDatasetBBArgs2()

        bbBuildData(rawArgs = args)

        bbStart()

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


    }, finally = {
        bbStop()
    })

})

test_that("List datasets test ", {

    tryCatch({

        bbDir<-tempdir()

        clearGeneratedFiles(bbDir)
        setwd(bbDir)

        args<-testDatasetBBArgs2()

        bbBuildData(rawArgs = args)

        bbStart()

        res<- bbListDatasets()

        expect_true('uniprot' %in% res$id)
        expect_true('hgnc' %in% res$id)
        expect_true('entrez' %in% res$id)
        expect_true('ensembl' %in% res$id)
        expect_true('taxchild' %in% res$id)
        expect_true('transcript' %in% res$id)
        expect_true('exon' %in% res$id)

    }, finally = {

        bbStop()

    })

})
