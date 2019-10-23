test_that("Test attr and url functions", {

    tryCatch({

        bbDir<-tempdir()

        clearGeneratedFiles(bbDir)
        setwd(bbDir)

        args<-testDatasetBBArgs(hgnc=TRUE)

        bbBuildData(rawArgs = args)

        bbStart()

        # bbAttr
        res<-bbAttr("tpi1","hgnc")
        expect_true(length(res)>=6)
        expect_true('locus_group' %in% res)

        res<-bbAttr("shh","ensembl")
        expect_true(length(res)>=9)
        expect_true('seq_region_name' %in% res)

        res<-bbAttr("ENST00000297261","transcript")
        expect_true(length(res)>=7)
        expect_true('start' %in% res)

        res<-bbAttr("ENSE00001146308","exon")
        expect_true(length(res)>=4)
        expect_true('start' %in% res)


        # bbURL
        res<-bbURL("ENSG00000164690","ensembl")
        expect_equal(res,"https://www.ensembl.org/homo_sapiens/Gene/Summary?db=core;g=ENSG00000164690")

        res<-bbURL("p15498","uniprot")
        expect_equal(res,"//www.uniprot.org/uniprot/p15498")

    }, finally = {
        bbStop()
    })

})
