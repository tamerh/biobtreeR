test_that("Search test", {

    tryCatch({

    bbDir<-tempdir()

    clearGeneratedFiles(bbDir)
    setwd(bbDir)

    args<-testDatasetBBArgs(hgnc=TRUE)

    bbBuildData(rawArgs = args)

    bbStart()

    res<-bbSearch("tpi1,vav_human,ENST00000297261")

    expect_length(res,3)
    expect_length(res[1]$input,4)
    expect_length(res[2]$identifier,4)
    expect_length(res[3]$dataset,4)

    res<-res[order(res$dataset),]


    expected_res <- data.frame(input=c("TPI1","TPI1","ENST00000297261","VAV_HUMAN"),
                               identifier=c("ENSG00000111669","HGNC:12009","ENST00000297261","P15498"),
                           dataset=c("ensembl","hgnc","transcript","uniprot"))

    expect_true(all.equal(expected_res,res,check.attributes=FALSE))

    }, finally = {
        bbStop()
    })

})
