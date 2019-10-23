test_that("Mapping test", {

    tryCatch({

    bbDir<-tempdir()

    clearGeneratedFiles(bbDir)
    setwd(bbDir)

    args<-testDatasetBBArgs(hgnc=TRUE)

    bbBuildData(rawArgs = args)

    bbStart()

    res<-bbMapping("AT5G3_HUMAN",'map(go)',attrs = "type")

    expect_length(res,2)
    expect_length(res[1]$mapping_id,10)
    expect_length(res[2]$type,10)

    res<-res[order(res$mapping_id),]

    expected_res <- data.frame(mapping_id=c("GO:0000276","GO:0005741","GO:0006754","GO:0008289",
                                            "GO:0015986","GO:0016021","GO:0042407","GO:0042776",
                                            "GO:0045263","GO:0046933"))

    expected_res<-expected_res[order("mapping_id")]

    expect_true(all.equal(expected_res["mapping_id"],res["mapping_id"],check.attributes=FALSE))

    # Test filtering
    expected_res <- data.frame(mapping_id=c("GO:0006754","GO:0015986","GO:0042407","GO:0042776"))

    expected_res<-expected_res[order("mapping_id")]

    res <-bbMapping("AT5G3_HUMAN",'map(go).filter(go.type=="biological_process")',attrs = "type")
    res<-res[order(res$mapping_id),]

    expect_true(all.equal(expected_res["mapping_id"],res["mapping_id"],check.attributes=FALSE))

    }, finally = {
        bbStop()
    })


})
