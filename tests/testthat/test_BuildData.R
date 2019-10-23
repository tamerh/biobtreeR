test_that("Build data test", {

    bbDir<-tempdir()
    tempDir<-tempdir()
    clearGeneratedFiles(bbDir)
    setwd(bbDir)

    args<-testDatasetBBArgs()

    expect_false(file.exists(file.path(bbDir,"out","db","db.meta.json")))

    bbBuildData(rawArgs = args)

    expect_true(file.exists(file.path(bbDir,"out","db","db.meta.json")))

    # now test with user defined outdir instead of tempdir
    clearGeneratedFiles(bbDir)
    bbDir<-file.path(tempDir,"userOut")
    expect_true(dir.create(bbDir))

    setwd(bbDir)
    args<-testDatasetBBArgs() # need to build again with new path

    expect_false(file.exists(file.path(bbDir,"out","db","db.meta.json")))

    bbBuildData(rawArgs = args,outDir=bbDir)

    expect_true(file.exists(file.path(bbDir,"out","db","db.meta.json")))

})

