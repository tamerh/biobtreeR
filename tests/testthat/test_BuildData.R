test_that("Build data test", {

    wdir<-getwd()

    tryCatch(
        {
            if(exists("bbConfig",envir = biobtreeREnv)){
                remove("bbConfig",envir = biobtreeREnv)
            }

            bbDir<-tempdir()
            tempDir<-tempdir()
            clearGeneratedFiles(tempdir())
            setwd(bbDir)

            args<-testDatasetBBArgs()

            expect_false(file.exists(file.path(bbDir,"out","db","db.meta.json")))

            bbBuildData(outDir = bbDir, rawArgs = args)

            expect_true(file.exists(file.path(bbDir,"out","db","db.meta.json")))

            # test with user defined
            clearGeneratedFiles(bbDir)
            bbDir<-file.path(tempDir,"userOut")
            expect_true(dir.create(bbDir))

            setwd(bbDir)
            args<-testDatasetBBArgs() # need to build again with new path

            expect_false(file.exists(file.path(bbDir,"out","db","db.meta.json")))

            bbBuildData(rawArgs = args,outDir=bbDir)

            expect_true(file.exists(file.path(bbDir,"out","db","db.meta.json")))

        },finally = {
            setwd(wdir)
        }

    )




})

