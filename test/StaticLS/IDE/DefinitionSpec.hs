module StaticLS.IDE.DefinitionSpec (spec) where

import StaticLS.HIE.File.Except
import StaticLS.IDE.Definition
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import Test.Hspec
import qualified TestImport as Test
import qualified TestImport.Assert as Test
import qualified TestImport.TestData as Test

spec :: Spec
spec =
    describe "Correctly retrieves definitions" $ do
        describe "All available sources" $ do
            it "retrieves the myFun definition from a different module" $ do
                staticEnv <- Test.initStaticEnv
                locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                mDefnLoc <- Test.assertRight "hie file read error" locs
                defnLoc <- Test.assertHead "no definition loc found" mDefnLoc
                expectedLoc <- Test.myFunDefLocation
                defnLoc `shouldBe` expectedLoc

            it "Returns a renderable err when there is a version mismatch" $ do
                staticEnv <- Test.initStaticEnvOpts Test.badGhcTestStaticEnvOptions
                eDefnInfo <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                defnErr <- Test.assertLeft "expected hie definition failure" eDefnInfo
                let isHieFileVersionException (HieFileVersionException{}) = True
                    isHieFileVersionException _ = False
                isHieFileVersionException defnErr `shouldBe` True
                pure ()

        describe "Missing sources" $ do
            describe "Finding sources with only hie files" $ do
                it "should still pull from hie files when missing hiedb" $ do
                    let emptyOpts =
                            StaticEnvOptions
                                { optionHieDbPath = Nothing
                                , optionHieFilesPath = Just (Test.hieDirGhcVer Test.ghcVerDir)
                                }
                    staticEnv <- Test.initStaticEnvOpts emptyOpts
                    locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                    mDefnLoc <- Test.assertRight "hie file read error" locs
                    defnLoc <- Test.assertHead "no definition loc found" mDefnLoc
                    expectedLoc <- Test.myFunDefLocation
                    defnLoc `shouldBe` expectedLoc

                it "should still pull from hie files with empty hiedb" $ do
                    let emptyOpts =
                            StaticEnvOptions
                                { optionHieDbPath = Just "./TestData/not-a-real-hiedb-file"
                                , optionHieFilesPath = Just ".hiefiles"
                                }
                    staticEnv <- Test.initStaticEnvOpts emptyOpts
                    locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                    mDefnLoc <- Test.assertRight "hie file read error" locs
                    defnLoc <- Test.assertHead "no definition loc found" mDefnLoc
                    expectedLoc <- Test.myFunDefLocation
                    defnLoc `shouldBe` expectedLoc

            it "does not crash with missing all sources" $ do
                let emptyOpts =
                        StaticEnvOptions
                            { optionHieDbPath = Nothing
                            , optionHieFilesPath = Nothing
                            }
                staticEnv <- Test.initStaticEnvOpts emptyOpts
                locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                locs `shouldBe` Right []
