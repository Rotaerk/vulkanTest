{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake/" } $ do
  want ["build", "shaders"]

  "clean" ~> do
    cmd_ "cabal" ["clean"]
    removeFilesAfter ".shake" ["//*"]

  "build" ~> cmd "cabal" ["build"]

  forM executableNames $ \name -> do
    "run-" ++ name ~> do
      need ["shaders"]
      cmd "cabal" ["run", name]

  "shaders" ~> do
    need =<< fmap shaderSrcPathToOutPath <$> getDirectoryFiles "shaders" ["*.vert", "*.frag"]

  shadersOutDir </> "*.spv" %> \outPath@(shaderOutPathToSrcPath -> srcPath) -> do
    need [srcPath]
    cmd "glslangValidator" ["-V", srcPath, "-o", outPath]

executableNames :: [String]
executableNames =
  [
    "sandbox",
    "triangle"
  ]

shadersSrcDir :: FilePath
shadersSrcDir = "shaders"

shadersOutDir :: FilePath
shadersOutDir = "data/shaders"

shaderSrcPathToOutPath :: FilePath -> FilePath
shaderSrcPathToOutPath srcPath = shadersOutDir </> takeFileName srcPath <.> "spv"

shaderOutPathToSrcPath :: FilePath -> FilePath
shaderOutPathToSrcPath outPath = shadersSrcDir </> takeBaseName outPath
