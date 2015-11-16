import CodeGen(genx86)
import System.Environment
import System.Exit

main = do
  args <- getArgs
  if length args /= 2 then print "Usage: ./Driver <inputFile> <outputFile>" >> (exitWith $ ExitFailure 1)
    else do
       inFile <- readFile $ args !! 0
       code <- return $ genx86 inFile
       writeFile (args !! 1) code
       exitSuccess
