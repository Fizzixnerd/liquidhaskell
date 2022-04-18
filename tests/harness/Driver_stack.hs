import Test.Build

main :: IO ()
main = program stackTestEnv stackOutputStripper stackErrorStripper stackBuild
