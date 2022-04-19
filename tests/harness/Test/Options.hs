module Test.Options where

import qualified Data.Text as T
import Test.Types hiding (Parser)
import Test.Groups
import Options.Applicative
import Data.List (intersperse)

options :: Parser Options
options = Options <$>
  (many (argument
         (T.pack <$> str)
          (metavar "TESTGROUPNAMES..."
           <> showDefault
           <> help ("Zero or more of: " <> mconcat (intersperse ", " (T.unpack <$> allTestGroupNames))))))

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc
   <> progDesc "Execute groups of tests.")
