module Evaluator where

import Genome

data Evaluator =
  Evaluator { genome       :: Genome
            , evalfn       :: Genome -> Double
            }