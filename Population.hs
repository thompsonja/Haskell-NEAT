module Population where

import Genome
import ConnectionGene

type Population = [Genome]

createPopulation numGenomes inputs outputs = take numGenomes (repeat $ createInitialGenome inputs outputs)

getLastInnovationId = (+1) . maximum . map (innovationId) . concat