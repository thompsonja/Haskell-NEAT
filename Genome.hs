-- Genome.hs: Genome is essentially a list of ConnectionGenes

module Genome where

--import Random(randomR, StdGen, mkStdGen, newStdGen)
import ConnectionGene
import NodeGene

import Data.List

type Genome = [ConnectionGene]

addConnection genome from to weight inno = (ConnectionGene from to weight True inno):genome

disableConnection toDisable genome = disabledGene : filter (/= toDisable) genome
    where disabledGene = (ConnectionGene (from toDisable) (to toDisable) (weight toDisable) False (innovationId toDisable))

addNode genome index inno = firstNewConnection : secondNewConnection : genome
    where connectionToSplit = genome !! index
          newNode = (NodeGene Hidden 0.0 (1 + getLastNodeId genome))
          firstNewConnection = (ConnectionGene (from connectionToSplit) newNode 1.0 True (inno))
          secondNewConnection = (ConnectionGene newNode (to connectionToSplit) (weight connectionToSplit) True (inno+1))

getLastNodeId genome = max (maximum $ map (nodeId) $ map (from) genome) (maximum $ map (nodeId) $ map (to) genome)

test = maximum . map (innovationId)

createInitialGenome inputs outputs = zipWith ($) partialConstructors [0..inputs*outputs-1]
    where partialConstructors = [(ConnectionGene a b 1.0 True) | a <- sensorNodes, b <- outputNodes]
          sensorNodes = map (NodeGene Sensor 0.0) [0..inputs-1]
          outputNodes = map (NodeGene Output 0.0) [inputs..inputs+outputs-1]

getNodeCounts genome = [sensor, hidden, output]
    where allNodes = nub $ map from genome ++ map to genome
          allNodeTypes = map nodeType allNodes
          sensor = length . filter (==Sensor) $ allNodeTypes
          hidden = length . filter (==Hidden) $ allNodeTypes
          output = length . filter (==Output) $ allNodeTypes
