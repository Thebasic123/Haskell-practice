module Test where


import Test.QuickCheck
import World
import Physics
import TestSupport
import Simulation

-- input 0 into unused 
-- if list is empty, then advanceWolrd is equal to world(also we can't divide by 0)
prop_EnergyConservation world = (worldEnergy world == worldEnergy (advanceWorld 0 0.001 world)) ||
 ((abs (worldEnergy world - worldEnergy (advanceWorld 0 0.001 world)) / worldEnergy world) < realToFrac (World.epsilon))
-- second line of test is checking nonempty list



