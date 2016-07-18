module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics


pointAdd :: (Float,Float) -> (Float,Float) -> (Float,Float) 
pointAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)

pointMultiply :: (Float,Float) -> Float -> (Float,Float) 
pointMultiply (x,y) times = (x*times,y*times)

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle 0 (Particle m p v) = Particle m p v
moveParticle time (Particle m p@(x,y) v@(xv,yv)) = Particle m newP v
   where newP = (x+xv*time,y+yv*time)
    
-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.
--
accelerate :: Float -> [Particle] -> [Particle]
accelerate time [] = [] 
accelerate time pList@(p:ps) = map (accelerateSingle time pList) pList



accelerateSingle :: Float->[Particle]->Particle ->Particle
accelerateSingle time pList particle@(Particle m p v) = foldl (updateVelo time) particle pList

updateVelo :: Float->Particle -> Particle -> Particle -- update the first Particle
updateVelo time p1@(Particle m p v) p2 = Particle m p newV
   where newV = pointAdd (pointMultiply (force p1 p2) time) v

-- Progressing the world stat     
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld _ time world@(World first second rate []) = world 
advanceWorld _ time world@(World first second rate pList) = World first second rate movedList
   where movedList = moveParticleList (time*rate) updatedList
         updatedList = accelerate (time*rate) pList

moveParticleList :: Float ->[Particle]->[Particle]
moveParticleList time [] = []
moveParticleList time pList@(p:ps) = moveParticle time p:(moveParticleList time ps)