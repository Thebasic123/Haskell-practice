module Ex01 where

  -- needed to display the picture in the playground
import Codec.Picture

  -- our line graphics programming interface
import LineGraphics




-- Part 1

house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),(615, 325), (615, 250), (650, 250),(650, 363),
         (730, 450), (700, 450), (700, 750)]

door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]

smoke :: Path
smoke = [(635,240),(625,230),(635,220),(625,210)]

grey :: Colour
grey = (255, 255, 255, 128)

scene :: Picture
scene = [(lightgreen, house), (red, door),(grey,smoke)]


-- Part 2

scaleLine :: Float -> Line -> Line
scaleLine factor ((x1,y1),(x2,y2)) 
       = ((x3,y3),(x4,y4))
    where
       difference = (((x2-x1)*(1-factor)) / 2)
       x3 = difference + x1
       y3 = difference + y1
       x4 = x2 - difference
       y4 = y2 - difference

nestedSquares :: Float      -- scale factor
              -> Int        -- number of squares
              -> Colour     -- line colour
              -> Line       -- initial diagonal
              -> Picture
nestedSquares factor n colour line@((x1,y1),(x2,y2))
       | n <= 0 = []
       | otherwise = (colour,currentSquare) : nestedSquares factor newNumber colour newLine
       where 
         newNumber = n -1 
         newLine = scaleLine factor line 
         currentSquare = [(x1,y1),(x1,y2),(x2,y2),(x2,y1),(x1,y1)]


