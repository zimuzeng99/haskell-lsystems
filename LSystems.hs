module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (x, _, _) = x

-- |Returns the base string for the given system.
base :: System -> String
base (_, y, _) = y

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, z) = z

-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar searchChar rules
 | null results = "norule"
 | otherwise = head results
   where results = [y | (x,y) <- rules, x == searchChar]

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne _ "" = ""
expandOne rules (ch : chs)
 | result == "norule" = ch : expandOne rules chs
 | otherwise = result ++ expandOne rules chs
   where result = lookupChar ch rules

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules baseStr@(ch : chs) n
 | n == 0 = baseStr
 | otherwise = expand rules (expandOne rules baseStr) (n - 1)

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.

toRad :: Float -> Float
toRad deg = deg / 180 * pi

move :: Char -> TurtleState -> Float -> TurtleState
move cmd state rotate
 | cmd == 'F' = ((x + cos (toRad theta), y + sin (toRad theta)), theta)
 | cmd == 'L' = (pos, theta + rotate)
 | cmd == 'R' = (pos, theta - rotate)
   where
    (pos@(x, y), theta) = state

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 commands rotate colour
 = fst (trace1' commands rotate colour ((0, 0), 90) [])

trace1' :: String -> Float -> Colour -> TurtleState -> [ColouredLine] -> ([ColouredLine], String)
trace1' commands rotate colour state traces
 | null commands = (traces, "")
 | cmd == 'F' = trace1' cmds rotate colour newState (line : traces)
 | cmd == '[' = trace1' remaining rotate colour state (branch ++ traces)
 | cmd == ']' = (traces, cmds)
 | otherwise = trace1' cmds rotate colour newState traces
   where
    newState = move cmd state rotate
    line = (fst state, fst newState, colour)
    (cmd: cmds) = commands
    (branch, remaining) = trace1' cmds rotate colour state []

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 commands rotate colour
 = trace2' commands rotate colour ((0,0), 90) [] []

trace2' :: String -> Float -> Colour -> TurtleState -> [TurtleState] -> [ColouredLine] -> [ColouredLine]
trace2' commands rotate colour state turtleStack traces
 | null commands = traces
 | cmd == 'F' = trace2' cmds rotate colour newState turtleStack (line : traces)
 | cmd == '[' = trace2' cmds rotate colour state (state:turtleStack) traces
 | cmd == ']' = trace2' cmds rotate colour (head turtleStack) (tail turtleStack) traces
 | otherwise = trace2' cmds rotate colour newState turtleStack traces
   where
    newState = move cmd state rotate
    line = (fst state, fst newState, colour)
    (cmd: cmds) = commands

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
