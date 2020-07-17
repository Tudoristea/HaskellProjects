module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (ang, axi, rls)
  = ang

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (ang, axi, rls)
  = axi

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (ang, axi, [])
  = [(' ', "No Rules")]
rules (ang, axi, rlzz@(rl:rls))
  = rlzz

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
 -- Pre: the character has a binding in the Rules list
lookupChar chr ((rl,rls):rest) --((dl:dls):
  | chr == rl = rls
  | otherwise = lookupChar chr rest

    
  -- Fancier way to define the function with list comprehension
  --lookupChar chr rls
  --  = concat [snd | (fst,snd) <- rls, fst == chr]



  -- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne [] givenRls
  = []
expandOne (chr:givenStr) givenRls
  | lUpChrInGivenRls /= [] = lUpChrInGivenRls ++ expandOne givenStr givenRls
  | otherwise              = chr : expandOne givenStr givenRls 
      where
        lUpChrInGivenRls = lookupChar chr givenRls
 
-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
-- expand givenStr 0 givenRls
--   = 
expand (chr:givenStr) cont givenRls
  = iterate (flip expandOne givenRls) (chr:givenStr) !! cont

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move cmnd angl ((tX,tY),tA)
  | cmnd == 'L' = ((tX,tY),(tA + angl)) 
  | cmnd == 'R' = ((tX,tY),(tA - angl))
  | cmnd == 'F' = ((cos z + tX, sin z + tY), tA) -- convert in radiani tA 
    where 
      z = tA * pi / 180
  
  
 -- transform cos din radian in ceva
  -- | otherwise   = error

--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--
trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 
  = undefined

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands' angl c
  = trace2' [] commands' ((0.0,0.0), 90)
    where 
      initial = (0.0,0.0)
      trace2' :: Stack -> Commands -> TurtleState -> [ColouredLine]
      trace2' _ [] _
        = []
      trace2' stk (cmnd:cmnds) trtstate@(pos,a)
        | cmnd == '[' = trace2' (trtstate:stk) cmnds trtstate
        | cmnd == 'R' = trace2' stk cmnds (move 'R' angl trtstate)
        | cmnd == 'F' = new : (trace2' stk cmnds (move 'F' angl trtstate))
        | cmnd == 'L' = trace2' stk cmnds (move 'L' angl trtstate)
        | cmnd == ']' = trace2' last cmnds first 
         where
          first:last = stk
          new = (pos, pos', c) 
          (pos',neb) = move 'F' angl trtstate

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
