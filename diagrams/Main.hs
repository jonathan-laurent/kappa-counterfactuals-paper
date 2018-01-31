{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude hiding (Linear)
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Names (darkgrey, grey, lightgrey)

import System.IO.Unsafe

import Graphics.SVGFonts

--------------------------------------------------------------------------------

--cmuTypewriterFont = unsafePerformIO $ loadFont "fonts/cmutt.svg"

cmuTypewriterFont = bit

renderText h t = stroke (textSVG' opts t) # fc black # lineWidth 0
  where opts = TextOpts cmuTypewriterFont INSIDE_H HADV False 1 h

agRadius = 1.0

textRule = renderText 1.0

textAgent = renderText 0.8

textId = renderText 0.7

--------------------------------------------------------------------------------

strokeWidth = 0.05

--------------------------------------------------------------------------------

data Shape = Circle | Square

renderShape :: Shape -> Diagram B
renderShape Circle = circle agRadius # lwG strokeWidth
renderShape Square = square (2 * agRadius) # lwG strokeWidth

site :: Bool -> Diagram B
site phos =
  circle 0.35 # fc color # lwG strokeWidth
  where
    color = if phos then grey else white

right = unitX
left = - unitX
top = unitY

agent :: Shape -> String -> [(Point V2 Double, Diagram B)] -> Diagram B
agent shape name sites =
  position sites <>
  (textAgent name) <>
  (renderShape shape # fc white)


bond n1 n2 =
  connectPerim' (with & arrowHead .~ noHead) n1 n2
  (0 @@ turn) (0.5 @@ turn)

duo a a' = a ||| strutX 1 ||| a'

rule_gen slow name m m' =
  ((textRule (name ++ ":") # alignX (-1)) <> (strutX 2.2 # alignX (-1)))
  ||| (left ||| strutX space ||| right)
  # connect
  # lwG strokeWidth
  where
   space = 3.5
   left  = localize m # named "left"
   right = localize m' # named "right"
   def_style = with & gaps .~ global 0.9 & headLength .~ global 0.4
   style =
     if slow then
       def_style & shaftStyle %~ dashingG [0.15, 0.15] 0
     else def_style
   connect =
     connectPerim' style
     "left" "right" (0 @@ turn) (0.5 @@ turn)

rule = rule_gen False

slow_rule = rule_gen True

agS hasD optX = agent Circle "S" [(right, d hasD), (top, x optX)]
  where
    d True = site False # named "sd"
    d False = mempty
    x Nothing = mempty
    x (Just phos) = site phos

agK hasD optX = agent Square "K" [(left, d hasD), (top, x optX)]
  where
    d True = site False # named "kd"
    d False = mempty
    x Nothing = mempty
    x (Just phos) = site phos

bondSK = bond "sd" "kd"

--------------------------------------------------------------------------------

model :: Diagram B
model =
  vsep 1.0 [bind, unbind, unbind_star, phos, pK]
  --vsep 1.0 [pK, bind, unbind, unbind_star, phos]
  where
    
    bind = slow_rule "b" m (m # bondSK)
      where m = duo (agS True Nothing) (agK True Nothing)

    unbind = rule "u" (m # bondSK) m
      where m = duo (agS True Nothing) (agK True (Just False))

    unbind_star = slow_rule "u*" (m # bondSK) m
      where m = duo (agS True Nothing) (agK True (Just True))

    phos = slow_rule "p" (m False) (m True)
      where m phos =  duo (agS True (Just phos)) (agK True Nothing) # bondSK

    pK = slow_rule "pk" (agK False (Just False)) (agK False (Just True))


cpats :: Diagram B
cpats = hsep 1.0 $ [
  agK False (Just False),
  agS True Nothing,
  agK True Nothing,
  duo (agS True Nothing) (agK True (Just False)) # bondSK,
  duo (agS True Nothing) (agK True (Just True))  # bondSK,
  duo (agS True (Just False)) (agK True Nothing) # bondSK
  ]

signature :: Diagram B
signature = hsep 2.0 $ [
  agS True (Just False),
  agK True (Just False)
  ]


labelBy d t =
  beside (r2 (1, -1)) d (withEnvelope env (textId t))
  where
    env :: D V2 Double
    env = square 0.8
  

data MixtureOptions = Linear | Compact | Standard deriving Eq

mixture :: MixtureOptions -> Diagram B
mixture opts =
  case opts of
    Linear -> linear
    Compact -> compact
    Standard -> standard
    
  where

    -- Elementary bricks    
    ag1 = agS True (Just False) `labelBy` "1"
    ag2 = agK True (Just True)  `labelBy` "2"
    ag3 = agS True (Just True)  `labelBy` "3"
    ag4 = agK True (Just False) `labelBy` "4"
    ag5 = agS True (Just False) `labelBy` "5"
    block1 = duo ag1 ag2 # bondSK

    -- Full version
    standard = rotateBy (1/16) block1 ||| strutX 0.6 ||| rotateBy (-1/16) block2
      where
        elts2 = [ rotateBy (1/8) ag3, ag4, ag5 ]
        block2 = atPoints (triangle 4.7 # rotateBy (-1/4)) elts2
        
    -- Compact version
    compact = rotateBy (1/rot) block1 ||| strutX 1.2 ||| rotateBy (0/rot) block2
      where
        rot = 25
        elts2 = [ rotateBy (-1/rot) ag3,
                  rotateBy (-1/rot) ag4,
                  rotateBy (1/rot) ag5 ]
        h = 2.0
        w = 8
        sep = 0.7
        block2 = atPoints [p2 (0,h), p2 (w/2,0), p2 (w-sep,h)] elts2

    -- linear version
    linear = hsep 1.3 [ block1, ag3, ag4, ag5]
     

--------------------------------------------------------------------------------

renderDiag f d =
  renderSVG f sizeSpec $ frame 1.0 d
  where sizeSpec = mkSizeSpec (Just 200.0 ^& Nothing)

main = do
  renderDiag "model.svg" model
  --renderDiag "cpats.svg" cpats
  renderDiag "signature.svg" signature
  renderDiag "mixture.svg" (mixture Standard)
  renderDiag "mixture-compact.svg" (mixture Compact)
  renderDiag "mixture-linear.svg" (mixture Linear)
