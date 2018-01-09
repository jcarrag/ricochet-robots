module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Array (fromFoldable) as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), filter, fromFoldable, groupBy, last, reverse, sort, (..), (:))
import Data.List.Types (toList)
import Data.Map (Map, fromFoldable, lookup) as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as D

type State =
  { game :: Game
  }

type Game =
  { walls :: List WallCell
  , people :: List PersonCell
  , grid :: List Cell
  , toggledPerson :: Maybe PersonCell
  }

data SkipDir = Bck | Fwd
data SkipSize = Sm | Md | Lg

data Query a
  = Toggle (Maybe PersonCell) a
  | Move (Maybe PersonCell) Direction a

type AppEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  | eff)

data Coord = Coord Int Int

data Person
  = Red
  | Blue
  | Yellow
  | NoPerson

data Cell = Cell Coord Person Wall

data Direction
  = North
  | East
  | South
  | West

data Wall
  = Vertical
  | Horizontal
  | Both
  | NoWall

data WallCell = WallCell Coord Wall

data PersonCell = PersonCell Coord Person

derive instance genericPerson :: Generic Person _
derive instance genericCoord :: Generic Coord _
derive instance genericWall :: Generic Wall _
derive instance genericCell :: Generic Cell _
derive instance genericWallCell :: Generic WallCell _
derive instance genericPersonCell :: Generic PersonCell _
derive instance genericDirection :: Generic Direction _
instance showPerson :: Show Person where
  show = genericShow
instance showCoord :: Show Coord where
  show = genericShow
instance showWall :: Show Wall where
  show = genericShow
instance showCell :: Show Cell where
  show = genericShow
instance showWallCell :: Show WallCell where
  show = genericShow
instance showPersonCell :: Show PersonCell where
  show = genericShow
instance showDirection :: Show Direction where
  show = genericShow
derive instance eqCoord :: Eq Coord
derive instance ordCoord :: Ord Coord
derive instance eqPerson :: Eq Person
derive instance eqPersonCell :: Eq PersonCell
derive instance ordPerson :: Ord Person
derive instance eqWall:: Eq Wall
derive instance ordWall:: Ord Wall
derive instance eqCell :: Eq Cell
derive instance ordCell :: Ord Cell

walls :: List WallCell
walls =
  fromFoldable
  [ WallCell (Coord 3 0) Vertical
  , WallCell (Coord 1 2) Both
  , WallCell (Coord 0 1) Horizontal
  ]

lookupWalls :: List WallCell -> M.Map Coord WallCell
lookupWalls =
  M.fromFoldable <<<
  map (\wc@(WallCell coord _) -> Tuple coord wc)

people :: List PersonCell
people =
  fromFoldable
    [ PersonCell (Coord 0 3) Red
    , PersonCell (Coord 2 3) Blue
    , PersonCell (Coord 0 0) Yellow
    ]

lookupPeople :: List PersonCell -> M.Map Coord PersonCell
lookupPeople =
  M.fromFoldable <<<
  map (\pc@(PersonCell coord _) -> Tuple coord pc)

grid :: List Cell
grid = do
  x <- 0 .. 3
  y <- 3 .. 0
  pure $ Cell (Coord x y) NoPerson NoWall

groupGrid :: List Cell -> List (List Cell)
groupGrid =
  map toList <<<
  groupBy (\(Cell (Coord x1 _) _ _) (Cell (Coord x2 _) _ _) -> x1 == x2)

addWalls :: List WallCell -> List Cell -> List Cell
addWalls walls cells = do
  cell@Cell coord person _ <- cells
  -- "lift" WallCell into Cell?
  -- could be middleware?
  -- use Control.State?
  let newCell =
        maybe cell id (map (\(WallCell newCoord wall) -> Cell newCoord person wall) $ M.lookup coord $ lookupWalls walls)
  pure newCell

addPeople :: List PersonCell -> List Cell -> List Cell
addPeople people cells = do
  cell@Cell coord _ wall <- cells
  -- TODO: Error if placing new person on Coord of existing person
  let newCell =
        maybe cell id (map (\(PersonCell newCoord person) -> Cell newCoord person wall) $ M.lookup coord $ lookupPeople people)
  pure newCell

type Ops =
  { compX :: Int -> Int -> Boolean
  , compY :: Int -> Int -> Boolean
  , ord :: List Cell -> List Cell
  }

filt :: Direction -> PersonCell -> List Cell -> List Cell
filt dir (PersonCell (Coord x y) p) =
  collectValidTiles dir <<<
  filter (\(Cell c'@(Coord x' y') p' w') -> x' `(ops dir).compX` x) <<<
  filter (\(Cell c'@(Coord x' y') p' w') -> y `(ops dir).compY` y') <<<
  (ops dir).ord <<<
  sort
  where
    -- TODO: refactor
    collectValidTiles :: Direction -> List Cell -> List Cell
    -- West
    collectValidTiles West (cell@(Cell _ NoPerson Vertical  ) : _   )           = cell : Nil
    collectValidTiles West (cell@(Cell _ NoPerson Both      ) : _   )           = cell : Nil
    collectValidTiles West (cell@(Cell _ NoPerson Horizontal) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles West (cell@(Cell _ NoPerson NoWall    ) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles West (cell@(Cell _ p'       Vertical  ) : _   ) | p == p' = cell : Nil
    collectValidTiles West (cell@(Cell _ p'       Both      ) : _   ) | p == p' = cell : Nil
    collectValidTiles West (cell@(Cell _ p'       Horizontal) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles West (cell@(Cell _ p'       NoWall    ) : rest) | p == p' = cell : collectValidTiles dir rest
    -- East
    collectValidTiles East (cell@(Cell _ NoPerson Vertical  ) : _   )           = Nil
    collectValidTiles East (cell@(Cell _ NoPerson Both      ) : _   )           = Nil
    collectValidTiles East (cell@(Cell _ NoPerson Horizontal) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles East (cell@(Cell _ NoPerson NoWall    ) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles East (cell@(Cell _ p'       Vertical  ) : _   ) | p == p' = Nil
    collectValidTiles East (cell@(Cell _ p'       Both      ) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles East (cell@(Cell _ p'       Horizontal) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles East (cell@(Cell _ p'       NoWall    ) : rest) | p == p' = cell : collectValidTiles dir rest
    -- South
    collectValidTiles South (cell@(Cell _ NoPerson Vertical  ) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles South (cell@(Cell _ NoPerson Both      ) : _   )           = cell : Nil
    collectValidTiles South (cell@(Cell _ NoPerson Horizontal) : _   )           = cell : Nil
    collectValidTiles South (cell@(Cell _ NoPerson NoWall    ) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles South (cell@(Cell _ p'       Vertical  ) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles South (cell@(Cell _ p'       Both      ) : _   ) | p == p' = cell : Nil
    collectValidTiles South (cell@(Cell _ p'       Horizontal) : _   ) | p == p' = cell : Nil
    collectValidTiles South (cell@(Cell _ p'       NoWall    ) : rest) | p == p' = cell : collectValidTiles dir rest
    -- North
    collectValidTiles North (cell@(Cell _ NoPerson Vertical  ) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ NoPerson Both      ) : _   )           = Nil
    collectValidTiles North (cell@(Cell _ NoPerson Horizontal) : _   )           = Nil
    collectValidTiles North (cell@(Cell _ NoPerson NoWall    ) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       Vertical  ) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       Both      ) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       Horizontal) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       NoWall    ) : rest) | p == p' = cell : collectValidTiles dir rest
    -- Base
    collectValidTiles _ _ = Nil

    ops :: Direction -> Ops
    ops West = { compX: (<=)
               , compY: (==)
               , ord: reverse
               }
    ops East = { compX: (>=)
               , compY: (==)
               , ord: id
               }
    ops North = { compX: (==)
                , compY: (<=)
                , ord: id
                }
    ops South = { compX: (==)
                , compY: (>=)
                , ord: reverse
                }

-- check for: walls, people, goal
movePerson :: PersonCell -> Direction -> List Cell -> PersonCell
movePerson pc@(PersonCell c@(Coord x y) p) direction
  = maybe pc id <<<
    -- replace last with head
    last <<<
    map (\(Cell coord' _ _) -> PersonCell coord' p) <<<
    filt direction (PersonCell c p) <<<
    fromFoldable

gridToHtml :: Game -> List Cell -> H.ComponentHTML Query
gridToHtml game =
  arrayArrayToHtml game <<<
  groupGrid <<<
  addPeople game.people <<<
  addWalls game.walls

-- TODO: Reader monad to implicitly add state to these render functions?
arrayArrayToHtml :: Game -> List (List Cell) -> H.ComponentHTML Query
arrayArrayToHtml state =
  HH.div [ HP.class_ $ wrap "outer"] <<<
  map (arrayToHtml state) <<<
  A.fromFoldable

arrayToHtml :: Game -> List Cell -> H.ComponentHTML Query
arrayToHtml state =
  HH.div [ HP.class_ $ wrap "inner"] <<<
  map (cellToHtml state) <<<
  A.fromFoldable

-- TODO: making variable naming consistent
cellToHtml :: Game -> Cell -> H.ComponentHTML Query
cellToHtml {toggledPerson} (Cell coord@(Coord x y) person wall) =
  HH.div
    [ HP.class_ $ wrap $ ("box " <> toggledClass toggledPerson)
    , HE.onClick $ HE.input_ $ clickHandler toggledPerson coord person
    ] $
    [ HH.text $ "(" <> show x <> "," <> show y <> ")"] <>
    walls <>
    people
  where
    -- TODO: use message vs. query?
    clickHandler :: forall a. Maybe PersonCell -> Coord -> Person -> (a -> Query a)
    clickHandler tp@(Just (PersonCell (Coord x' y') _)) _ NoPerson | x'>x && y'==y = Move tp West
    clickHandler tp@(Just (PersonCell (Coord x' y') _)) _ NoPerson | x'<x && y'==y = Move tp East
    clickHandler tp@(Just (PersonCell (Coord x' y') _)) _ NoPerson | y'>y && x'==x = Move tp South
    clickHandler tp@(Just (PersonCell (Coord x' y') _)) _ NoPerson | y'<y && x'==x = Move tp North
    clickHandler _ _ NoPerson = Toggle Nothing
    clickHandler _ _ _ = Toggle (Just (PersonCell coord person))

    toggledClass :: Maybe PersonCell -> String
    toggledClass (Just (PersonCell c' p')) | c'==coord && p'==person = "toggled "
    toggledClass _ = ""

    genPerson :: String -> H.ComponentHTML Query
    genPerson colour = HH.div [ HP.class_ $ wrap $ "person icon " <> colour] []

    people :: Array (H.ComponentHTML Query)
    people = case person of
      Blue -> [ genPerson "blue"]
      Red -> [ genPerson "red"]
      Yellow -> [ genPerson "yellow"]
      NoPerson -> []

    genWall :: String -> H.ComponentHTML Query
    genWall className = HH.div [ HP.class_ $ wrap $ "wall " <> className] []

    walls :: Array (H.ComponentHTML Query)
    walls = case wall of
      Vertical -> [ genWall "vertical"]
      Horizontal -> [ genWall "horizontal"]
      Both -> [ genWall "vertical"
              , genWall "horizontal"
              ]
      NoWall -> []

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where
    initialState =
      { game:
          { people
          , walls
          , grid
          , toggledPerson: Nothing
          }
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ wrap "container" ]
        [ HH.div
          [ HP.class_ $ wrap "root"]
          [ gridToHtml state.game grid ]
        ]
--    log' = H.liftEff <<< log
    eval :: Query ~> H.ComponentDSL State Query Void (Aff (AppEffects eff))
    eval move@(Move (Just npc@(PersonCell coord _)) direction next) = do
      {walls, people, grid} <- H.gets _.game
      let obstacledGrid = (addWalls walls <<< addPeople people) grid
      let npc'@(PersonCell coord' np) = movePerson npc direction obstacledGrid
      let newPeople = map (\opc@(PersonCell _ op) -> if np == op then npc' else opc) people
--      log' "people:"
--      log' $ show people
--      log' $ show newPeople
--      log' "person:"
--      log' $ show npc
--      log' "filtered oldGrid:"
--      log' $ show $ filt direction npc $ fromFoldable obstacledGrid
      H.modify \s -> s {game {people = newPeople}}
--      let nextGrid = addWalls walls <<< addPeople newPeople $ grid
--      log' "filtered nextGrid:"
--      log' $ show $ filt direction npc' $ fromFoldable nextGrid
--      log' $ "nextGrid:"
--      log' $ show nextGrid
      pure next
    eval (Move Nothing _ next) = pure next
    eval (Toggle toggledPerson next) = do
--      log' $ "toggledPerson: " <> show toggledPerson
--      {walls, people, grid} <- H.gets _.game
--      let obstacledGrid = (addWalls walls <<< addPeople people) grid
--      log' $ show obstacledGrid
      H.modify \s -> s {game {toggledPerson = toggledPerson}}
      pure next

main :: forall e.
  Eff
    (AppEffects
      ( avar :: AVAR
      , ref :: REF
      , exception :: EXCEPTION
      | e
      )
    )
    Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  H.liftEff $ log "Running"
