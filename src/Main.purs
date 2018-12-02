module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Data.Array (fromFoldable) as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), filter, find, fromFoldable, groupBy, last, reverse, sort, (..), (:))
import Data.List.Types (toList)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
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
  , destinations :: List DestinationCell
  , toggledPerson :: Maybe PersonCell
  }

data SkipDir = Bck | Fwd
data SkipSize = Sm | Md | Lg

data Query a
  = Toggle (Maybe PersonCell) a
  | Move (Maybe PersonCell) Direction a

data Coord = Coord Int Int

data Person
  = Red
  | Blue
  | Yellow
  | NoPerson

data Cell = Cell Coord Person Wall Destination

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

data Tile
  = Moon
  | Square

data Destination
  = Destination Tile Person
  | NoDestination

data WallCell = WallCell Coord Wall

data PersonCell = PersonCell Coord Person

data DestinationCell = DestinationCell Coord Destination

derive instance genericPerson :: Generic Person _
derive instance genericCoord :: Generic Coord _
derive instance genericWall :: Generic Wall _
derive instance genericCell :: Generic Cell _
derive instance genericWallCell :: Generic WallCell _
derive instance genericPersonCell :: Generic PersonCell _
derive instance genericDirection :: Generic Direction _
derive instance genericTile :: Generic Tile _
derive instance genericDestination :: Generic Destination _
derive instance genericDestinationCell :: Generic DestinationCell _
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
instance showTile :: Show Tile where
  show = genericShow
instance showDestination :: Show Destination where
  show = genericShow
instance showDestinationCell :: Show DestinationCell where
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
derive instance eqTile :: Eq Tile
derive instance ordTile :: Ord Tile
derive instance eqDestination :: Eq Destination
derive instance ordDestination :: Ord Destination

initWalls :: List WallCell
initWalls =
  fromFoldable
  [ WallCell (Coord 3 0) Vertical
  , WallCell (Coord 1 2) Both
  , WallCell (Coord 0 1) Horizontal
  ]

findWall :: Coord -> List WallCell -> Maybe WallCell
findWall c = find (\(WallCell c' _) -> c' == c)

initPeople :: List PersonCell
initPeople =
  fromFoldable
    [ PersonCell (Coord 0 3) Red
    , PersonCell (Coord 2 3) Blue
    , PersonCell (Coord 0 0) Yellow
    ]

findPerson :: Coord -> List PersonCell -> Maybe PersonCell
findPerson c = find (\(PersonCell c' _) -> c' == c)

initDestinations :: List DestinationCell
initDestinations =
  fromFoldable
    [ DestinationCell (Coord 1 2) (Destination Moon Blue)
    , DestinationCell (Coord 3 0) (Destination Moon Red)
    ]

findDestination :: Coord -> List DestinationCell -> Maybe DestinationCell
findDestination c = find (\(DestinationCell c' _) -> c' == c)

grid :: List Cell
grid = do
  x <- 0 .. 3
  y <- 3 .. 0
  pure $ Cell (Coord x y) NoPerson NoWall NoDestination

groupGrid :: List Cell -> List (List Cell)
groupGrid =
  map toList <<<
  groupBy (\(Cell (Coord x1 _) _ _ _) (Cell (Coord x2 _) _ _ _) -> x1 == x2)

addWalls :: List WallCell -> List Cell -> List Cell
addWalls walls cells = do
  cell@Cell coord person _ dest <- cells
  -- "lift" WallCell into Cell?
  -- could be middleware?
  -- use Control.State?
  let newCell =
        maybe cell identity (map (\(WallCell coord' wall') -> Cell coord' person wall' dest) $ findWall coord walls)
  pure newCell

addPeople :: List PersonCell -> List Cell -> List Cell
addPeople people cells = do
  cell@Cell coord _ wall dest <- cells
  -- TODO: Error if placing new person on Coord of existing person
  let newCell =
        maybe cell identity (map (\(PersonCell coord' person') -> Cell coord' person' wall dest) $ findPerson coord people)
  pure newCell

addDestinations :: List DestinationCell -> List Cell -> List Cell
addDestinations destinations cells = do
  cell@Cell coord person wall _ <- cells
  let newCell =
        maybe cell identity (map (\(DestinationCell coord' dest') -> Cell coord' person wall dest') $ findDestination coord destinations)
  pure newCell

type Ops =
  { compX :: Int -> Int -> Boolean
  , compY :: Int -> Int -> Boolean
  , ord :: List Cell -> List Cell
  }

filt :: Direction -> PersonCell -> List Cell -> List Cell
filt dir (PersonCell (Coord x y) p) =
  collectValidTiles dir <<<
  filter (\(Cell c'@(Coord x' y') _ _ _) -> x' `ops'.compX` x && y  `ops'.compY` y') <<<
  (ops dir).ord <<<
  sort
  where
    -- TODO: refactor
    collectValidTiles :: Direction -> List Cell -> List Cell
    -- West
    collectValidTiles West  (cell@(Cell _ NoPerson Vertical   _) : _   )           = cell : Nil
    collectValidTiles West  (cell@(Cell _ NoPerson Both       _) : _   )           = cell : Nil
    collectValidTiles West  (cell@(Cell _ NoPerson Horizontal _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles West  (cell@(Cell _ NoPerson NoWall     _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles West  (cell@(Cell _ p'       Vertical   _) : _   ) | p == p' = cell : Nil
    collectValidTiles West  (cell@(Cell _ p'       Both       _) : _   ) | p == p' = cell : Nil
    collectValidTiles West  (cell@(Cell _ p'       Horizontal _) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles West  (cell@(Cell _ p'       NoWall     _) : rest) | p == p' = cell : collectValidTiles dir rest
    -- East
    collectValidTiles East  (cell@(Cell _ NoPerson Vertical   _) : _   )           = Nil
    collectValidTiles East  (cell@(Cell _ NoPerson Both       _) : _   )           = Nil
    collectValidTiles East  (cell@(Cell _ NoPerson Horizontal _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles East  (cell@(Cell _ NoPerson NoWall     _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles East  (cell@(Cell _ p'       Vertical   _) : _   ) | p == p' = Nil
    collectValidTiles East  (cell@(Cell _ p'       Both       _) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles East  (cell@(Cell _ p'       Horizontal _) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles East  (cell@(Cell _ p'       NoWall     _) : rest) | p == p' = cell : collectValidTiles dir rest
    -- South
    collectValidTiles South (cell@(Cell _ NoPerson Vertical   _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles South (cell@(Cell _ NoPerson Both       _) : _   )           = cell : Nil
    collectValidTiles South (cell@(Cell _ NoPerson Horizontal _) : _   )           = cell : Nil
    collectValidTiles South (cell@(Cell _ NoPerson NoWall     _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles South (cell@(Cell _ p'       Vertical   _) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles South (cell@(Cell _ p'       Both       _) : _   ) | p == p' = cell : Nil
    collectValidTiles South (cell@(Cell _ p'       Horizontal _) : _   ) | p == p' = cell : Nil
    collectValidTiles South (cell@(Cell _ p'       NoWall     _) : rest) | p == p' = cell : collectValidTiles dir rest
    -- North
    collectValidTiles North (cell@(Cell _ NoPerson Vertical   _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ NoPerson Both       _) : _   )           = Nil
    collectValidTiles North (cell@(Cell _ NoPerson Horizontal _) : _   )           = Nil
    collectValidTiles North (cell@(Cell _ NoPerson NoWall     _) : rest)           = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       Vertical   _) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       Both       _) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       Horizontal _) : rest) | p == p' = cell : collectValidTiles dir rest
    collectValidTiles North (cell@(Cell _ p'       NoWall     _) : rest) | p == p' = cell : collectValidTiles dir rest
    -- Base
    collectValidTiles _ _ = Nil

    ops' :: Ops
    ops' = ops dir

    ops :: Direction -> Ops
    ops West = { compX: (<=)
               , compY: (==)
               , ord: reverse
               }
    ops East = { compX: (>=)
               , compY: (==)
               , ord: identity
               }
    ops North = { compX: (==)
                , compY: (<=)
                , ord: identity
                }
    ops South = { compX: (==)
                , compY: (>=)
                , ord: reverse
                }

-- check for: walls, people, goal
movePerson :: PersonCell -> Direction -> List Cell -> PersonCell
movePerson pc@(PersonCell c@(Coord x y) p) direction
  = maybe pc identity <<<
    -- replace last with head
    last <<<
    map (\(Cell coord' _ _ _) -> PersonCell coord' p) <<<
    filt direction (PersonCell c p) <<<
    fromFoldable

gridToHtml :: Game -> List Cell -> H.ComponentHTML Query
gridToHtml game =
  arrayArrayToHtml game <<<
  groupGrid <<<
  addPeople game.people <<<
  addWalls game.walls <<<
  addDestinations game.destinations

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
cellToHtml {toggledPerson} (Cell coord@(Coord x y) person wall dest) =
  HH.div
    [ HP.class_ $ wrap $ ("box " <> toggledClass toggledPerson)
    , HE.onClick $ HE.input_ $ clickHandler toggledPerson coord person
    ] $
--    [ HH.text $ "(" <> show x <> "," <> show y <> ")"] <>
    walls <>
    people <>
    destinations
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

    people :: Array (H.ComponentHTML Query)
    people = case person of
      NoPerson -> []
      _ -> [ HH.div [ HP.class_ $ wrap $ "person icon " <> genPersonColour person] []]

    genWall :: String -> H.ComponentHTML Query
    genWall className = HH.div [ HP.class_ $ wrap $ "wall " <> className] []

    walls :: Array (H.ComponentHTML Query)
    walls = case wall of
      Vertical -> [ genWall "vertical "]
      Horizontal -> [ genWall "horizontal "]
      Both -> [ genWall "vertical "
              , genWall "horizontal "
              ]
      NoWall -> []

    destinations :: Array (H.ComponentHTML Query)
    destinations = case dest of
      NoDestination -> []
      (Destination t' p') -> [ HH.div [ HP.class_ $ wrap $ "icon tile " <> genTile t' <> genPersonColour p'] []]

    genTile :: Tile -> String
    genTile t = case t of
      Moon -> "moon "
      Square -> "square "

    genPersonColour :: Person -> String
    genPersonColour p = case p of
      Blue -> "blue "
      Red -> "red "
      Yellow -> "yellow "
      NoPerson -> ""

ui :: H.Component HH.HTML Query Unit Void Aff
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
          { walls: initWalls
          , people: initPeople
          , destinations: initDestinations
          , toggledPerson: Nothing
          }
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ wrap "container" ]
        [ gridToHtml state.game grid ]
--    log' = H.liftEff <<< log
    eval :: forall m. Query ~> H.ComponentDSL State Query Void m --Aff
    eval = case _ of
      move@(Move (Just npc@(PersonCell coord _)) direction next) -> do
        {walls, people} <- H.gets _.game
        let obstacledGrid = (addWalls walls <<< addPeople people) grid
        let npc'@(PersonCell coord' np) = movePerson npc direction obstacledGrid
        let newPeople = map (\opc@(PersonCell _ op) -> if np == op then npc' else opc) people
--        log' "people:"
--        log' $ show people
--        log' $ show newPeople
--        log' "person:"
--        log' $ show npc
--        log' "filtered oldGrid:"
--        log' $ show $ filt direction npc $ fromFoldable obstacledGrid
        _ <- H.modify \s -> s {game {people = newPeople}}
--        let nextGrid = addWalls walls <<< addPeople newPeople $ grid
--        log' "filtered nextGrid:"
--        log' $ show $ filt direction npc' $ fromFoldable nextGrid
--        log' $ "nextGrid:"
--        log' $ show nextGrid
        pure next
      (Move Nothing _ next) -> pure next
      (Toggle toggledPerson next) -> do
--      log' $ "toggledPerson: " <> show toggledPerson
--      {walls, people, grid} <- H.gets _.game
--      let obstacledGrid = (addWalls walls <<< addPeople people) grid
--      log' $ show obstacledGrid
        _ <- H.modify \s -> s {game {toggledPerson = toggledPerson}}
        pure next

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  D.runUI ui unit body

