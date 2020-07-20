module Types where

import SExpr

data Vec = Vec !Integer !Integer deriving (Eq, Ord, Show)

parseVec :: SExpr -> Vec
parseVec (Cons (Int x) (Int y)) = Vec x y
parseVec x = error ("not a vector: " ++ show x)

instance ToSExpr Vec where
  toSExpr (Vec x y) = Cons (Int x) (Int y)

data GameResponse = GameResponse {
                        success :: !Bool
                      , stage :: GameStage
                      , staticInfo :: StaticGameInfo
                      , state :: GameState
                      }

instance Show GameResponse where
  showsPrec _ r | success r = showString "stage: " . shows (stage r)
                            . showString ", " . shows (state r)
                | otherwise = showString "GameResponse-Error"


data GameStage = Setup | Playing | Ended deriving (Eq, Ord, Enum, Show)

data GameState = GameState {
                     gameTick :: !Integer
                   , stX1 :: SExpr
                   , shipsAndCommands :: [(Ship, [Command])]
                 } deriving (Show)

data Role = Attacker | Defender deriving (Eq, Ord, Enum, Show)

instance ToSExpr Role where
  toSExpr Attacker = Int 0
  toSExpr Defender = Int 1

data StaticGameInfo = StaticGameInfo { g0 :: SExpr
                                     , myRole :: !Role
                                     , g2 :: SExpr
                                     , g3 :: SExpr
                                     , g4 :: SExpr
                                     } deriving (Show)


data Ship = Ship { shipRole :: !Role
                 , shipId :: !Integer
                 , position :: !Vec
                 , velocity :: !Vec
                 , x4 :: SExpr
                 , x5 :: SExpr
                 , x6 :: SExpr
                 , x7 :: SExpr
                 } deriving (Show)

parseShip :: SExpr -> Ship
parseShip e = Ship { shipRole = decodeEnum (car e)
                   , shipId = intValue (nth 1 e)
                   , position = parseVec (nth 2 e)
                   , velocity = parseVec (nth 3 e)
                   , x4 = nth 4 e
                   , x5 = nth 5 e
                   , x6 = nth 6 e
                   , x7 = nth 7 e
                   }

-- instance ToSExpr Ship where
--   toSExpr s = list [toSExpr (shipRole s), toSExpr (shipId s), toSExpr (position s), toSExpr (velocity s)]

data Command = Accelerate !Integer !Vec
             | Detonate !Integer
             | Shoot !Integer !Vec SExpr
             deriving (Show)
             
parseCommand :: SExpr -> Command
parseCommand cmd@(Cons (Int n) (Cons (Int id) rest)) =
    case n of
      0 -> Accelerate id (parseVec (car rest))
      1 -> Detonate id
      2 -> Shoot id (parseVec (car rest)) (nth 1 rest)

instance ToSExpr Command where
  toSExpr (Accelerate ship vec) = list [Int 0, toSExpr ship, toSExpr vec]
  toSExpr (Detonate ship)       = list [Int 1, toSExpr ship]
  toSExpr (Shoot ship vec sX3)  = list [Int 2, toSExpr ship, toSExpr vec, sX3]

parseResponse :: SExpr -> GameResponse
parseResponse (Cons (Int 1) e) = parseSuccess e
parseResponse _ = GameResponse False undefined undefined undefined

parseSuccess :: SExpr -> GameResponse
parseSuccess e = GameResponse {
                    success = True
                  , stage = stage
                  , staticInfo = staticInfo
                  , state = state }
  where
    stage = decodeEnum (car e)
    staticInfo = parseStaticInfo (nth 1 e)
    state = parseState (nth 2 e)

parseStaticInfo :: SExpr -> StaticGameInfo
parseStaticInfo e = StaticGameInfo{ myRole = decodeEnum (nth 1 e),
                                    g0 = nth 0 e,
                                    g2 = nth 2 e,
                                    g3 = nth 3 e,
                                    g4 = nth 4 e }

parseState :: SExpr -> GameState
parseState e = GameState { gameTick = intValue (car e)
                         , stX1 = nth 1 e
                         , shipsAndCommands = parseList sandc (nth 2 e) }
  where
    sandc (Cons ship (Cons cmds _)) = (parseShip ship, parseList parseCommand cmds)

parseList :: (SExpr -> a) -> SExpr -> [a]
parseList _      Nil = []
parseList parser (Cons x xs) = parser x : parseList parser xs

decodeEnum :: Enum a => SExpr -> a
decodeEnum e = toEnum (fromInteger (intValue e))

intValue :: SExpr -> Integer
intValue (Int n) = n
intValue x = error ("expected number, got " ++ show x)
