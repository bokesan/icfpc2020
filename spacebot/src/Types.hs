module Types where

import SExpr

data Vec = Vec !Integer !Integer deriving (Eq, Ord, Show)

parseVec :: SExpr -> Vec
parseVec (Cons (Int x) (Cons (Int y) _)) = Vec x y
parseVec x = error ("not a vector: " ++ show x)

instance ToSExpr Vec where
  toSExpr (Vec x y) = list [Int x, Int y]

data GameResponse = GameResponse {
                        success :: !Bool
                      , stage :: !GameStage
                      , staticInfo :: !StaticGameInfo
                      , state :: !GameState
                      }

instance Show GameResponse where
  showsPrec _ r | success r    = showString "(1, "
                               . shows (stage r)
                               . showString ", "
                               . shows (staticInfo r)
                               . showString ", "
                               . shows (state r)
                               . showChar ')'
                | otherwise = showString "(0)"


data GameStage = Setup | Playing | Ended deriving (Eq, Ord, Enum)

instance Show GameStage where
  showsPrec _ s = shows (fromEnum s)

data GameState = GameState {
                     gameTick :: !Integer
                   , shipsAndCommands :: [(Ship, [Command])]
                 }

instance Show GameState where
  showsPrec _ s = showChar '(' . shows (gameTick s)
                . showString ", x1, "
                . shows (shipsAndCommands s) . showChar ')'

data Role = Attacker | Defender deriving (Eq, Ord, Enum)

instance Show Role where
  showsPrec _ s = shows (fromEnum s)

instance ToSExpr Role where
  toSExpr Attacker = Int 0
  toSExpr Defender = Int 1

data StaticGameInfo = StaticGameInfo { myRole :: !Role }

instance Show StaticGameInfo where
   showsPrec _ g = showString "(x0, " . shows (myRole g)
                 . showString ", x2, x3)"


data Ship = Ship { shipRole :: !Role
                 , shipId :: !Integer
                 , position :: !Vec
                 , velocity :: !Vec
                 } deriving (Show)

parseShip :: SExpr -> Ship
parseShip e = Ship { shipRole = decodeEnum (car e)
                   , shipId = intValue (nth 1 e)
                   , position = parseVec (nth 2 e)
                   , velocity = parseVec (nth 3 e)
                   }

-- instance ToSExpr Ship where
--   toSExpr s = list [toSExpr (shipRole s), toSExpr (shipId s), toSExpr (position s), toSExpr (velocity s)]

data Command = Accelerate !Integer !Vec
             | Detonate !Integer
             | Shoot !Integer !Vec

parseCommand :: SExpr -> Command
parseCommand cmd@(Cons (Int n) (Cons (Int id) rest)) =
    case n of
      0 -> Accelerate id (parseVec (car rest))
      1 -> Detonate id
      2 -> Shoot id (parseVec (car rest))

instance ToSExpr Command where
  toSExpr (Accelerate ship vec) = list [Int 0, toSExpr ship, toSExpr vec]
  toSExpr (Detonate ship)       = list [Int 1, toSExpr ship]
  toSExpr (Shoot ship vec)      = list [Int 2, toSExpr ship, toSExpr vec]

instance Show Command where
    showsPrec _ (Accelerate ship vec) = showString "(0, " . shows ship . showString ", "
                                      . shows vec . showChar ')'
    showsPrec _ (Detonate ship) = showString "(1, " . shows ship . showChar ')'
    showsPrec _ (Shoot ship vec) = showString "(2, " . shows ship . showString ", "
                                 . shows vec . showChar ')'

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
parseStaticInfo e = StaticGameInfo $ decodeEnum (nth 1 e)

parseState :: SExpr -> GameState
parseState e = GameState { gameTick = intValue (car e)
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
