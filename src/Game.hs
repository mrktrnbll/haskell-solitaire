module Game where
import Deck
import Error
import Data.List (transpose)

{- Commands and instructions, representing moves to be made -}
type StackIndex = Int
type Count = Int
type FromStack = StackIndex
type ToStack = Int

-- An instruction is a top-level command.
data Instruction = Quit | Undo | GameCommand Command

-- A Command is a move to be played in the game.
data Command = Move Count FromStack ToStack
             | MoveStack FromStack ToStack
             | Draw
             | MoveFromDiscard StackIndex
             | MoveToPillar CardSource
             | MoveFromPillar Suit StackIndex
             | Solve

data CardSource = FromStack StackIndex | FromDiscard

{- Board representation -}

-- A column is a list of (Card, Bool) pairs, where the Bool
-- represents visibility: true for visible, false for hidden
type Column = [(Card, Bool)]

-- The pillars / foundation stacks are represented as Maybe Card
-- values, where Nothing represents an empty pillar and Just c 
-- denotes that card 'c' is at the top of the pillar.
-- Note that we don't need to store all cards since they are stored
-- in ascending order.
data Pillars = MkPillars {
        spades :: Maybe Value,
        clubs :: Maybe Value,
        hearts :: Maybe Value,
        diamonds :: Maybe Value
  }
  deriving (Show, Eq)

emptyPillars :: Pillars
emptyPillars = MkPillars {
        spades = Nothing,
        clubs = Nothing,
        hearts = Nothing,
        diamonds = Nothing
    }

-- The board consists of a deck, discard pile, pillars, and 7 columns.
data Board = MkBoard {
    boardDeck :: [Card],
    boardDiscard :: [Card],
    boardPillars :: Pillars,
    boardColumns :: [Column]
}
    deriving (Eq)


{- EXERCISE 3: Show boardDeck helper function -}
showDeck :: [Card] -> String
showDeck deck = "Deck size: " ++ show (length deck)

{- EXERCISE 3: Show boardDiscard helper function -}
showDiscard :: [Card] -> String
showDiscard [] = "Discard: <empty>"
showDiscard cards = "Discard: " ++ unwords (map show topCards)
  where
    topCards = (take 3 cards)

{- EXERCISE 3: Show boardPillars helper function -}
showPillars :: Pillars -> String
showPillars pillars = unlines [
    "  Spades: " ++ showValue (spades pillars),
    "  Clubs: " ++ showValue (clubs pillars),
    "  Hearts: " ++ showValue (hearts pillars),
    "  Diamonds: " ++ showValue (diamonds pillars)
  ]
  where
    showValue Nothing = "<empty>"
    showValue (Just value) = show value

{- EXERCISE 3: Show boardColumns helper function -}
type Newcolumn = [Maybe (Card, Bool)]
convertColumn :: Column -> Newcolumn
convertColumn col = map Just col

showColumns :: [Column] -> String
showColumns columns = unlines (header : formattedRows)
  where
    header = "[0] [1] [2] [3] [4] [5] [6]"

    newColumns = map convertColumn columns
    maxColumnLength = maximum (map length newColumns)
    paddedColumns = map (padColumn maxColumnLength) newColumns

    
    padColumn :: Int -> Newcolumn -> Newcolumn -- refactor this
    padColumn maxLen newColumn = replicate (maxLen - length newColumn) Nothing ++ newColumn

    transposeRows = reverse $ transpose paddedColumns
    
    rows = map formatColumn transposeRows

    formattedRows = map (unwords . map pad) rows

    formatColumn :: Newcolumn -> [String] -- refactor this 
    formatColumn col = map formatCard col
      where
        formatCard (Just (card, True)) = show card
        formatCard (Just (_   , False)) = "???"
        formatCard Nothing = "   "

    pad :: String -> String
    pad s = s ++ ""

{- EXERCISE 3: Show instance for the board -}
{- We recommend writing helper functions. -}
instance Show Board where
    show board = deck ++ "\n" ++ discard ++ "\nPillars:\n" ++ pillars ++ "\n" ++ columns
      where
        deck = showDeck (boardDeck board)
        discard = showDiscard (boardDiscard board)
        pillars = showPillars (boardPillars board)
        columns = showColumns (boardColumns board)


{- EXERCISE 4: Board Setup -}
-- setup :: Deck -> Board
-- setup d = error "fill in 'setup' in Game.hs"

createColumn :: [Card] -> Column
createColumn [] = []
createColumn deck = reverse (zip (init deck) (repeat False) ++ [((last deck), True)])

takeAndRemove :: Int -> Int -> [Card] -> [Card]
takeAndRemove from to deck = take (to-from+1) (drop from deck)

setup :: Deck -> Board
setup deck =
    MkBoard {
        boardDeck = boardDeckValue,
        boardDiscard = [],
        boardPillars = MkPillars {
            spades = Nothing,
            clubs = Nothing,
            hearts = Nothing,
            diamonds = Nothing
        },
        boardColumns = boardColumnsValue
    }
    where
        boardColumnsValue = [
            createColumn (takeAndRemove 0 0 deck),

            createColumn (takeAndRemove 1 2 deck),

            createColumn (takeAndRemove 3 5 deck),

            createColumn (takeAndRemove 6 9 deck),

            createColumn (takeAndRemove 10 14 deck),

            createColumn (takeAndRemove 15 20 deck),

            createColumn (takeAndRemove 21 27 deck)]
        boardDeckValue = reverse (take 24 (reverse deck))

{- EXERCISE 5: Win checking -}
isWon :: Board -> Bool
isWon board =
    let pillars = boardPillars board
    in spades pillars == Just King &&
        clubs pillars == Just King &&
        hearts pillars == Just King &&
        diamonds pillars == Just King

{- Pillar helper functions -}
-- Gets the pillar for a given suit.
getPillar :: Pillars -> Suit -> Maybe Value
getPillar ps Spades = spades ps
getPillar ps Clubs = clubs ps
getPillar ps Hearts = hearts ps
getPillar ps Diamonds = diamonds ps

-- Decrements a pillar. 
decValue :: Maybe Value -> Maybe Value
decValue Nothing = Nothing
decValue (Just Ace) = Nothing
decValue (Just x) = Just (pred x)

-- Increments a pillar.
incValue :: Maybe Value -> Maybe Value
incValue Nothing = Just Ace
incValue (Just x) = Just (succ x)

-- Increments the pillar for a given suit.
incPillar :: Pillars -> Suit -> Pillars
incPillar ps Spades = ps { spades = incValue (spades ps) }
incPillar ps Clubs = ps { clubs = incValue (clubs ps) }
incPillar ps Hearts = ps { hearts = incValue (hearts ps) }
incPillar ps Diamonds = ps { diamonds = incValue (diamonds ps) }

-- Decrements the pillar for a given suit.
decPillar :: Pillars -> Suit -> Pillars
decPillar ps Spades = ps { spades = decValue $ spades ps }
decPillar ps Clubs = ps { clubs = decValue $ clubs ps }
decPillar ps Hearts = ps { hearts = decValue $ hearts ps }
decPillar ps Diamonds = ps { diamonds = decValue $ diamonds ps }

{- EXERCISE 6: Helper Functions -}

-- Flips the top card of all columns, if not already flipped  TODO - Check is done properly, not happening inplace?
flipCards :: Board -> Board
flipCards board = board { boardColumns = map flipLastCard (boardColumns board) }
  where
    flipLastCard :: Column -> Column
    flipLastCard [] = []
    flipLastCard ((card, False):rest) = (card, True) : rest
    flipLastCard ((card, True):rest) = (card, True) : rest

-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack card onto =
    cardValue onto > cardValue card && -- not redundant, when onto was ace pred throws exception •`_´•
    cardValue card == pred (cardValue onto) &&
    ((isRed card && isBlack onto) || (isBlack card && isRed onto))

-- Updates a column at the given index TODO - Check is done properly, not happening inplace? of course its not inplace dofus
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn index column boardColumns = take index boardColumns ++ [column] ++ drop (index + 1) boardColumns -- im a genius if this oneliner works

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar c Nothing = cardValue c == Ace 
canStackOnPillar c (Just v) = cardValue c /= King && cardValue c == succ v -- redundant check? try avoiding error as canStack with pred

{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw board
    | not (null (boardDeck board)) = Right board { boardDeck = remainingDeck, boardDiscard = updatedDiscard }
    | not (null (boardDiscard board)) = Right board { boardDeck = newDeck, boardDiscard = [] }
    | otherwise = Left DeckEmpty
    where
        (topCard:remainingDeck) = boardDeck board
        updatedDiscard = topCard : boardDiscard board

        newDeck = reverse (boardDiscard board)

{-  -}

{- EXERCISE 8: Move -}
move :: Int -> Int -> Int -> Board -> Either Error Board
move cardCount from to board
    | cardStackable = Right board { boardColumns = newBoardColumns }
    | otherwise = Left WrongOrder
    where
        toColumn = (boardColumns board) !! to
        fromColumn = (boardColumns board) !! from
        checkFromCard = fst (last (take cardCount fromColumn))
        cardStackable = canStack checkFromCard (fst (head toColumn)) 
        newColumn = updateColumn to ((take cardCount fromColumn) ++ toColumn) (boardColumns board)
        newBoardColumns = updateColumn from (drop 1 fromColumn) newColumn

{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack from to b = error "fill in 'moveStack' in Game.hs"

{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard idx b = error "fill in 'moveFromDiscard' in Game.hs"

{- EXERCISE 11: Move to Pillar -} 
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar cs b = error "fill in 'moveToPillar' in Game.hs"
            
{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx b = error "fill in 'moveFromPillar' in Game.hs"

{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve board = error "fill in 'solve' in Game.hs"




{- Scaffolding: This checks input indexes and calls the relevant functions -}
checkStackIndex :: Int -> Either Error ()
checkStackIndex x | x >= 0 && x <= 6 = return ()
                  | otherwise = Left InvalidStack

makeMove' :: Command -> Board -> Either Error Board
makeMove' (Move count from to) b = do
    checkStackIndex from
    checkStackIndex to
    move count from to b
makeMove' (MoveStack from to) b = do
    checkStackIndex from
    checkStackIndex to
    moveStack from to b
-- If deck nonempty, move a card from the top of the deck to the top of the discard pile
-- If deck empty, reverse discard pile and put it back as deck
makeMove' Draw b = draw b
makeMove' (MoveFromDiscard idx) b = checkStackIndex idx >> moveFromDiscard idx b
-- Take the top card from the given stack and move to pillar -- if possible
makeMove' (MoveToPillar source) b =
    case source of
        FromDiscard -> moveToPillar source b
        FromStack idx -> checkStackIndex idx >> moveToPillar source b
makeMove' (MoveFromPillar suit idx) b = checkStackIndex idx >> moveFromPillar suit idx b
makeMove' Solve b = Right $ solve b

makeMove :: Command -> Board -> Either Error Board
makeMove cmd b = fmap flipCards (makeMove' cmd b)
