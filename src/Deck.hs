module Deck where
import System.Random
import Data.List (sort)
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types

{- Card definitions and instances -}
data Suit = Spades | Clubs | Diamonds | Hearts
    deriving (Eq, Ord) -- Arbitrary ordering, doesn't really matter

data Value = 
      Ace | Two | Three | Four | Five | Six | Seven
    | Eight | Nine | Ten | Jack | Queen | King
    deriving (Eq, Enum, Bounded)

instance Ord Value where
    compare c1 c2 = compare (fromEnum c1) (fromEnum c2)


{- Card representation, creation, and helper functions -}
data Card = MkCard { cardSuit :: Suit, cardValue :: Value }
    deriving (Eq, Ord)

mkCard :: Suit -> Value -> Card
mkCard suit value = MkCard { cardSuit = suit, cardValue = value }

isRed :: Card -> Bool
isRed c = cardSuit c == Hearts || cardSuit c == Diamonds

isBlack :: Card -> Bool
isBlack = not . isRed


{- Card Show instances -}
redStr :: String -> String
redStr str = (setSGRCode [SetColor Foreground Vivid Red]) ++ str ++ (setSGRCode [])

instance Show Suit where
    show Spades   = "♠"
    show Clubs    = "♣"
    show Diamonds = redStr "♦"
    show Hearts   = redStr "♥"

instance Show Value where
    show Ace = "A"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show other = show (fromEnum other + 1)

instance Show Card where
    show card = padding ++ str
        where 
            padding = replicate (2 - length (show (cardValue card))) ' '
            str = show (cardSuit card) ++ show (cardValue card)

{- Deck construction -}

type Deck = [Card]

{- EXERCISE 1: Deck Creation -}
deckOf52 :: [Card]
deckOf52 = [ MkCard suit value | suit <- [Spades, Clubs, Diamonds, Hearts], value <- [Ace .. King] ]

{- You can use this to check whether your shuffled deck contains the correct
 - cards -}
deckCorrect :: Deck -> Bool
deckCorrect deck = sort deck == sort deckOf52

{- Shuffling -}

{- EXERCISE 2 (helper): Swap card -}
swapCards :: Deck -> Int -> Int -> Deck
swapCards deck i j = [if k == i then deck !! j else if k == j then deck !! i else x | (k, x) <- zip [0..] deck]

{- EXERCISE 2 (helpers): Recursive call -}
recursiveShuffle :: Int -> StdGen -> Deck -> Deck
recursiveShuffle i rng deck
    | i >= length deck - 1 = deck
    | otherwise =
        let (j, rng') = randomR (i, length deck - 1) rng
            deck' = swapCards deck i j
        in recursiveShuffle (i + 1) rng' deck'

{- EXERCISE 2: Fisher-Yates Shuffle -}
shuffle :: StdGen -> Deck -> Deck
shuffle rng deck = recursiveShuffle 0 rng deck


{- shuffleDeck is called by Main.hs when setting up -}
shuffleDeck :: IO Deck
shuffleDeck = do
    gen <- initStdGen
    return $ shuffle gen deckOf52
