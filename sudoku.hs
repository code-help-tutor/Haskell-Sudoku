WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-- Just a porting from http://norvig.com/sudoku.html, Dec 2011 by @nfunato --}
import Data.List ((\\), delete, nub, null)
import Data.Map ((!))
import qualified Data.Map as M -- Map, adjust, fold, fromList, toList
import Control.Exception (assert)
import Text.Printf (printf)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM, msum)

type Square = (Char,Char)
type Unit   = [Square]
type SqMap a = M.Map Square [a]

type UnitMap = SqMap Unit
type PeerMap = SqMap Square

rows     = "ABCDEFGHI"
cols     = "123456789"
cross rs cs = [(r,c)| r<-rs, c<-cs] :: [Square]
squares  = cross rows cols :: [Square]
unitlist = [cross rs cs | rs<-["ABC","DEF","GHI"], cs<-["123","456","789"]] ++
           [cross rows [c] | c<-cols] ++ [cross [r] cols | r<-rows]   :: [Unit]
units    = M.fromList [(sq,[u|u<-unitlist, sq`elem`u]) | sq<-squares] :: UnitMap
peers    = M.fromList [(sq, set(units!sq) \\ [sq]) | sq<-squares]     :: PeerMap
             where set = nub . concat

-- ===================================================================

paddings = ".0"
digits   = "123456789"
type Digit   = Char
type Grid    = SqMap Digit

normalize_input  :: String -> String
normalize_input = subst '.' "0" . check_len 81 . filter valid_char 
  where valid_char    = flip elem (paddings ++ digits)
        check_len n s = assert (length s == n) s
        subst new olds = map (\x -> if x `elem` olds then new else x) 

read_input, read_input' :: String -> [(Square,Digit)]
read_input  = filter ((`elem` digits).snd) . read_input'
read_input' = zip squares . normalize_input 

display_input :: String -> IO ()
display_input = display_grid . read_to_grid 
  where read_to_grid = M.fromList . map (\(sq,ch)->(sq,[ch])) . read_input'

display_grid :: Grid -> IO () 
display_grid  = putStr . grid_string

grid_string :: Grid -> String
grid_string grid = concat [ disp r c | r<-rows, c<-cols ]
  where disp r c = printf (fmt r c) width (grid!(r,c))
        width    = 1 + M.fold (\elem acc -> max (length elem) acc) 0 grid
        hbreak   = concat [s,"+",s,"+",s,"\n"] where s = replicate(width*3+1)'-'
        fmt r c  = "%*s" ++
                   (if c `elem` "36" then " |" else "") ++
                   (if c=='9' then "\n" else "") ++
                   (if c=='9' && r `elem` "CF" then hbreak else "")

initial_grid = M.fromList $ zip squares (repeat digits) :: Grid

-- ===================================================================

test_parse_grid, test_main :: String -> IO ()
test_parse_grid = mapM_ display_grid . mapMaybe parse_grid . return
test_main       = mapM_ display_grid . mapMaybe solve      . return

main = mapM_ display_grid . mapMaybe solve =<< fmap lines (readFile "hard1.txt")
solve str  = search =<< parse_grid str

parse_grid = foldM assign initial_grid . read_input :: String -> Maybe Grid

search :: Grid -> Maybe Grid
search gr =              
  case [(len,(sq,ds))| (sq,ds)<-M.toList gr, let len=length ds, len/=1 ] of
    []     -> return gr
    tuples -> do let (_,(sq,ds)) = minimum tuples
                 msum [ assign gr (sq,d) >>= search | d<-ds ]

-- two constraints cause propagations according to an assignment, i.e.
-- 1. one value constraint
--    if a square takes only ONE VALUE, then eliminate the value from its peers.
-- 2. one place constraint
--    if a value can be at an only ONE PLACE in a unit, then place it there.

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign gr (sq, d) = foldM eliminate gr $ zip (repeat sq) (delete d (gr!sq))

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate gr (sq, d) =
  let ds = gr!sq in
  if d `notElem` ds
  then return gr
  else do
    let ds' = delete d ds
        gr' = M.adjust (const ds') sq gr
    -- seek ONE VALUE constraint, and propagate it if exist
    just_gr' <- case ds' of
                  []   -> Nothing
                  [d'] -> foldM eliminate gr' $ zip (peers!sq) (repeat d')
                  _    -> Just gr'
    foldM (place d) just_gr' (units!sq)

place :: Digit -> Grid -> Unit -> Maybe Grid
place d gr u =
  -- seek ONE PLACE constraint, and propagate it if exist
  case filter (\sq -> d `elem` (gr!sq)) u of
    []   -> Nothing
    [sq] -> assign gr (sq,d)
    _    -> return gr

-- ===================================================================

grid1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
grid3 = "  . . 5 |3 . . |. . . 8 . . |. . . |. 2 . . 7 . |. 1 . |5 . . ------+------+------4 . . |. . 5 |3 . . . 1 . |. 7 . |. . 6 . . 3 |2 . . |. 8 . ------+------+------. 6 . |5 . . |. . 9 . . 4 |. . . |. 3 . . . . |. . 9 |7 . ."
hard1 = ".....6....59.....82....8....45..0.....3....0...6..3.54...325..6.................."

do_test () =
  assert (length squares  == 81) True &&
  assert (length unitlist == 27) True &&
  assert (all (\sq -> length (units!sq) ==  3) squares) True &&
  assert (all (\sq -> length (peers!sq) == 20) squares) True &&
  assert (units!('C','2') == units_c2) True &&
  assert (null (peers!('C','2') \\ peers_c2)) True
  where
    units_c2 = [[('A','1'),('A','2'),('A','3'),('B','1'),('B','2'),('B','3'),
                 ('C','1'),('C','2'),('C','3')],
                [('A','2'),('B','2'),('C','2'),('D','2'),('E','2'),('F','2'),
                 ('G','2'),('H','2'),('I','2')],
                [('C','1'),('C','2'),('C','3'),('C','4'),('C','5'),('C','6'),
                 ('C','7'),('C','8'),('C','9')]]
    peers_c2 = [('A','2'),('B','2'),('D','2'),('E','2'),('F','2'),('G','2'),
                ('H','2'),('I','2'),
                ('C','1'),('C','3'),('C','4'),('C','5'),('C','6'),('C','7'),
                ('C','8'),('C','9'),
                ('A','1'),('A','3'),('B','1'),('B','3')]

-- eof
