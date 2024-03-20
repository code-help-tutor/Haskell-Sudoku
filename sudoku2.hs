WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-- Just a porting from http://norvig.com/sudoku.html, Mar 2015 by @nfunato --}

import Data.List ((\\),nub,delete,null,union)
import Data.Map  ((!))
import qualified Data.Map as M -- Map, adjust, fold, fromList, toList
import Control.Exception (assert)
import Text.Printf (printf)
import Data.Maybe (maybe)
import Control.Monad (foldM, msum)


-- ===================================================================
-- Square, Unit, allSqList, unitMap, peerMap
--

cp rs cs = [(r,c)| r<-rs,c<-cs] -- computing a cartesian product

rows = "ABCDEFGHI"
cols = "123456789"

type Square = (Char,Char)	    -- square at coordinate (Row,Col)
type Unit   = [Square]          -- square groups each of which shares digits

allSqList :: [Square]
allSqList = cp rows cols        -- all squares *sorted in display order*

-- a map from each square to 3 units, i.e. row_unit, col_unit and sq_unit
unitMap :: M.Map Square [Unit]
unitMap = M.fromList [(sq, [u|u<-allUnitList, sq`elem`u]) | sq<-allSqList]
  where allUnitList = sq_units ++ row_units ++ col_units
        sq_units  = [cp rs cs | rs<-["ABC","DEF","GHI"],cs<-["123","456","789"]]
        row_units = [cp [r] cols | r<-rows]
        col_units = [cp rows [c] | c<-cols]

-- a map from each square to all other squares in (unitMap!square)
peerMap :: M.Map Square [Square]
peerMap = M.fromList [(sq, set(unitMap!sq) \\ [sq]) | sq<-allSqList]
  where set = nub . concat


-- ===================================================================
-- Digit, Grid, digits, initial_grid, display_grid
-- 

type Digit = Char                 -- Digit ranges over digits below
type Grid  = M.Map Square [Digit] -- a map from each square to candidate digits

digits = "123456789" :: [Digit]

initial_grid :: Grid              -- each square initially holds all digits
initial_grid = M.fromList $ zip allSqList (repeat digits)

grid_string :: Grid -> String
grid_string grid = concatMap disp allSqList
  where disp (r,c) = printf (fmt r c) width (grid!(r,c))
        width    = 1 + M.fold (\elem acc -> max (length elem) acc) 0 grid
        hbreak   = concat [s,"+",s,"+",s,"\n"] where s = replicate(width*3+1)'-'
        fmt r c  = "%*s" ++
                   (if c `elem` "36" then " |" else "") ++
                   (if c=='9' then "\n" else "") ++
                   (if c=='9' && r `elem` "CF" then hbreak else "")

-- each grid cell shows a candidate digits string
display_grid :: Maybe Grid -> IO ()
display_grid = putStr . maybe "" grid_string


-- ===================================================================
-- Move, input_to_moves (and display_input)
-- 

type Move = (Square, Digit)

normalize_input :: String -> String
normalize_input  = subst '.' "0" . check_len 81 . filter valid_char 
  where paddings = ".0"   -- '0' is replaced with '.' by normalization 
        valid_char     = flip elem (paddings ++ digits)
        check_len n s  = assert (length s == n) s
        subst new olds = map (\x -> if x `elem` olds then new else x) 

-- the result list includes a padding move as (sq, '.') in its elements
input_to_moves'= zip allSqList . normalize_input

input_to_moves :: String -> [Move]
input_to_moves = filter ((`elem` digits) . snd) . input_to_moves'

-- only for debug
display_input :: String -> IO ()
display_input = display_grid . Just . input_to_grid 
  where input_to_grid :: String -> Grid
        input_to_grid = M.fromList . map (\(sq,ch)->(sq,[ch])) . input_to_moves'


-- ===================================================================
-- essential part of the solver
--

-- two constraints cause propagations according to an assignment, i.e.
-- 1. one value constraint
--    if a square takes only ONE VALUE, then eliminate the value from its peers.
-- 2. one place constraint
--    if a value can be at an only ONE PLACE in a unit, then place it there.

assign :: Grid -> Move -> Maybe Grid
assign gr (sq, d) = foldM eliminate gr $ zip (repeat sq) (delete d (gr!sq))

eliminate :: Grid -> Move -> Maybe Grid
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
                  [d'] -> foldM eliminate gr' $ zip (peerMap!sq) (repeat d')
                  _    -> Just gr'
    foldM (place d) just_gr' (unitMap!sq)

place :: Digit -> Grid -> Unit -> Maybe Grid
place d gr u =
  -- seek ONE PLACE constraint, and propagate it if exist
  case filter (\sq -> d `elem` (gr!sq)) u of
    []   -> fail "sudoku: place" -- Nothing
    [sq] -> assign gr (sq,d)
    _    -> return gr

search :: Grid -> Maybe Grid
search gr =              
  case [(len,(sq,ds))| (sq,ds)<-M.toList gr, let len=length ds, len/=1 ] of
    []     -> return gr
    tuples -> do let (_,(sq,ds)) = minimum tuples
                 msum [ assign gr (sq,d) >>= search | d<-ds ]


-- ===================================================================
-- main
--

assign_initial_moves :: String -> Maybe Grid
assign_initial_moves = foldM assign initial_grid . input_to_moves

solve :: String -> Maybe Grid
solve str = assign_initial_moves str >>= search


solveStr  :: String -> IO ()
solveStr str = display_grid $ solve str

solveFile :: FilePath -> IO ()
solveFile path = solveStr =<< readFile path

-- main = solveFile "hard1.txt" 
main = solveStr hard1			-- just for example (most time-consuming one)


--  ===================================================================
-- problems to be solved
--

-- note: "display_grid $ assign_initial_moves grid1/kmzn1" returns a solution

grid1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
grid3 = "  . . 5 |3 . . |. . . 8 . . |. . . |. 2 . . 7 . |. 1 . |5 . . ------+------+------4 . . |. . 5 |3 . . . 1 . |. 7 . |. . 6 . . 3 |2 . . |. 8 . ------+------+------. 6 . |5 . . |. . 9 . . 4 |. . . |. 3 . . . . |. . 9 |7 . ."
hard1 = ".....6....59.....82....8....45..0.....3....0...6..3.54...325..6.................."

kmzn1 = "...71246.....3.1.8.......734..3....565.....178....6..229.......5.1.9.....38124..."
kmzn2 = ".185........9..87...4..2...6..8..7......7......2..5..4...4..1...49..1........764."
kmzn3 = "..37.8............6..31....9.8..2..5..4...3..2..1..4.9....29...............8.47.."
tsd1 = "080260401000407906000030008100000090005000200040000007400070000308501000201043070"

tsd2 = ".6......7...5.3...9.1...3....37...2..2.6.8.9..4...51....7...4.2...3.1...2......3."

qii1 = "8..........36......7..9.2...5...7.......457.....1...3...1....68..85...1..9....4.."

web1 = "9.5...1...73.549...4.6.2.7.26.5...8....468....3...9.41.8.9.5.1...621.89...7...2.3"

-- http://qiita.com/yumura_s/items/4e759467d64f7a0cb335    
-- the most difficult sudoku problem(?) by a Finnish mathematician
web2 = "8..........36......7..9.2...5...7.......457.....1...3...1....68..85...1..9....4.."
