--my really small ai
--but like what must happen
import Mouse
import List exposing   (..)
import Signal exposing (..)
import Signal.Extra exposing (..)
import Time exposing   (..)
import Text exposing   (..)
import List exposing   (..)
import Keyboard
import Color exposing  (..)
import Graphics.Collage exposing(..)
import Window
import List2D exposing (..)
import Graphics.Element exposing(..)
--import Date exposing (year,hour, minute, second, fromTime) 
--import Random 

                                                            ---THE COFFEE GAME ----
setChallege : Int -> Ingredients
setChallege index = 
    if index==0 then mocha else if index==1 then latte 
    else if index==3 then fredo else if index==4 then americano 
    else if index==5 then hotchocolate else cuppachino

type Status = CPlaying | CWon | CLost |CDone -- Done is changed by the timer
--totalScore: Ingredients->number
totalScore {a,b,c,d,e,f,g} = a+b+c+d+e+f

type alias Ingredients ={water: Int, --a list of all the possible ingredients a coffe can have
                    milk:Int, 
                    caramel: Int,
                    espresso: Int, 
                    hotChocolate: Int, 
                    cream: Int,
                    name: String
                }
initaliseCoffee : (Int ,Int , Int, Int, Int,  Int, String)  -> Ingredients
initaliseCoffee (a,b,c,d,e,f,g) = { 
                    water = a,
                    milk =b,
                    caramel = c,
                    espresso = d,
                    hotChocolate = e,
                    cream = f,

                    name = g
    
        }
mixList : Ingredients -> String
mixList coffe =  let 
                    water = if coffe.water>0 then " Water :  "++(toString coffe.water)++"\n" else ""
                    milk = if coffe.milk>0 then " Milk :  "++(toString coffe.milk)++"\n" else ""
                    carString = if coffe.caramel>0 then " Caramel :  "++(toString coffe.caramel)++"\n" else ""
                    coffString = if coffe.espresso>0 then " Coffee :  "++(toString coffe.espresso)++"\n" else ""
                    hotchocString = if coffe.hotChocolate>0 then " Hotchocolate :  "++(toString coffe.hotChocolate)++"\n" else ""
                    creamString = if coffe.cream>0 then " Cream :  "++(toString coffe.cream)++"\n" else ""
                in coffe.name ++"\n"++ water ++ milk ++carString ++ coffString ++ hotchocString ++ creamString --++ "WTF"

challenge : Ingredients
challenge= superCoffe   --choose the coffe for the challenge depeding on the current time

mocha : Ingredients
mocha  = initaliseCoffee (0,40,0,30,20,10,"Mochachino")
americano : Ingredients
americano  = initaliseCoffee (50,0,0,50,0,0,"Americano")
cuppachino : Ingredients
cuppachino  = initaliseCoffee (0,20,0,40,10,30,"Cappuccino")
latte : Ingredients
latte  = initaliseCoffee (0,30,10,30,0,20,"Latte")
hotchocolate : Ingredients
hotchocolate  = initaliseCoffee (30,40,0,0,20,10,"HotChocolate")
fredo : Ingredients
fredo  = initaliseCoffee (0,30,30,40,0,0,"Fredo")
superCoffe : Ingredients
superCoffe = initaliseCoffee (0,0,0,0,0,5, "")

--View
squares : Color -> Float -> ( Float, Float ) -> Form
squares col sides (x,y)= 
    rect (sides*2)  sides  --we make a nolygon with 7 sides and 50 rad, pass it
      |>filled col -- fill that shape with green
      |> move (x,y) --move it to this position :)
--mainbubble

coffeEquals : Ingredients -> Ingredients -> Status --determines if two coffees are the same, remember this oder
coffeEquals template player = if 
                                player.water>template.water || player.milk>template.milk 
                                || player.caramel>template.caramel || player.espresso>template.espresso
                                || player.cream>template.cream || player.hotChocolate>template.hotChocolate then CLost --meaning you have overmixed
                            else if 
                                player.water==template.water&& player.milk==template.milk 
                                && player.caramel==template.caramel&& player.espresso==template.espresso
                                && player.cream==template.cream&& player.hotChocolate==template.hotChocolate then CWon -- meaning you won
                            else CPlaying
--mouseDown = 0
updatePlayer : Input -> PacGame -> PacGame--, --the state will add to the player's elements
updatePlayer  input game   = --the xy is the window dimensions,  the player is all the existing inggridients
--check if he mouse clicks are within height
           let  
                --newClick = input.nr
                (x,y) = input.window
                (mousex,mousey) = input.mouse
                a = -x//2 
               -- -- (a,b) is exactly where in the screen the ingredients are
                machx =39+a --the space the machine will take up on the screen
                b = y//2
                machy =b-115
                width=  270 --the width of the boxes
                height = toFloat ((y)//6) -- the height of each of the boexs
                c=  toFloat ((b*2)//6)
                d = round c
                waterBox =  (width, 39 )   --the box is the corner values
                milkBox =(width, 39+height*1) 
                creamBox = (width, 39+height*2) 
                coffeeBox =  (width, 39+height*3) 
                chocBox =  (width, 39+height*4) 
                caramelBox = (width, 39+height*5) 
                newIngredients = game.ingredients
                myDown = if newClick<=game.clicks then True else False
               --  = if input.click==True && mouseDown==0 then  
                newChallenge= 
                                if game.challenge==superCoffe then 
                                    setChallege (random) 
                                else game.challenge
                name = newIngredients.name --after updating the score you shoul update the list of bubbles
                newBalls = game.balls
                onTarget = mousex<width && mousey>39  --this is a method to check if a click is within the range of the given value
                newClick = game.clicks+1

                updates = if onTarget == False || input.click == False then (newIngredients,newBalls) --you are nowhere near target
                    else 
                        if   (range waterBox (mousex,mousey) height) then 
                        ( {newIngredients| water=newIngredients.water+ 10} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=blue,cstate=True}]) -- bubble 30 inf 0 (mug,0) (1,1) "Blue" )--now we figure out which target he it,thank God the order of the ingredients is not randomised
                    else 
                        if   (range milkBox (mousex,mousey)height) then 
                           ( {newIngredients| milk=newIngredients.milk+ 10} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=lightBrown,cstate=True}])
                    else 
                        if   (range coffeeBox (mousex,mousey)height) then 
                           ( {newIngredients| espresso=newIngredients.espresso+ 10} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=darkBrown,cstate=True}])
                    else 
                        if   (range chocBox (mousex,mousey)height) then 
                            ( {newIngredients| hotChocolate=newIngredients.hotChocolate+ 10} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=brown,cstate=True}]) --now we figure out which target he it,thank God the order of the ingredients is not randomised
                   else 
                      if   (range caramelBox (mousex,mousey)height) then 
                          ( {newIngredients| caramel=newIngredients.caramel+ 10} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=orange,cstate=True}])
                   else 
                      if (range creamBox (mousex,mousey)height)==True then 
                           ( {newIngredients| cream=newIngredients.cream+ 10} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=lightYellow,cstate=True}]) --so the boxes work
                    else (newIngredients,newBalls)
               --   else (newIngredients,newBalls) --add ten if the player clicked on you and do nothing if we cannot figure out who clicked on you
                --gameState = game.state
                random = ( (round input.delta)//1)%
                          ( (round input.delta)//10) 
                newState = if game.level==1 && game.clicks>=180 then CDone
                           else if game.clicks>=140 then CDone
                          else  coffeEquals game.challenge  (getX updates) 
                newGame = if newState == CPlaying then 
                                  if input.click == False then {game | 
                                    balls =  updateAll input.delta (getY updates), 
                                    challenge = newChallenge, clicks = newClick}

                                  else {game | ingredients = (getX updates), 
                                  cstate = newState, balls =  updateAll input.delta (getY updates), 
                                  challenge = game.challenge, clicks = game.clicks+1}

                        else if newState == CWon then {game |
                                                      ingredients = initaliseCoffee (0,0,0,0,0,0, ""), 
                                                      cstate = CWon, balls =[], 
                                                      challenge = setChallege (random),
                                                      clicks = game.clicks+1, --add one for every game iteration
                                                      score = addscore game.score 1 0 0} --Add a life to the player for every win
                        else if input.click==True then
                            {game | cstate=CLost, clicks=if game.level==1 then 178 else 218}
                              
                        else {game |
                              ingredients = initaliseCoffee (0,0,0,0,0,0, ""), 
                              cstate = CLost, balls =[], 
                              challenge = setChallege (random),
                              clicks = 0,
                              time=0, --set all the time back to zero
                              score = addscore game.score 0 0 0, --move the player forward after winning
                              isCoffee=False,--return to the game
                              state=Break} --pause and allow the player to collet themselves

          in newGame
--updateBubbles : 

  --a method to check if a value is in range of another
range : ( comparable, number ) -> ( comparable, number ) -> number -> Bool
range (a,b) (x,y) height =     if  x> a then False   --a,b is the corners in the window, x,y is mouseclicks 
                                     else if y<=b then False
                                     else if y> b+height then False
                                     --else if y<10 then False
                                     else True   
inBox : Int -> Int -> Int -> Bool           
inBox center halfHeight yClick = if 
                                   ( center + halfHeight) > yClick && (center - halfHeight)<yClick ---determining if the mouse pointer is in a box
                                        then True
                                else False 


--now we work on the balls

type alias Ball = {
    x : Float,
    y: Float,
    vy:Float,
    col: Color,
    cstate: Bool --whether the ball is falling or not
}
--defaultBall: Ball
defaultBall = {x=0,y=420,vy=0,col=blue,cstate=True}

balls : List { col : Color, cstate : Bool, vy : number, x : number', y : number'' }
balls = [defaultBall] --a list of  balls

{--}
updateBall : Time -> Ball -> Ball --takes the time, a  ball and the end point of the ball's decent
updateBall dt ball  =  if ball.y <= -210 then  --dont move the ball when it has reached the bottton 
    {ball| col=(rgba 0 0 255 0)} --set the state to false so that we do not draw it :)
  else
    physicsUpdate dt ball 
    --}
updateAll : Time ->List Ball -> List Ball
updateAll dt balls = --map (\b -> { b | meta = b.meta })
                        List.map (\b ->updateBall dt b ) balls --it works :) yebo Girl
physicsUpdate: Float->Ball->Ball
physicsUpdate dt obj = --changes the velocity of the ball
  { obj |
     -- x = obj.x + obj.vx * dt,
      y = obj.y-60
  }

drawBall : { a | col : Color, cstate : Bool, x : Float, y : Float } -> Form
drawBall ball= circle 50 
                    |> filled (if ball.cstate==False then lightBrown else ball.col)
                    |> move (ball.x,ball.y)



                                                            ---THE PACMAN GAME ----

type alias PacGame ={player: Pacman , 
                    ghost1: Ghost, ghost2: Ghost, 
                    ghost3: Ghost, ghost4: Ghost, 
                    grid: List(List Int), state:GameState,level: Int,
                    score: Score,time: Int,
                    isCoffee: Bool,
                    ingredients: Ingredients,
                    cstate: Status, balls: List Ball, challenge: Ingredients, clicks: Int}
defaultGame : PacGame
defaultGame    =      {player = myPacman,
                      ghost1 = clyde, ghost2 = blinky, ghost3 = pinky, ghost4 = inky,
                      grid = List2D.level3,
                      state = Break, --Press enter to start
                      level = 1,
                      score = defaultScore, time = 0,
                      isCoffee = False,
                      ingredients =initaliseCoffee (0,0,0,0,0,0,""), cstate = CPlaying, balls = [], challenge = challenge,clicks=0
                      } 
nextLevel : PacGame -> PacGame                                                            
nextLevel game =let newlevel = game.level+1
                    --newGrid = if newlevel==2 then List2D.level2 else List2D.level3
                   

                 in {player=myPacman,ghost1=clyde, ghost2= blinky,ghost3=pinky, ghost4=inky,
                     grid = List2D.level3,
                     state= Break,
                     level = newlevel,
                    -- time=0,
                     score = game.score,time=0,
                     isCoffee = False,
                     ingredients = game.ingredients, cstate = game.cstate, balls = game.balls, challenge = game.challenge, clicks = 0}
nextLife : PacGame -> PacGame                                                            
nextLife game =let --newlevel = game.level
                    newGrid = List2D.level3
                   -- newPlayer={x=List2D.getX(List2D.startPoint ) ,y=List2D.getY(List2D.startPoint),vy=0,col=yellow,dir=Paused,state=True}
                    
                 --   newBlinky =
                    newScore = {points =game.score.points, multipliers=game.score.multipliers, mecha= game.score.mecha-1}
                 in {player=myPacman,ghost1=clyde, ghost2= blinky,ghost3=pinky, ghost4=inky,
                     grid = game.grid,
                     state= Break,
                     level = game.level,
                     score = newScore,
                     time=0,
                     isCoffee = game.isCoffee,
                     ingredients = game.ingredients, cstate = game.cstate, balls = game.balls, challenge = game.challenge, clicks = 0}



                                                                --Pacman movements
type alias Pacman = {
    x : Float,
    y: Float,
    vy:Float,
    col: Color,
    dir : Direction,
    state: Bool --true when eating                                          --whether the player is caught or not
}
--lis2D.cellSize = 20
 --the coffee game's position and what there was before it, and its
type GameState = Win | Lost | Playing | Break | Dead | Blue--what is happening in the game
type alias Score = {points:Int, multipliers: Int, mecha: Int} --multipliers will multiply the score and the collections are the number of other games played
defaultScore : Score
defaultScore = {points = 0, multipliers=0, mecha =3} --start with three lives, you have died if your mecha is below 0
addscore:Score->Int->Int->Int ->Score
addscore score a b c={score| points =score.points+a,multipliers=if (score.multipliers+b)==5 then 0 else (score.multipliers+b) ,
                                               mecha=  if (score.multipliers+b)==5 then score.mecha+c+1 else score.mecha+c} --well Done Girl
myPacman: Pacman --pacman is at the starting point on level one
myPacman = {x=List2D.getX(List2D.startPoint ) 
            ,y=List2D.getY(List2D.startPoint ),
            vy=0,col=yellow,dir=Paused,state=True}
type Direction = Left| Right| Up| Down |Paused

                                                                                     --THE GHOSTS--
type alias Ghost= {
    x : Float,
    y: Float,
    vy:Float,
    col: Color,
    dir : Direction,
    target : (Float,Float), --whether the player is caught or not
    state : Bool
}
clyde: Ghost --feeing ignorance 
clyde ={x=List2D.getX(List2D.startClyde List2D.level1),y=List2D.getY(List2D.startClyde List2D.level1),vy=0,col=orange, dir=Up,target =(myPacman.x,myPacman.y),state=False}

pinky: Ghost
pinky ={x=List2D.getX(List2D.startPinky List2D.level3),y=List2D.getY(List2D.startPinky List2D.level3),vy=0,col= (rgb 255 105 180), dir=Up,target =(myPacman.x,myPacman.y),state=True}

blinky: Ghost
blinky ={x=List2D.getX(List2D.startBlinky List2D.level3),y=List2D.getY(List2D.startBlinky List2D.level3),vy=0,col=lightRed, dir=Up,target =(myPacman.x,myPacman.y),state=False}

inky: Ghost
inky ={x=List2D.getX(List2D.startInky List2D.level3),y=List2D.getY(List2D.startInky List2D.level3),vy=0,col=(rgba 0 255 255 0), dir=Up,target =(myPacman.x,myPacman.y),state=False}

--the chaser- will only start to move towards pacman when he is 8 blocks away from him
dirString : Direction -> String
dirString dir = if dir==Left then "Left" else if dir==Right then "Right" else if dir==Down then "Down" else if dir==Up then " Up" else "Pause"
oppDir : Direction -> Direction
oppDir dir = if dir==Right then Left
             else if dir==Left then Right
             else if dir==Up then Down
             else if dir == Down then Left
             else dir
getX : ( a, b ) -> a
getX (a,b) = a
getY : ( a, b ) -> b
getY (a,b) = b

runGhost: Direction -> Ghost -> Ghost
runGhost dt obj = --The ghost runs based on the given direction
  { obj |x=
          if dt == Right then  obj.x +(List2D.cellSize)
          else if dt == Left then   obj.x - (List2D.cellSize)
          else obj.x
        ,y = if dt == Down then  obj.y - (List2D.cellSize)
             else if dt == Up then   obj.y + (List2D.cellSize)
             else obj.y,dir=dt}
--Blinky is the chaser
onGridBlinky : Ghost -> List (List Int) -> Direction -> Int --this returns where the ghost will be
onGridBlinky ghost grid dir= --you need to check what the previous direction was before moving
                      
                       if dir==Right then  --going right from up
                                   if (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0 && 
                                               (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0
                                            then (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y))
                                          else 0
                       else
                                         if dir==Left then
                                              if (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0 && 
                                               (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0
                                            then (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y))
                                          else 0

                      --else (List2D.inGrid grid (ghost.x+(List2D.cellSize/5),ghost.y))--determine if the player's move was allowe else not(List2D.inGrid grid (ghost.x-(List2D.cellSize/5),ghost.y+(List2D.cellSize/5))) 

                       else
                                         if dir==Up then --this is just to make sure that do dont
                                         if (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0 && 
                                               (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0
                                            then (List2D.inGrid grid (ghost.x,ghost.y+(List2D.cellSize/2)))
                                          else 0

                       else
                                         if dir==Down then --dont meddle
                                           if (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0 && 
                                               (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) /=0
                                            then (List2D.inGrid grid (ghost.x,ghost.y+(List2D.cellSize/2)))
                                          else 0
                       else (List2D.inGrid grid (ghost.x ,ghost.y)) --when its paused
                               


setBlinkyDirection :List (List Int) -> Ghost -> Direction -> (Float,Float) -> Ghost -- based on the relative distance between the players, determine the direction of the
setBlinkyDirection grid blinky  dir (a,b) = let 
                                            --  (right,left,up,down)= (onGrid blinky grid Right,onGrid blinky grid Left,onGrid blinky grid Up,onGrid blinky grid Down)-- [Right,Left,Up,Down]
                                              (ghostX,ghostY) = List2D.toInt grid (blinky.x,blinky.y)
                                              (targetX,targetY) = List2D.toInt grid (a+(if dir==Left then -cellSize*4 else cellSize*4),
                                                                                            b+(if dir ==Down then -cellSize*4 else cellSize*4) ) --))
                                              horiz = if (ghostX-targetX)<0 then Right else Left
                                              vert = if (ghostY-targetY)<0 then Down else Up
                                              prefr = --if abs(ghostY-targetY)==1 then (horiz,horiz) else
                                                      if abs(ghostX-targetX)<= abs(ghostY-targetY) then (vert,horiz)
                                                      else (horiz,vert) --the preference gets checked first
                                              previousDir = blinky.dir
                                              theDirection = if (onGridBlinky (runGhost (getX prefr) blinky ) grid (getX prefr))/=0 then                         (runGhost (getX prefr) blinky ) --if the preference is in the grid then take it
                                                              else if (onGridBlinky (runGhost blinky.dir blinky ) grid (getX prefr))/=0 then                         (runGhost blinky.dir blinky )
                                                             else if (onGridBlinky (runGhost (getY prefr) blinky ) grid (getY prefr))/=0 then                    (runGhost (getY prefr) blinky )
                                                            
                                                             else if (onGridBlinky (runGhost (oppDir (getX prefr)) blinky ) grid (oppDir (getX prefr) ))/=0 then (runGhost (oppDir (getX prefr)) blinky )--if the preference is in the grid then take it
                                                             else if (onGridBlinky (runGhost (oppDir (getY prefr)) blinky ) grid (oppDir (getY prefr) ))/=0 then (runGhost (oppDir (getY prefr)) blinky )
                                                             else blinky
                                        --we need a list of directions that are possible to go to        
                                      in theDirection
setPinkyDirection :List (List Int) -> Ghost -> Direction -> (Float,Float) -> Ghost -- based on the relative distance between the players, determine the direction of the
setPinkyDirection grid blinky  dir (a,b) = let 
                                            --  (right,left,up,down)= (onGrid blinky grid Right,onGrid blinky grid Left,onGrid blinky grid Up,onGrid blinky grid Down)-- [Right,Left,Up,Down]
                                              (ghostX,ghostY) = List2D.toInt grid (blinky.x,blinky.y)
                                              (targetX,targetY) = List2D.toInt grid (a+(if dir==Up then (List2D.cellSize*2) else if dir ==Down then (-List2D.cellSize*2) else 0 ),
                                                                                       b+if dir==Right then (List2D.cellSize*2) else if dir ==Left then (-List2D.cellSize*2) else 0) --))
                                              horiz = if (ghostX-targetX)<0 then Right else Left
                                              vert = if (ghostY-targetY)<0 then Down else Up
                                              prefr = --if abs(ghostY-targetY)==1 then (horiz,horiz) else
                                                      if abs(ghostX-targetX)<= abs(ghostY-targetY) then (vert,horiz)
                                                      else (horiz,vert) --the preference gets checked first
                                             -- previousDir = blinky.dir
                                              theDirection = if (onGridBlinky (runGhost (getX prefr) blinky ) grid (getX prefr))/=0 then                         (runGhost (getX prefr) blinky ) --if the preference is in the grid then take it
                                                             else if (onGridClyde (runGhost blinky.dir blinky ) grid (getX prefr))/=0 then                         (runGhost blinky.dir blinky )
                                                             else if (onGridClyde (runGhost (getY prefr) blinky ) grid (getY prefr))/=0 then                    (runGhost (getY prefr) blinky )
                                                             else if (onGridClyde (runGhost (oppDir (getX prefr)) blinky ) grid (oppDir (getX prefr) ))/=0 then (runGhost (oppDir (getX prefr)) blinky )--if the preference is in the grid then take it
                                                             else if (onGridClyde (runGhost (oppDir (getY prefr)) blinky ) grid (oppDir (getY prefr) ))/=0 then (runGhost (oppDir (getY prefr)) blinky )
                                                             else blinky
                                        --we need a list of directions that are possible to go to        
                                      in theDirection

--clyde is the feeing ignorance
setClydeDirection :List (List Int) -> Ghost -> (Float,Float) -> Ghost -- based on the relative distance between the players, determine the direction of the
setClydeDirection grid clyde (a,b)  = let 
                                            --  (right,left,up,down)= (onGrid clyde grid Right,onGrid clyde grid Left,onGrid clyde grid Up,onGrid clyde grid Down)-- [Right,Left,Up,Down]
                                              (ghostX,ghostY) = List2D.toInt grid (clyde.x,clyde.y)
                                              (targetX,targetY) = List2D.toInt grid (a,b)
                                              horiz = if (ghostX-targetX)<0 then Right else Left
                                              vert = if (ghostY-targetY)<0 then Down else Up
                                              prefr = --if abs(ghostY-targetY)==1 then (horiz,horiz) else
                                                      if abs(ghostX-targetX)<= abs(ghostY-targetY) then (vert,horiz)
                                                      else (horiz,vert) --the preference gets checked first
                                             
                                              theDirection = if (onGrid (runGhost (getX prefr) clyde ) grid (getX prefr))/=0 then                         (runGhost (getX prefr) clyde ) --if the preference is in the grid then take i
                                                            -- else if (onGrid (runGhost clyde.dir clyde ) grid ((getX prefr) ))/=0 then (runGhost clyde.dir clyde )
                                                             else if (onGrid (runGhost (getY prefr) clyde ) grid (getY prefr))/=0 then                    (runGhost (getY prefr) clyde )
                                                             else if (onGrid (runGhost (oppDir (getY prefr)) clyde ) grid (oppDir (getY prefr) ))/=0 then (runGhost (oppDir (getY prefr)) clyde )
                                                             
                                                             else if (onGrid (runGhost (oppDir (getX prefr)) clyde ) grid (oppDir (getX prefr) ))/=0 then (runGhost (oppDir (getX prefr)) clyde )--if the preference is in the grid then take it
                                                            
                                                             else clyde
                                        --we need a list of directions that are possible to go to        
                                      in theDirection
updateClyde : Ghost -> List(List Int) -> Pacman -> Ghost
updateClyde clyde grid man  = 
                               setClydeDirection grid clyde (man.x,man.y) 
scatterClyde : Ghost -> List(List Int) -> Ghost
scatterClyde clyde grid  = 
                               setClydeDirection grid clyde List2D.scatterClyde 


updateBlinky : Ghost -> List(List Int) -> Pacman -> Direction -> Ghost
updateBlinky blinky grid man  dir= 
                               setBlinkyDirection grid blinky dir (man.x,man.y)
scatterBlinky : Ghost -> List(List Int) -> Ghost
scatterBlinky blinky grid  = 
                               setClydeDirection grid blinky List2D.scatterBlinky


updatePinky : Ghost -> List(List Int) -> Pacman -> Ghost
updatePinky pinky grid man  = 
                               setPinkyDirection grid pinky man.dir (man.x,man.y) 
scatterPinky : Ghost -> List(List Int) -> Pacman -> Ghost
scatterPinky blinky grid  man= 
                               setPinkyDirection grid blinky man.dir  List2D.scatterPinky --when in scatter mode set the target to an edge

onGridClyde : Ghost -> List (List Int) -> Direction -> Int --this returns where the ghost will be
onGridClyde ghost grid dir= if dir==Right then (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2)))--determine if the player's move was allowe else not
                       else
                                         if dir==Left then (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2)))
                       else
                                         if dir==Up then (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2))) --works
                       else
                                         if dir==Down then --dont meddle
                                          (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y+(List2D.cellSize/2)))
                       else (List2D.inGrid grid (ghost.x ,ghost.y)) --when its paused
                               

onGrid : Ghost -> List (List Int) -> Direction -> Int --this returns where the ghost will be
onGrid ghost grid dir= if dir==Right then (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y))--determine if the player's move was allowe else not
                       else
                                         if dir==Left then (List2D.inGrid grid (ghost.x-(List2D.cellSize/2),ghost.y))
                       else
                                         if dir==Up then (List2D.inGrid grid (ghost.x,ghost.y+(List2D.cellSize/2)))
                       else
                                         if dir==Down then 
                                          (List2D.inGrid grid (ghost.x,ghost.y+(List2D.cellSize/2)))
                       else (List2D.inGrid grid (ghost.x ,ghost.y)) --when its paused

setTarget : Ghost -> List (List Int) -> (Float,Float) -> Ghost -- set the target and direction, depending on the grid
setTarget ghost grid (x,y)=let --set a target and change the player's direction to follow it
                      -- (x,y)= ghost.target
                            (ghostX,ghostY) = (5,5)
                            newDirection = if abs( abs(ghost.x)- abs(x)) <= (8*List2D.cellSize) then
                                         if ghost.x>x then Right else Left
                                     else if abs( abs(ghost.y)- abs(y)) <= (8*List2D.cellSize)  then 
                                               if ghost.y>y then Up else Down
                                     else Paused --how do we know if the ghost is in the grid
                         in {ghost|dir=newDirection,target=(x,y)}
                             
inRange : Ghost -> ( Float, Float) -> Bool                      --- else obj.dir}--only change the direction on mouse click
inRange ghost (a,b) = if abs( abs(ghost.x)- abs(a)) <= 8 then True
                      else if abs( abs(ghost.y)- abs(b)) <= 8 then True
                      else False --if False put ghost on pause

                                                                                                              --INPUT INPUT INPUT
type alias Keys = {x:Int,y:Int}
delta : Signal Float
delta = Signal.map inSeconds (Time.every 150)
deltaAnime : Signal Float
deltaAnime = Signal.map inMilliseconds (Time.every 113)
countClick : Signal Int
countClick = (Signal.foldp (\clk count -> count + 1) 0 Mouse.clicks)


                                                                                      ----  VIEW VIEW VIEW VIEW 

instrStr : String 
instrStr ="In order to move pacman around press the WASD keys,\n W -Up, A -Left, S- Down and D-Right\n Space to break or restart.\nAvoid the Ghosts and collect all the food \nDrink as much coffee as you can to get lives"
printScore : Score -> Int -> String
printScore score level =if score.mecha<0 then "No lives left" else "Level : "++toString level++ "\nScore\nPoints          : " ++ toString score.points ++"\nMultipliers : "++ toString score.multipliers ++"\nLives            : "++ toString score.mecha                                                                                            ---VIEW ---

drawPacman : { a | col : Color, state : Bool, x : Float, y : Float } -> Form
drawPacman man= circle (List2D.cellSize/2.5)--this is just to make it small not to actually change it
                    |> filled (if man.state==False then black else man.col)
                    |>move (man.x,man.y)
drawMouth : Pacman -> Color -> Form
drawMouth man col= if col==red then
                      circle (List2D.cellSize/3-1)--this 
                          |> filled red
                          |>move (man.x+(List2D.cellSize/4-1),man.y)
                  else if man.dir ==Right then
                     circle (List2D.cellSize/4-1)--this 
                          |> filled col
                          |>move (man.x+(List2D.cellSize/4-1),man.y)--we are just opening the mouth of pacman, to be more realistic
                 else if man.dir ==Left then
                     circle (List2D.cellSize/4-1)
                          |> filled col
                          |>move (man.x-(List2D.cellSize/4-1),man.y)
                 else if man.dir ==Down then
                     circle (List2D.cellSize/4-1)
                          |> filled col
                          |>move (man.x,man.y-(List2D.cellSize/4-1))
                 else if man.dir ==Up then 
                      circle (List2D.cellSize/4-1)--the default direction is up :)
                          |> filled col
                          |>move (man.x,man.y+(List2D.cellSize/4-1))
                 else circle (List2D.cellSize/6-1)--this 
                          |> filled yellow
                          |>move (man.x,man.y) 
drawGhost : Ghost->Direction -> Form
drawGhost ghost dir= --image  (round (mugWidth*1.2) ) (round (mugWidth*1.2) ) "coffeeImage.png"  |>toForm |>move (toFloat 0, toFloat -b+mugWidth/2.3) 
                  if ghost.col== (rgb 255 105 180) then 
                        image (round (List2D.cellSize/2)*2 ) (round (List2D.cellSize/2)*2) 
                      (if dir==Left then "paper.png"  
                        else if dir==Right then "paper.png" 
                        else if dir==Down then "paper.png" 
                        --else if dir==Up then "red.png"
                        else "paper.png" ) 
                        |>toForm
                        |>move (ghost.x,ghost.y)
                   --else if ghost.col== lightRed then  
                  else if ghost.col== orange then 
                      image (round (List2D.cellSize/2.5)*2 ) (round (List2D.cellSize/2)*2) 
                      (if dir==Left then "red_right.png"  
                        else if dir==Right then "red_left.png" 
                        else if dir==Down then "red_down.png" 
                        --else if dir==Up then "red.png"
                        else "red.png" ) 
                        |>toForm
                        |>move (ghost.x,ghost.y)
                   else if ghost.col== lightRed then 
                      image (round List2D.cellSize ) (round List2D.cellSize) --I got the orientation mixed up in paint and I did not have the energy to change it
                        (if ghost.dir==Left then "orange_left.png"  
                        else if ghost.dir==Right then "orange_right.png" 
                        else if ghost.dir==Down then "orange_down.png" 
                        else "orange.png" )
                        |>toForm
                        |>move (ghost.x,ghost.y)
                  else 
                  circle (List2D.cellSize/2.5-1)
                  |> filled ghost.col
                  |>move (ghost.x,ghost.y)
drawCoffee : Bool -> Form
drawCoffee bool = if bool==True then
                image (round (List2D.cellSize) ) (round (List2D.cellSize) ) "mug.png"
                        |>toForm
                        |>move List2D.mugPoint
                  else circle (List2D.cellSize/2.5-1)
                  |> filled (rgba 0 0 0 0)
                  |>move (0,0)  
showCoffee: PacGame->Bool
showCoffee game = (if game.level==1 then --set the different conditions for the coffee side
                      if game.clicks>=40 &&game.clicks<=180 then True --use this in the view
                          else False
                    else  if game.clicks>=80 &&game.clicks<=220 then True
                    else False )
view : PacGame -> (Int,Int) -> Float -> Element
view game (w,h) dt= 
              if game.isCoffee == False then 
                  let 
                        state = if game.state==Lost then 
                                  "Game Over" |> fromString |> Text.bold |>Text.height (toFloat w/15) |> Text.color red|> centered |> toForm |> move (toFloat -w/4.4,toFloat h/2.5)
                          else if game.state==Win then
                                  "Pac-Master!!" |> fromString |> Text.bold |>Text.height (toFloat w/13) |> Text.color yellow|> justified |> toForm |> move (toFloat -w/4.4,toFloat h/1.3)
                          else if game.state==Break then
                                   "Click to Play"
                                         |> fromString |> Text.bold |>Text.height (toFloat w/35) |> Text.color lightYellow|> centered |> toForm |> move (toFloat -w/4.9,toFloat h/1.7)
                          else if game.state==Dead then
                                   "Click to restart"
                                         |> fromString |> Text.bold |>Text.height (toFloat w/35) |> Text.color lightRed|> centered |> toForm |> move (toFloat -w/4.4,toFloat h/1.7)
                          else  "" |> fromString |> Text.bold |>Text.height 80 |> Text.color purple|> centered |> toForm |> move (0,0)

                        title 
                          =" Pac-programmer" |> fromString |> Text.bold |>Text.height (toFloat w/18) |> Text.color blue|> centered |> toForm |> move (toFloat w/4.3,toFloat h/1.28)
                        score=(printScore game.score game.level)|> fromString |> Text.bold |>Text.height (toFloat h/30) |> Text.color brown|> justified |> toForm |> move (toFloat w/5.1,toFloat h/2.6)
                        instr =
                          instrStr|> fromString |> Text.italic |>Text.height (toFloat w/70) |> Text.color lightBlue|> centered |> toForm |> move (toFloat w/4.4,toFloat h/1.7)
                  in    
                          collage w 1600 (List.append  (List2D.drawGrid game.grid) 
                          [if game.state/=Dead then drawPacman game.player else drawMouth game.player red, 
                          drawMouth game.player (if game.state/=Playing then black else if isEven dt then black  else rgba 0 0 0 0),
                          drawGhost game.ghost1 game.ghost1.dir,
                          drawGhost game.ghost2 game.ghost2.dir,
                          drawGhost game.ghost3 game.ghost3.dir,
                          drawGhost game.ghost4 game.ghost4.dir,
                          drawCoffee (showCoffee game) ,
                          title,instr,score, state
                          --, theLife|> fromString|>Text.height (toFloat w/100)  |> Text.color (rgb 255 105 180)|> justified |> toForm |> move (toFloat -w/4,toFloat -h/2-150)

                          ]
                          --rect 850 1000 |>filled black |> move (-330,300)]

                         )
              else
                let 
                    a = -w//2
                    mugWidth = toFloat w/4
                    machx = 115+a --the space the machine will take up on the screen
                    b = h//2
                    machy =b-115
                    c = toFloat ((b*2)//6) --the edge of the screen
                    d = round c
                    menulist =if game.challenge==superCoffe then "" else (mixList game.ingredients)
                    mug = rect mugWidth (mugWidth*0.9)
                            |>filled lightBrown
                            |> move (toFloat 0, toFloat -b+mugWidth/2.3)
                    handle = rect (mugWidth/2) (mugWidth/10)
                            |>filled lightBrown
                            |>move (mugWidth-mugWidth/2,-210)
                    handle2 = rect (mugWidth/2) (mugWidth/10)
                            |>filled lightBrown
                            |>move (mugWidth/2,toFloat -h/2.33)
                    handle3 = rect (mugWidth/10) (mugWidth/1.9)
                            |>filled lightBrown
                            |>move (mugWidth/1.3,toFloat -h/3.09)
                    progressBar = if game.level==1 then 
                                      if game.clicks>=178 then 0 
                                      else (toFloat (w- w//70*game.clicks//2) ) 
                                  else if  game.clicks>=218 then 0
                                      else (toFloat (w- w//70*game.clicks//2) ) 
                    title = if game.cstate == CLost then 
                                    "Coffee Break Over" |> fromString |> Text.bold |>Text.height 120 |> Text.color darkRed|> centered |> toForm |> move (100,toFloat (h-h//2-20)  )
                           else if game.cstate == CWon then
                                    "Coffee is Life\n extra life earned" |> fromString |> Text.bold |>Text.height 100 |> Text.color lightPurple|> centered |> toForm |> move (0,0)
                            else "Coffe Break" |> fromString |> Text.bold |>Text.height 40 |> Text.color purple|> centered |> toForm |> move (0,0)
                in collage w h  --this is where we determine the view depending on the playState
                    (List.append  
                    [ --  (toForm (show )),
                    --standard view has nothing to do with the game
                    rect progressBar 40   --we make a nolygon with 7 sides and 50 rad, pass it
                               |>filled red -- fill that shape with green
                               |> move ( toFloat (w//2-w+100),toFloat (h-h//2-20) ) --move it to this position :)
                      ,
                     image  (round (mugWidth*1.2) ) (round (mugWidth*1.2) ) "coffeeImage.png"  |>toForm |>move (toFloat 0, toFloat -b+mugWidth/2.3) , title,
                    mixList game.challenge|> fromString |> Text.bold |>Text.height (toFloat h/25) |> Text.color purple|>  justified |> toForm |> move (0,toFloat (h-h//2-120) ),
                     "Ingredients" |> fromString |> Text.bold |>Text.height 40 |> Text.color black|> centered |> toForm |> move (toFloat (w//2-w+100),toFloat (h-h//2-20) ),
                    "Right Click on the ingredients\n on the left to make\n the required coffee\nIn time" |> fromString |> Text.italic |>Text.height (toFloat h/50) |> Text.color lightBlue|> centered |> toForm |> move (toFloat w/4,toFloat h/3 ),
                     
                      
                    squares lightBlue c  (toFloat machx,toFloat machy),
                    "Water" |> fromString |> Text.bold |>Text.height 40 |> Text.color white|> centered |> toForm |> move (toFloat machx,toFloat machy),
                    
                     squares  lightBrown   c  (toFloat machx,toFloat (machy-d)),
                     "Milk" |> fromString |> Text.bold |>Text.height 40 |> Text.color black|> centered |> toForm |> move (toFloat machx,toFloat (machy-d) ),
                     
                     squares lightYellow c  (toFloat machx, toFloat(machy- d*2) ),
                     "Cream" |> fromString |> Text.bold |>Text.height 40 |> Text.color white|> centered |> toForm |> move  (toFloat machx, toFloat(machy- d*2) ),
                   
                     squares  darkBrown   c  (toFloat machx,toFloat (machy-d*3)),
                     "Coffee" |> fromString |> Text.bold |>Text.height 40 |> Text.color white|> centered |> toForm |> move (toFloat machx,toFloat (machy-d*3) ),
                    
                     squares brown c  (toFloat machx,toFloat (machy-d*4)),
                    "Hot Chocolate" |> fromString |> Text.bold |>Text.height 40 |> Text.color white|> centered |> toForm |> move (toFloat machx,toFloat (machy-d*4) ),

                     squares  orange   c  (toFloat machx,toFloat (machy-d*5)),
                   "Caramel" |> fromString |> Text.bold |>Text.height 40 |> Text.color white|> centered |> toForm |> move (toFloat machx,toFloat (machy-d*5))
                     ,--mug,
                     menulist|> fromString |> Text.bold |>Text.height (toFloat h/25) |> Text.color purple|>  justified |> toForm |> move (0,(-mugWidth+mugWidth/2) )
                    ]   (List.map  drawBall  game.balls))


--check if a value is divisible by two
isEven : Float -> Bool
isEven dt = if (round dt)%2==0 then True else False
--check if the game has ended
gameOver : PacGame -> GameState
gameOver game  = if   (List2D.samePosition game.grid (game.player.x,game.player.y) (game.ghost1.x,game.ghost1.y)) &&
                      (List2D.samePosition game.grid (game.player.x,game.player.y) (game.ghost2.x,game.ghost2.y)) &&
                      (List2D.samePosition game.grid (game.player.x,game.player.y) (game.ghost3.x,game.ghost3.y)) 
                          --(List2D.samePosition game.grid (game.player.x,game.player.y) (game.ghost4.x,game.ghost4.y)) 
                          then game.state
                else Dead



                                                ---UPDATE             UPDATE          UPDATE 

updatePacman :Input -> Pacman -> Pacman                                      --update the Pacman using the delta
updatePacman input man = --take the keys and change the direction
               man |> turn input.dir
                    |> run input.delta  --so the player can either run or turn

run: Float -> Pacman -> Pacman
run dt obj = --changes the velocity of the Pacman
  { obj |x=
          if obj.dir == Right then
                        if  (obj.x  )>= cellSize
                              then List2D.getX(startPoint )
                        else obj.x +(List2D.cellSize)
          else if obj.dir == Left then   
                              if (obj.x )<= -cellSize*18
                                then cellSize
                              else obj.x - (List2D.cellSize) --List2D.cellSize*20 
         else obj.x
        ,y = if obj.dir == Down then  obj.y - (List2D.cellSize)
             else if obj.dir == Up then   obj.y + (List2D.cellSize)
             else obj.y
    -- else if you are not moving right down or left then you are moving  left

  }
turn: Keys -> Pacman -> Pacman
turn keys obj={obj| dir=if keys.x==1 then 
                           Right 
                        else if keys.x== -1 then 
                            Left 
                        else if keys.y==1 then
                             Up 
                        else if keys.y== -1 then
                         Down 
                        else obj.dir}--only change the direction on mouse click


updateGame : Input -> PacGame -> PacGame
updateGame input game = let
                              --update the player 
                            newPlayer = if game.state/=Playing  then game.player 
                                       -- else if (toInt (game.player.x,game.player.y))
                                        else (updatePacman input game.player) 
                           --check if the new player position is on the grid before printing 
                            onGrid = if game.player.dir==Right then (List2D.inGrid game.grid (newPlayer.x-(List2D.cellSize/2),newPlayer.y))--determine if the player's move was allowe else not
                                     else
                                         if game.player.dir==Left then (List2D.inGrid game.grid (newPlayer.x-(List2D.cellSize/2),newPlayer.y))
                                     else
                                         if game.player.dir==Up then (List2D.inGrid game.grid (newPlayer.x,newPlayer.y+(List2D.cellSize/2)))
                                     else
                                         if game.player.dir==Down then 
                                          (List2D.inGrid game.grid (newPlayer.x,newPlayer.y+(List2D.cellSize/2)))
                                    else (List2D.inGrid game.grid (newPlayer.x ,newPlayer.y)) --when its paused
                          -- update the score
                            newScore = if 
                                    onGrid==0||onGrid==1 then game.score 
                                    else if onGrid==2 
                                        then addscore game.score 1 0 0
                                    else  addscore game.score 2 1 0 --updated the score yebo girl --if score is 200 then you won
                          --update the grid
                            newGrid = if onGrid/=0 then (List2D.setGrid game.grid ( newPlayer.x) (newPlayer.y) )
                                    else game.grid
                            
                          --now check if the score of the player is a winning score :)
                            newState = if game.level==1 && game.score.points>=250 then Win --80
                                      else if game.level==2 && game.score.points>= 500 then Win --180
                                      else if game.level==3 && game.score.points==750 then Win --will put lost later when I have the ghost ai working
                                     -- else if game.player.dir /= Paused then Playing
                                      else if game.state ==Break &&  input.clicks % 2  ==1 then Playing
                                      else if game.state ==Playing &&  input.clicks % 2  ==0 then Break
                                      else if game.score.mecha<0 then Lost 
                                      else (gameOver game)
                            newClyde = if game.state/=Playing || (isEven input.delta &&game.level==1)
                                            then game.ghost1
                                       else if game.score.points<10 &&game.level==1--bug 
                                        then (scatterClyde game.ghost1 game.grid) 
                                         else if game.level>=2 &&game.score.points<20 
                                             then (scatterClyde game.ghost1 game.grid) 
                                      else (updateClyde game.ghost1 game.grid game.player)

                            newBlinky = if game.state/=Playing || (isEven input.delta &&game.level==1)
                                                              then game.ghost2 
                                        else if game.score.points<20 &&game.level==1 --pumpkin
                                           then (scatterBlinky game.ghost2 game.grid)
                                        else if game.level==2   
                                             then (updateBlinky game.ghost2 game.grid game.player game.ghost3.dir)
                                        else if game.level==3 &&game.score.points<40 
                                             then (scatterBlinky game.ghost2 game.grid)
                                        else (updateBlinky game.ghost2 game.grid game.player game.ghost3.dir)

                            newPinky = if game.state/=Playing||(isEven input.delta &&game.level==1) --a pause/slow down clause
                                                        then game.ghost3 --paperwork
                                       else if game.score.points<80 &&game.level==2 
                                              then (scatterPinky game.ghost3 game.grid game.player)
                                       else if game.level==1 &&game.score.points<30 
                                            then (scatterPinky game.ghost3 game.grid game.player)
                                       else (updatePinky game.ghost3 game.grid game.player)
                            showCoffe = showCoffee game --determines if the coffee should be showing
                            --newGrid = if showCoffe then (addCoffee input.delta mygrid) else mygrid 
                            isCoffe=if showCoffe==True && (List2D.toInt List2D.level3 (game.player.x,game.player.y) )==(9,11) then True 
                                      else False
                            --game = { game | isCoffee = isCoffe }

                        in --if  newState==Lost && input.space==False then {game|state = Lost} else 
                           if  newState==Dead &&newScore.mecha==0 && input.click == False then {game|state=Lost} --what to do after restarting
                           else if newState==Lost then defaultGame
                           else if   newState==Win && input.click==True then nextLevel game --now you want the player to progress
                           else if  newState==Dead && input.click==True then nextLife game      
                           else {game| player =if onGrid==0 then game.player --handles the break and the pause
                                         else newPlayer, score = newScore, grid= newGrid,
                                         isCoffee=isCoffe, 
                                         state= newState,ghost1=newClyde,ghost2=newBlinky,ghost3 = newPinky,
                                         clicks=if newState == Playing then game.clicks+1 else game.clicks}  --hold back on the coffee break after any other break


updateMerge : Input -> PacGame->PacGame
updateMerge input game = 
                          if game.isCoffee == False then updateGame input game
                          else updatePlayer input game

type alias Input = 
                   {click: Bool,
                    mouse :(Int,Int),
                    clicks :Int, 
                    dir: Keys,
                    window : (Int,Int),
                    delta:Time
                   }

--the game state variable
input : Signal Input
input =
  Signal.sampleOn delta <|
    Input <~
      (Mouse.isDown) ~
      (Mouse.position) ~
      countClick ~
      (Keyboard.wasd) ~
      (Window.dimensions) ~
      (Signal.map inSeconds(Time.every 153))

gameState : Signal PacGame
gameState = (Signal.foldp updateMerge defaultGame input )



main : Signal Element
main = Signal.map3 view gameState Window.dimensions deltaAnime
{-- main =
  Markdown.toHtml markdown


markdown = """

# This is Markdown

[Markdown](http://daringfireball.net/projects/markdown/) lets you
write content in a really natural way.

  * You can have lists, like this one
  * Make things **bold** or *italic*
  * Embed snippets of `code`
  * Create [links](/)
  * ...

The [elm-markdown][] package parses all this content, allowing you
to easily generate blocks of `Element` or `Html`.

[elm-markdown]: http://package.elm-lang.org/packages/evancz/elm-markdown/latest

"""

  --}
--Markdown.toHtlml theLife
theLife : String
theLife = """
    # **Life of a Programmer** *by* [**Ednecia Matlapeng**] (https://github.com/sewagodimo)
     Elm is oa new and exciting functional programming language, functional programming 
     According to wikipedia a [functional programming](https://en.wikipedia.org/wiki/Functional_programming) is :\n
     *a style of building the structure and elements of computer programs—that treats computation as the evaluation of\n 
      mathematical functions and avoids changing-state and mutable data. It is a declarative programming paradigm
    *which means programming is done with expressions. 
     *the output value of a function depends only on the arguments that are input to the function, 
     *so calling a function f twice with the same value for an argument x will produce the same result f(x) each time. 
     *Eliminating side effects, i.e. changes in state that do not depend on the function inputs, 
     *can make it much easier to understand and predict the behavior of a program, 
     *which is one of the key motivations for the development of functional programming.


    #Using Elm to make a game that somehow shows the life of a Programmer
    At first I thought but programmers are not that cool, I mean we code coffee and repeat
    so I thought this game is going to be really boring. After a couple of brainstorming sessions with 
    my mentor Ramsay I finally decided to make pacman, which I thought was more interesting (yes an could
    have been more interesting) but towards the end Ramsay suggested that I merge the two. Instead of just remaking
    the arcade game, why not add some spunk into it. So I added some common enemy to the everyday programmers.
    The ghosts of the game are 
              *paperwork
              *Bugs
              *Procastination (The halloween mask), anything silly really. **We love the Silly**
    #The Perk: The Coffee Machine (The haven)
      After a certain amount of time the limited tim coffee cup shows up one the screen
      When the user gets to it they enter the world of coffee, it is here they must make the required coffee 
      to earn an extra life. Anyone can tell you, this analogy is KEY.
    #Progression
      *Reduced thinking time, the ghosts move faster and spend less time thinking
      *Larger time intervals for cofffe breaks
      *Ghosts have reduced scatter time

      #What I think about Elm
      """