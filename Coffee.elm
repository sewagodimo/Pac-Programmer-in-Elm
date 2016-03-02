--the coffe class

import Signal  exposing (..)
import Time exposing (..) 
import Date exposing (year,hour, minute, second, fromTime) 
import Graphics.Element exposing (..) 
import Random 
import Window 
import Color exposing (..)
import Graphics.Collage exposing(..)
import Mouse
--import BoxesAndBubblesBodies exposing (..)
--import BoxesAndBubbles exposing (..)
--import Math2D exposing (mul2)
import Text exposing (fromString)
import List exposing(..)
import Signal exposing(..)
--import Anima
--import Mouse
--import ball exposing(..)

--type alias Ingridients = {}
--Model
--challengeGenerator =always (rem (Time.inMinutes/10) (round Time.inMinutes/10))
--challengeGenerator =always (rem (Time.inMinutes/10) (round Time.inMinutes/10))
setChallege index = if index==2 then mocha else if index==1 then latte else if index==3 then fredo else if index==4 then americano else if index==5 then hotchocolate else cuppachino

type Status = Playing | Won | Lost
type alias CoffeeGame = {ingridients : Ingridients
                        , state : Status, balls : List Ball,challenge : Ingridients,clicks: Int}
defaultCoffeeGame : CoffeeGame
defaultCoffeeGame = {ingridients =initaliseCoffee (0,0,0,0,0,0,""), state = Playing,balls =[],challenge = challenge,clicks=0 }
totalScore {a,b,c,d,e,f,g} = a+b+c+d+e+f
--a list of the bubbles that will be drawn when the player moves
--addBubble : List a -> Color -> (Int,Int)->List a --takes a bubble and then adds it to the list of bubbles, to be appanded with the score

type alias Ingridients ={water: Int, --a list of all the possible ingridients a coffe can have
                    milk:Int, 
                    caramel: Int,
                    espresso: Int, 
                    hotChocolate: Int, 
                    cream: Int,
                    name: String
                }
initaliseCoffee : (Int ,Int , Int, Int, Int,  Int,  String )-> Ingridients
initaliseCoffee (a,b,c,d,e,f,g) = { 
                    water = a,
                    milk =b,
                    caramel = c,
                    espresso = d,
                    hotChocolate = e,
                    cream = f,

                    name = g
    
        }
mixList : Ingridients -> String
mixList coffe =  let 
                    water = if coffe.water>0 then " Water :  "++(toString coffe.water)++"\n" else ""
                    milk = if coffe.milk>0 then " Milk :  "++(toString coffe.milk)++"\n" else ""
                    carString = if coffe.caramel>0 then " Caramel :  "++(toString coffe.caramel)++"\n" else ""
                    coffString = if coffe.espresso>0 then " Coffee :  "++(toString coffe.espresso)++"\n" else ""
                    hotchocString = if coffe.hotChocolate>0 then " Hotchocolate :  "++(toString coffe.hotChocolate)++"\n" else ""
                    creamString = if coffe.cream>0 then " Cream :  "++(toString coffe.cream)++"\n" else ""
                in coffe.name ++"\n"++ water ++ milk ++carString ++ coffString ++ hotchocString ++ creamString --++ "WTF"

--player : Ingridients
--challenge: Time -> Ingridients
challenge= superCoffe   --choose the coffe for the challenge depeding on the current time

mocha : Ingridients
mocha  = initaliseCoffee (0,40,0,30,20,10,"Mochachino")
americano : Ingridients
americano  = initaliseCoffee (50,0,0,50,0,0,"Americano")
cuppachino : Ingridients
cuppachino  = initaliseCoffee (0,20,0,40,10,30,"Cappuccino")
latte : Ingridients
latte  = initaliseCoffee (0,30,10,30,0,20,"Latte")
hotchocolate : Ingridients
hotchocolate  = initaliseCoffee (30,40,0,0,20,10,"HotChocolate")
fredo : Ingridients
fredo  = initaliseCoffee (0,30,30,40,0,0,"Fredo")
superCoffe : Ingridients
superCoffe = initaliseCoffee (0,0,0,0,0,5, "")

--!NB on ORDER, uncomment this it is important



--when the player clicks on the screen, return their windows dimensions

--coffeeLable : Ingridients -> String
--coffeeLable cofe = let a = cofe.name ++" "
 --                  in  if cofe


--View


squares col sides (x,y)= 
    rect (sides*2)  sides  --we make a nolygon with 7 sides and 50 rad, pass it
      |>filled col -- fill that shape with green
      |> move (x,y) --move it to this position :)
--mainbubble
view : (Int, Int) -> CoffeeGame -> Element
view (x,y) game =let 
                --(x,y) = (x'//2,y'//2)
                a = -x//2
                mugWidth = toFloat x/4
               -- -- (a,b) is exactly where in the screen the ingridients are
                machx =115+a --the space the machine will take up on the screen
                b = y//2
                machy =b-115
                c=  toFloat ((b*2)//6) --the edge of the screen
                d = round c
                menulist =if game.challenge==superCoffe then "" else (mixList game.ingridients)
                mug = rect mugWidth (mugWidth*0.9)
                        |>filled lightBrown
                        |> move (toFloat 0, toFloat -b+mugWidth/2.3)
                handle = rect (mugWidth/2) (mugWidth/10)
                        |>filled lightBrown
                        |>move (mugWidth-mugWidth/2,-210)
                handle2 = rect (mugWidth/2) (mugWidth/10)
                        |>filled lightBrown
                        |>move (mugWidth/2,toFloat -y/2.33)
                handle3 = rect (mugWidth/10) (mugWidth/1.9)
                        |>filled lightBrown
                        |>move (mugWidth/1.3,toFloat -y/3.09)
                title = if game.state==Lost then 
                                "Game Over \n Restart" |> fromString |> Text.bold |>Text.height 100 |> Text.color darkRed|> centered |> toForm |> move (100,30 )
                       else if game.state==Won then
                                "Coffee is Life" |> fromString |> Text.bold |>Text.height 100 |> Text.color lightPurple|> centered |> toForm |> move (0,0)
                        else "Playing" |> fromString |> Text.bold |>Text.height 40 |> Text.color purple|> centered |> toForm |> move (0,0)

                --here make a list of coffeeGame's bodies

            in collage x y  --this is where we determine the view depending on the playState
                    (List.append  
                    [ --  (toForm (show )),
                    --standard view has nothing to do with the game
                     image  (round (mugWidth*1.2) ) (round (mugWidth*1.2) ) "coffeeImage.png"  |>toForm |>move (toFloat 0, toFloat -b+mugWidth/2.3) ,title,
                    mixList game.challenge|> fromString |> Text.bold |>Text.height (toFloat y/25) |> Text.color purple|>  justified |> toForm |> move (0,toFloat (y-y//2-120) ),
                     "Ingredients" |> fromString |> Text.bold |>Text.height 40 |> Text.color purple|> centered |> toForm |> move (toFloat (x//2-x+100),toFloat (y-y//2-20) ),
                    "Right Click on the ingredients\n on the left to make\n the required coffee" |> fromString |> Text.italic |>Text.height (toFloat y/50) |> Text.color lightBlue|> centered |> toForm |> move (toFloat x/4,toFloat y/3 ),
                    
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
                     menulist|> fromString |> Text.bold |>Text.height (toFloat y/25) |> Text.color purple|>  justified |> toForm |> move (0,(-mugWidth+mugWidth/2) )
                    -- handle,handle2,handle3, 
                     
                    --,textured  "coffe.jpeg" (rect 400 400)

                    ]  (List.map  drawBall  game.balls) )
--bubbles : Ingridients -> (Float,Float)-> List Body String --returns a list of bodies depeding on the players score and the position of themug
--bubbles player (mugWidth,mugHeight) = let bodies = []
  --                                       if player.water >0

--Update
--checks if the mouse click and is in range of any box

coffeEquals : Ingridients -> Ingridients -> Status --determines if two coffees are the same, remember this oder
coffeEquals template player = if 
                                player.water>template.water || player.milk>template.milk 
                                || player.caramel>template.caramel || player.espresso>template.espresso
                                || player.cream>template.cream || player.hotChocolate>template.hotChocolate then Lost --meaning you have overmixed
                            else if 
                                player.water==template.water&& player.milk==template.milk 
                                && player.caramel==template.caramel&& player.espresso==template.espresso
                                && player.cream==template.cream&& player.hotChocolate==template.hotChocolate then Won -- meaning you won
                            else Playing
mouseDown = 0
updatePlayer : Input-> CoffeeGame->  CoffeeGame--, --the state will add to the player's elements
updatePlayer  input game   = --the xy is the window dimensions,  the player is all the existing inggridients
--check if he mouse clicks are within height
           let  
                --newClick = input.nr
                (x,y) = input.window
                (mousex,mousey) = input.mouse
                a = -x//2 
               -- -- (a,b) is exactly where in the screen the ingridients are
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
                newIngridients = game.ingridients
                myDown = if newClick<=game.clicks then True else False
               --  = if input.click==True && mouseDown==0 then  
                newChallenge= 
                                if game.challenge==superCoffe then 
                                    setChallege (round (input.delta*36.53)) 
                                else game.challenge
                name = newIngridients.name --after updating the score you shoul update the list of bubbles
                newBalls = game.balls
                onTarget = mousex<width && mousey>39  --this is a method to check if a click is within the range of the given value
                newClick =5-- if input.clicks==True && game.clicks==0 then 1 
                           --else if input.clicks==True then game.clicks+1 
                           --else 0
                updates=if onTarget == False ||input.click==False then (newIngridients,newBalls) --you are nowhere near target
                    else 
                        if   (range waterBox (mousex,mousey) height) then 
                        ( {newIngridients| water=newIngridients.water+ 5} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=blue,state=True}]) -- bubble 30 inf 0 (mug,0) (1,1) "Blue" )--now we figure out which target he it,thank God the order of the ingridients is not randomised
                    else 
                        if   (range milkBox (mousex,mousey)height) then 
                           ( {newIngridients| milk=newIngridients.milk+ 5} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=lightBrown,state=True}])
                    else 
                        if   (range coffeeBox (mousex,mousey)height) then 
                           ( {newIngridients| espresso=newIngridients.espresso+ 5} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=darkBrown,state=True}])
                    else 
                        if   (range chocBox (mousex,mousey)height) then 
                            ( {newIngridients| hotChocolate=newIngridients.hotChocolate+ 5} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=brown,state=True}]) --now we figure out which target he it,thank God the order of the ingridients is not randomised
                   else 
                      if   (range caramelBox (mousex,mousey)height) then 
                          ( {newIngridients| caramel=newIngridients.caramel+ 5} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=orange,state=True}])
                   else 
                      if (range creamBox (mousex,mousey)height)==True then 
                           ( {newIngridients| cream=newIngridients.cream+ 5} ,
                              List.append newBalls [{x=0,y=450,vy=0,col=lightYellow,state=True}]) --so the boxes work
                    else (newIngridients,newBalls)
               --   else (newIngridients,newBalls) --add ten if the player clicked on you and do nothing if we cannot figure out who clicked on you
                --gameState = game.state
                
                newState = coffeEquals game.challenge  (getX updates) 
                newGame=if newState ==Playing then 
                                ( if input.click == False then {game| balls =  updateAll input.delta (getY updates), challenge =newChallenge,clicks=newClick}
                                  
                                  else { ingridients = (getX updates), state = newState, balls =  updateAll input.delta (getY updates), challenge=game.challenge,clicks=newClick}
                               -- }
                                                        
                                                       
                                )
                        else if newState ==Won then {ingridients =initaliseCoffee (0,0,0,0,0,0, ""), state = Won,balls =[], challenge=setChallege (round (input.delta*36.53)),clicks=newClick}
                        else {ingridients =initaliseCoffee (0,0,0,0,0,0, ""), state = Lost,balls =[], challenge=setChallege (round (input.delta*36.53)),clicks=newClick} --restart the game if the player has lost 

          in newGame
--updateBubbles : 

  --a method to check if a value is in range of another
range (a,b) (x,y) height =     if  x> a then False   --a,b is the corners in the window, x,y is mouseclicks 
                                     else if y<=b then False
                                     else if y> b+height then False
                                     --else if y<10 then False
                                     else True   
inBox : Int ->Int -> Int -> Bool           
inBox center halfHeight yClick = if 
                                   ( center + halfHeight) > yClick && (center - halfHeight)<yClick ---determining if the mouse pointer is in a box
                                        then True
                                else False 
--type alias Input = {click:Bool, space: Bool, delta : Time} --you can either restart or click the game, you cannot pause, its too short 








--updateGame ->Input ->Game ->Game --will take in inputs and update the game, we update the games state depending on the user's input
getY (a,y) = y
getX (x,a)=x

--now we work on the balls

type alias Ball = {
    x : Float,
    y: Float,
    vy:Float,
    col: Color,
    state: Bool --whether the ball is falling or not
}
defaultBall = {x=0,y=420,vy=0,col=blue,state=True}
moveBall :Ball->Float->Ball
moveBall ball newX = {ball| x = newX }

balls : List { col : Color, state : Bool, vy : number, x : number', y : number'' }
balls = [defaultBall] --a list of  balls
newBall = [{x=0,y=420,vy=0,col=blue,state=True}]
addBalls:  List Ball ->Bool->List Ball
addBalls playingballs clicked = if clicked  then 
                                        List.append playingballs newBall
                                      else playingballs

{--}
updateBall : Time -> Ball-> Ball --takes the time, a  ball and the end point of the ball's decent
updateBall dt ball  =
  if ball.y<= -210 then  --dont move the ball when it has reached the bottton 
    {ball| col=(rgba 0 0 0 0)} --set the state to false so that we do not draw it :)
  else
    physicsUpdate dt ball 
    --}
updateAll : Time ->List Ball -> List Ball
updateAll dt balls = --map (\b -> { b | meta = b.meta })
                        List.map (\b ->updateBall dt b ) balls --it works :) yebo Girl


physicsUpdate dt obj = --changes the velocity of the ball
  { obj |
     -- x = obj.x + obj.vx * dt,
      y = obj.y - 20 * dt*pi-50
  }

drawBall : { a | col : Color, state : Bool, x : Float, y : Float } -> Form
drawBall ball= circle 50 
                    |> filled (if ball.state==False then lightBrown else ball.col)
                    |>move (ball.x,ball.y)

--setColor state col = if state==True then col else lightBrown

type alias Input= {
    click: Bool, --click creates a ball
    mouse :(Int,Int),
    nr : Int,
    window : (Int,Int),
    delta : Time
}

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map5 Input
      Mouse.isDown
      Mouse.position
     (Signal.foldp (\clk count -> count + 1) 0 Mouse.clicks) 
      Window.dimensions
      delta 
--I inferred the type annotation so you can copy it into your code:
gameState : Signal CoffeeGame
gameState = Signal.foldp updatePlayer defaultCoffeeGame input
delta : Signal Float
delta =
  Signal.map inSeconds (fps 60)
main : Signal Element
main = Signal.map2 view Window.dimensions gameState  -- (show Mouse.position)--dimensions are int values

--now lets list at least one type of coffee

