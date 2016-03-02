module List2D where

import Array
import List
import Random exposing(..)
import Graphics.Collage exposing(..)
import Graphics.Element exposing(..)
import Color exposing (..)
import Text exposing (fromString)

{-- This library was made by Ednecia Matlapeng
for 2D array manipulation in Elm
you have to constantly switch between using Arrays and Lists
in elm because with Arrays you can use getters and setters
but with lists you can actually have mutable values :)


    --}
--goest into the 2D list and then retrieves avalue from it

--an example of a 2D list 



level1 : List (List Int)
level1 = [[0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0],
          [0,2,2,3,2,0,2,2,2,2,2,2,2,2,2,2,0],
          [0,2,2,2,2,2,2,2,2,2,2,2,3,2,2,2,0],
          [0,2,0,0,0,2,2,0,2,3,0,0,2,0,0,2,0],
          [0,2,2,2,0,2,2,0,2,2,2,0,2,0,3,2,0],
          [0,2,2,3,2,2,2,2,2,2,2,0,2,2,2,2,0],
          [0,0,2,3,2,2,2,2,2,0,2,2,2,2,0,2,0], --these are the entrance points they will always be   
          [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --for Lord Knows what reason I need a base of two walls so that the collision detection at the bottom can work
          [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]  ]

level2 : List (List Int)
level2 = [[0,0,0,0,0,0,0,0,0,0,0,0,0],
          [0,2,2,2,2,2,2,2,2,2,0,2,0],
          [0,2,2,0,0,0,2,2,2,2,3,2,0],
          [1,2,2,2,2,2,2,0,0,0,2,2,0],
          [1,2,2,0,2,2,2,2,2,2,2,2,0], --these are the entrance points they will always be 
          [0,2,2,0,2,2,2,2,0,2,2,2,0],
          [0,2,2,2,2,0,2,3,0,2,2,2,0],
          [0,2,0,0,2,0,2,2,0,2,2,2,0],
          [0,2,2,2,2,0,2,3,2,2,0,0,0],
          [0,2,0,3,2,2,2,2,2,2,2,2,0], --9x9 
          [0,0,0,0,0,0,0,0,0,0,0,0,0],
          [0,0,0,0,0,0,0,0,0,0,0,0,0] ]

level3 : List (List Int)
level3 = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
          [0,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0],
          [0,3,0,0,2,0,0,0,2,0,2,0,0,0,2,0,0,3,0],
          [0,2,0,0,2,0,0,0,2,2,2,0,0,0,2,0,0,2,0],
          [0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0],
          [0,2,0,0,2,0,2,0,0,0,0,0,2,0,2,0,0,2,0], --these are the entrance points they will always be 
          [0,2,2,2,2,0,2,2,2,0,2,2,2,0,2,2,2,2,0],
          [0,0,0,2,2,0,0,0,2,0,2,0,0,0,2,2,0,0,0],
          [0,0,0,2,2,0,2,2,2,2,2,2,2,0,2,2,0,0,0],
          [1,1,1,2,2,0,2,2,1,1,1,2,2,0,2,2,1,1,1],
          [1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1],
          [0,0,0,2,0,2,2,2,2,2,2,2,2,2,0,2,0,0,0], 
          [0,0,0,2,0,2,2,0,0,0,0,0,2,2,0,2,0,0,0],  
          [0,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0],  
          [0,2,0,0,2,0,0,0,2,0,2,0,0,0,2,0,0,2,0],  
          [0,2,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,0],
          [0,0,2,0,2,0,2,0,0,0,0,0,2,0,2,0,2,0,0], 
          [0,2,2,2,2,0,2,2,2,0,2,2,2,0,2,2,2,2,0],  
          [0,3,0,0,0,0,0,0,2,0,2,0,0,0,0,0,0,3,0], 
          [0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0], 
          [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
         ,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
            ]
winningScore : Int-> Int
winningScore level = if level==1 then 60
                    else if level==2 then 65
                    else 200 --the final game has a score of 200


--conversts a 2D list to a 2D array
toArray : List (List Int) ->Array.Array (Array.Array Int)
toArray list = let  
                  rows = List.map ( \b-> Array.fromList b) list
               in Array.fromList rows
--the opposite of toArray
toList :Array.Array (Array.Array Int) -> List (List Int)
toList list = let arrlist =Array.toList list  
                  row =List.map ( \b-> Array.toList b) arrlist
              in  row

--changes the value at a certain point in the array
setVal : List (List Int) ->Int ->Int -> List (List Int)
setVal list x y  = --set the value in the list to one to show that it has been passed
                      let toChange = if (getVal x y list) ==0 then False else True --wether we should change
                                                                                 --the value or not
                          array = toArray list
                          newRow = Array.set x 1 (Array.fromList(
                                    (Maybe.withDefault [0] (  Array.get y ( Array.fromList( list)) )) ))
                          newList =(Array.set y newRow array ) --[[0]]
                      in  if toChange ==False then list
                          else (toList newList)
getVal: Int ->Int -> List (List Int) -> Int
getVal x y list = (Maybe.withDefault  0 (Array.get x  (Array.fromList( 
          (Maybe.withDefault [0] (  Array.get y ( Array.fromList( list)) ) )
         ))) )
--[[35,45,356,35],[4,34,353,43],[122,32,23,1],[434,34,23,13,63] ]

--insert here something to print a 2D array for pac man


cellSize : number
cellSize =40 --should depend on the window frame
       

drawCell : Int -> (Float,Float) -> Form
drawCell val (x,y) = if val==0  then
                      square cellSize --
                       |> filled (rgba 100 100 225 0.31)
                       |> move (-x*cellSize+cellSize,(y*cellSize- cellSize*2))
                    else if val == 1 then --clear
                      square cellSize 
                       |> filled (rgba 100 100 225 0)
                       |> move (cellSize-x*cellSize,(y*cellSize- cellSize*2))
                    else if  val == 3 then --Bigfood
                       circle (cellSize/7)
                            |>filled white
                            |>move (cellSize-x*cellSize,(y*cellSize- cellSize*2))
                    else if  val == 4 then --Bigfood
                       circle (cellSize/2-1)
                            |>filled lightBrown
                            |>move (cellSize-x*cellSize,(y*cellSize- cellSize*2))
                    else 
                       circle (cellSize/20) --SmallFoord
                            |>filled white
                            |>move (cellSize-x*cellSize,(y*cellSize- cellSize*2))
getList : (Int, List Int) -> List Int
getList (a,b) = b
getIndex: (Int, List Int) -> Int
getIndex (a,b) = a
getY:(number,number) ->number
getY (a,b) = b
getX : (number,number) ->number
getX (a,b) = a
                   
drawRow: (Int, List Int) -> Float->Float->List Form
drawRow  (index, row) y x= let     -- max = row
                                indexedList =(Array.toIndexedList (Array.fromList row))
                         in List.map (\b-> drawCell (getY b ) (x-(toFloat (getX b)),y-(toFloat index) )  ) indexedList --b is (Int,Int)

drawGrid : List (List Int) -> List Form
drawGrid list =let y = toFloat( List.length list )
                   lists =(Array.toIndexedList (Array.fromList list)) 
               in (List.append [rect 900 1000 |>filled black |> move (-350,300) ]
                  (List.concat(List.map (\b-> drawRow b y (toFloat (List.length (getList b))) ) lists) ) )

                                ---MOVING Around the GRID
                    --Where to first place the player in the grid
startPoint :  ( Float, Float ) --for the player
startPoint  =let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( level3)) ) ) ) --
                 height= toFloat( List.length level3 )
             in --if list==level1 then (-(width-9)*cellSize+ cellSize,(height)*cellSize- cellSize*2 )
                   --if list==level2 then (-(width)*cellSize+ cellSize,(height-3)*cellSize- cellSize*2 )
                   --else 
                    (-( (width)*cellSize)+ cellSize,(height-9)*cellSize- cellSize*2 )
mugPoint :  ( Float, Float ) --for the player
mugPoint  =let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( level3)) ) ) ) --
               height= toFloat( List.length level3 )
             in --if list==level1 then (-(width-9)*cellSize+ cellSize,(height)*cellSize- cellSize*2 )
                   --if list==level2 then (-(width)*cellSize+ cellSize,(height-3)*cellSize- cellSize*2 )
                   --else 
                    (-( (width-9)*cellSize)+ cellSize,(height-11)*cellSize- cellSize*2 )
atEnd : List (List Int) -> ( Float, Float ) ->(Float,Float)
atEnd list (x,y)=let width=  (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( list)) ) ) ) --
                     height= ( List.length list )
                     (a,b) = toInt list (x,y)
                 in 
                    if a>=width-1 && (b==height||b==height-1) then (toFloat ( (-width)*cellSize)+ cellSize,toFloat(b-9) *cellSize- cellSize*2 )
                    else if a<=0 && (b==height-9||b==height-10) then (toFloat cellSize,toFloat((b-9)*cellSize- cellSize*2) ) 
                    else (x,y)

startClyde : List (List Int) -> ( Float, Float )
startClyde list =let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( level3)) ) ) ) --
                     height= toFloat( List.length level3 )
                  --in if list==level1 then (-(width-5)*cellSize+ cellSize,(height-5)*cellSize- cellSize*2 )
                  -- else if list==level2 then (-(width-11)*cellSize+ cellSize,(height-2)*cellSize- cellSize*2 )
                 in (-( (width-8)*cellSize)+ cellSize,(height-9)*cellSize- cellSize*2 )
startBlinky : List (List Int) -> ( Float, Float )
startBlinky list = let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( list)) ) ) ) --
                       height= toFloat( List.length list )
                   in   (-( (width-9)*cellSize)+ cellSize,(height-8)*cellSize- cellSize*2 )

startInky : List (List Int) -> ( Float, Float )
startInky list = let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( list)) ) ) ) --
                     height= toFloat( List.length list )
                   in   (-( (width-9)*cellSize)+ cellSize,(height-9)*cellSize- cellSize*2 )
startPinky : List (List Int) -> ( Float, Float )
startPinky list = let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( level3)) ) ) ) --
                      height= toFloat( List.length level3 )
                   in   --if list==level2 then  (-( (width-9)*cellSize)+ cellSize,(height-9)*cellSize- cellSize*2 ) 
                       (-( (width-10)*cellSize)+ cellSize,(height-9)*cellSize- cellSize*2 )
scatterClyde:(Float,Float)
scatterClyde = let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( level3)) ) ) ) --
                   height= toFloat( List.length level3 ) 
               in (-( (width-(width-3))*cellSize)+ cellSize,(height-(20))*cellSize- cellSize*2 )
scatterBlinky:(Float,Float)
scatterBlinky = let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( level3)) ) ) ) --
                    height= toFloat( List.length level3 ) 
                in (( (width)*cellSize)+ cellSize,(height-(20))*cellSize- cellSize*2 )
scatterPinky:(Float,Float)
scatterPinky = let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( level3)) ) ) ) --
                   height= toFloat( List.length level3 ) 
               in (-( (width-(width-3))*cellSize)+ cellSize,(height)*cellSize- cellSize*2 )


            --Moving the PLayer
            --Moving the PLayer
            --Moving the PLayer
--type alias Keys = {x:Int,y:Int} --input from the user, they keys press left or right
--move : List(List Int) -> (Int, Int)->(Float,Float) --takes a grid and then dertermines what the input was
--inGrid: List (List Int) ->(Float ,Float) ->Bool --make it return a bool at first to check if it works properly then after make it return actual values
inGrid : List (List Int) -> ( Float, Float ) -> Int --yeah it works
inGrid list (x,y)=let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( list)) ) ) ) --
                      height= toFloat( List.length list ) 
                      x'=(round (width-(((abs x) )/cellSize)) ) - 1 --we need to find actuall values and check them again the grid
                      y'=round (height - (((abs y) + 2*cellSize)/cellSize)  )
                      check = getVal x' y' list --checks if a move is in the grid

                  in  check
--if inGrid==0, dont move, if check == 2 then Score, then move, else just move
--only move if inGrid is true
toInt: List(List Int) ->(Float,Float)->(Int,Int) --gets the x and y vals and converts them into their grid correspondents
toInt list (x,y)=let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( list)) ) ) ) --
                     height= toFloat( List.length list ) 
                     x'=(round (width-(((abs x) )/cellSize)) ) - 1 --we need to find actuall values and check them again the grid
                     y'=round (height - (((abs y) + 2*cellSize)/cellSize)  )
                     -- maps the value from the window to the grid :)
                  in  (x',y')

setGrid : List (List Int) ->Float ->Float -> List (List Int)
setGrid list x' y'  = --takes the position of the index on the screen and checks it against the screen
                      let width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( list)) ) ) ) --
                          height= toFloat( List.length list ) 
                          (x,y)=((round (width-(((abs x') )/cellSize)) ) - 1 ,round (height - (((abs y') + 2*cellSize)/cellSize)  ))
                          toChange = if (getVal x y list) ==0 then False else True --wether we should change
                                                                                 --the value or not
                          array = toArray list
                          newRow = Array.set x 1 (Array.fromList(
                                    (Maybe.withDefault [0] (  Array.get y ( Array.fromList( list)) )) ))
                          newList =(Array.set y newRow array ) --[[0]]
                      in  if toChange ==False then list
                          else (toList newList)
setGridInt : List (List Int) ->(Int,Int)-> List (List Int) --choose a random point and then set it to zero
setGridInt list (x, y ) = --takes the position of the index on the screen and checks it against the screen
                      let --width= toFloat (List.length(Maybe.withDefault [0] (  Array.get 0 ( Array.fromList( list)) ) ) ) --
                          --height= toFloat( List.length list ) 
                          --(x,y)=((round (width-(((abs x') )/cellSize)) ) - 1 ,round (height - (((abs y') + 2*cellSize)/cellSize)  ))
                          toChange = if (getVal x y list) ==0 then False else True --wether we should change
                                                                                 --the value or not
                          array = toArray list
                          newRow = Array.set x 0 (Array.fromList( --we want to set the value to zero
                                    (Maybe.withDefault [0] (  Array.get y ( Array.fromList( list)) )) ))
                          newList =(Array.set y newRow array ) --[[0]]
                      in  if toChange ==False then list
                          else (toList newList)

samePosition : List (List Int)->(Float,Float)->(Float,Float)->Bool
samePosition list (a,b) (x,y) =let 
                                    (playerx,playery) = toInt list (a,b)
                                    (targetx,targety) = toInt list (x,y)
                              in if playerx==targetx &&playery==targety then False
                                 else True --true means that they are not in the same position

type alias CoffeGame = {position: (Int,Int), revalue:Int, showing:Bool} --we place it in a position we know and then remove it later

--bbsetCoffe : CoffeeGame -> List (List Int)

addCoffee : Float->List (List Int)->List (List Int) --change the coffe spot
addCoffee d list=  setGridInt list (9,11)
                        

generator : ( Int, Int ) -> Random.Generator Int
generator (x, y)  =  (Random.int x y)

type alias Vect = (Int, Random.Seed)
getSeed :Vect -> Int
getSeed(x,y) = x

randomSeed : Float -> ( Int, Int ) -> Int
randomSeed t (x,y)=
  let
      now =  round(t*pi/2)
      seed1 = Random.initialSeed((now))
  in 
  getSeed( Random.generate (generator (x,y)) seed1 ) 



main : Element
main =
 ( collage 1500 1500 
                (List.append (drawGrid level3)
                ([--rect 850 1000 |>filled black |> move (-330,300),
                  circle (cellSize/2) |>filled green |> move (startPoint),
                   (toString ( inGrid level3 (startPoint )  )++"why?" ) |> fromString |> Text.bold |>Text.height 100 |> Text.color orange|> centered |> toForm |> move (300,0)])  
                    ))--(Array.toIndexedList (Array.fromList list))


