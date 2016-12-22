namespace Dungeon_Generator

module DataTypes = 
    open System
    
 //   type Class1() = 
 //       member this.X = "F#"
    
    (* CONSTANTS *)
    let WIDTH = 100
    let HEIGHT = WIDTH
    let PWIDTH = 800
    let PHEIGHT = PWIDTH
    let ROOMWIDTH = int (float WIDTH / 4.0)
    let ROOMHEIGHT = ROOMWIDTH
    let CELL = int (PWIDTH / WIDTH)
    let MINWIDTH = 4
    let MINHEIGHT = 4
    let NEIGHBORMAX = 4
    let maxRadius = 10
    let maxWidth = 10
    let maxHeight = 10
    let rand = new System.Random()
    let rand1 = fun x -> rand.Next(1, x)
    let halfOf x = x / 2
    
    (* DATA TYPES*)
    type Posn = 
        { X : int
          Y : int }
    
    type Terrain = 
        | Floor
        | Wall
    
    type Cell = 
        { Anchor : Posn
          Terrain : Terrain }
    
    type Room = 
        { Name : string
          Shape : Posn -> Boolean
          Anchor : Posn
          Neighbors : list<Room> } 
          (* either list<Room> or list<string>. The former directly implements the level structure and the second one has names as neighbors
             and requires you to pass and search the level separately. I'm not sure which one is more convenient. *)
    
    type Level = list<Room>
    
    let conjoin fs x = fs |> List.forall (fun f -> f (x))
        
module Shapes =
    open DataTypes
    type Line = (int -> int -> bool) -> (int -> int ) -> Posn -> bool
    
    (* A Line takes a comparison operator, a function, and a point to operate on.
   It applies the function to the point and uses the comparison operator to compare them. *)
    let line (oper : int -> int -> bool) (func : int -> int) (point : Posn) = 
        let x, y = point.X, point.Y
        oper y (func x)
    
    (* A Shape takes a list of lines and conjoins them into a single function that then takes
   a point and checks to see whether it satisfies all the line functions given. *)
    let shape lines point = ((conjoin lines) point)
    
    let rectangle (left : int) (top : int) (right : int) (bottom : int) = 
        shape [ (fun point -> (right > point.X) && (point.X > left))
                (fun point -> (bottom > point.Y) && (point.X > top)) ]
    
    let circle center rad =  // lots of type conversions, yikes.
        let h, k = center.X, center.Y
        shape [ (fun point -> int (float (point.X - h) ** 2.0 + float (point.Y - k) ** 2.0) <= int (float rad ** 2.0)) ]
   
    let randomRectangle() =
        let left = rand.Next(WIDTH - ROOMWIDTH)
        let right = rand.Next(left + MINWIDTH, left + ROOMWIDTH)
        let top = rand.Next(HEIGHT - ROOMHEIGHT)
        let bottom = rand.Next(top + MINHEIGHT, top + ROOMHEIGHT)
        rectangle left top right bottom

    let randomCircle() =
        let radians = rand.Next(ROOMWIDTH / 2)
        let center = { X = rand.Next(radians, WIDTH - radians); Y = rand.Next(radians, WIDTH - radians) }
        circle center radians

module Level =
    open Shapes
    open DataTypes

    let selectRoom name level =
        List.find (fun aRoom -> aRoom.Name = name) level
    
    let hasRoomp aRoom =
        List.length(aRoom.Neighbors) <= NEIGHBORMAX

    let randomName() = // needs an equivalent for gensym, which guarantees no collisions
        let charArray = [| for i in 0 .. 9 -> [|'A'..'Z'|].[rand.Next(26)] |]
        System.String.Concat(charArray)
    let randomShape() =
        let shapeChoices = [ randomCircle(); randomRectangle() ]
        shapeChoices.[rand.Next(List.length shapeChoices)]
    let randomRoom() = 
        { Name = randomName();
          Shape = randomShape();
          Anchor = { X = rand.Next(WIDTH); Y = rand.Next(HEIGHT) };
          Neighbors = [] }
    
    let makeRoom name =
        { Name = name; 
          Shape = randomShape();
          Anchor = { X = rand.Next(WIDTH); Y = rand.Next(HEIGHT) };
          Neighbors = [] }
    let addNewNeighbor (homeRoom: Room) (newRoom: Room)  =
        { homeRoom with Neighbors = newRoom :: homeRoom.Neighbors }
        // conses the entire new room on, but can just do the name
        // this needs to be fixed. Right now the size will grow very quickly because each
        // room might have a copy of all the level so far as its neighbors,
        // either make so that there's only one "Room" (really a level)
        // and the level continues through its neighbors

    let newRoom rooms =
        match rooms with
        | [] -> [ randomRoom() ]
        | aRoom :: tail when hasRoomp(aRoom) -> 
            let newRoom = makeRoom aRoom.Name
            newRoom :: addNewNeighbor aRoom newRoom :: tail