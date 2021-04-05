module Utils
open System 
open System.IO
open System.Reflection

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let readAll (filePath:string) =  
    use sr = new StreamReader (filePath)
    sr.ReadToEnd()

let split (c:char) (s:string) = s.Split(c)

let fst3 (a,_,_) = a
let snd3 (_,a,_) = a
let trd3 (_,_,a) = a

type vector3 = 
    val mutable x:int
    val mutable y:int
    val mutable z:int
    new (x, y, z) = { x = x; y = y; z = z }
    member this.add (v:vector3) = 
        this.x <- this.x + v.x 
        this.y <- this.y + v.y 
        this.z <- this.z + v.z 

type vector2 = 
    val mutable x:int
    val mutable y:int
    new (x, y) = { x = x; y = y }
    member this.add (v:vector2) = 
        this.x <- this.x + v.x 
        this.y <- this.y + v.y
    member this.toTuple = this.x, this.y

type 'a Queue (front:'a List, rear: 'a List)  = 
    let frontToBack = 
        match front, rear with 
            |[], rear -> (List.rev rear, []) 
            |x -> (x) 
    static member Empty = Queue(List.Empty, List.Empty) 
    member this.Add value = Queue(front, value :: rear)
    member this.TryHead = 
        match frontToBack with
            | [], _ -> None
            | a :: tail, _ -> Some(a)
    member this.Head = 
        match frontToBack with
            | [], _ -> failwith "Empty or not reversed"
            | a :: tail, _ -> a

    member this.Tail = 
        match frontToBack with
            |[], _ -> failwith "Empty"
            |a :: tail, r -> Queue(tail, r)
    member this.Length = front.Length + rear.Length
    member this.IsEmpty = this.Length = 0
    member this.ToSeq = (front @ List.rev rear) |> List.toSeq

    member this.enque value = Queue(front, value :: rear)
    member this.dequeue = 
        match frontToBack with
        | [], _ -> failwith "Empty queue!"
        | a :: tail, r -> a, Queue(tail, r)

