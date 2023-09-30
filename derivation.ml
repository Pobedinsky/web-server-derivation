open M_parser

let rec derive = function
| X               -> Var(1.)
| Pow (Var f, g) when f > 0. && f <> 1. -> Mult( Pow(Var f, g), Mult( Log(E, Var f), derive g))
| Pow (PI, g)     -> Mult( Pow(PI, g), Mult( Log(E, PI), derive g))
| Pow (E, g)      -> Mult( Pow(E, g), derive g)
| Pow (f, g) when g = E || g = PI -> Mult(g , Mult(Pow(f, Sum(g, Var(-1.))), derive f))
| Pow (f, Var(g)) -> Mult(Var g , Mult(Pow(f, Var(g-.1.)), derive f))
| Pow (f, g)      -> Sum( Mult(g, Mult(Pow(f, Sum(g, Var(-1.))), derive f)) , Mult( Pow(f,g), Mult(Log(E, f), derive(g))) )
| Mult (Var(f), g)-> Mult( Var f, derive g)
| Mult (f, g)     -> Sum( Mult( derive f, g), Mult(f, derive g ) )
| Div (f, g)      -> Div( Sum( Mult(derive f, g), Mult(Var(-1.),Mult(f, derive g) )), Pow(g, Var(2.)) )
| Sum (f, g)      -> Sum( derive f, derive g )
| Var _  | E | PI -> Var(0.)
| Log (Var f, g)  -> Mult( Div( derive g, g ), Log(Var f, E) )
| Log (PI, g)     -> Mult( Div( derive g, g ), Log(PI, E) )
| Log (E, g)      -> Div( derive g, g )
| _ -> failwith "Not done yet";;

let rec simplify = function
| Mult (f, g) when f = Var(1.) || g = Var(1.) -> if f = Var(1.) then simplify g else simplify f
| Mult (f, g) when f = Var(0.) || g = Var(0.) -> Var(0.)
| Sum (f, g) when f = Var(0.) || g = Var(0.) -> if f = Var(0.) then simplify g else simplify f
| Pow (f, Var(1.)) -> simplify f
| Pow (f, Var(0.)) -> Var(1.)
| Mult (f, g) -> Mult(simplify f, simplify g)
| Sum (f, g) -> Sum(simplify f, simplify g)
| Div (f, Var(1.)) -> simplify f
| Div (f, g) -> Div(simplify f, simplify g)
| Pow (f, g) -> Pow(simplify f, simplify g)
| Log (f, g) -> Log(f, simplify g)
| k -> k;;


let input = parse "stdin"
let testing = match input with | Some a -> a | None -> Var(0.)

let () = testing |> derive |> simplify |> string_of_function |> print_string;;
