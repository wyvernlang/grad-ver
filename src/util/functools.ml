
let rightProd x y = (x,y)
let leftProd x y = (y,x)

let rightProdf x f = (x,f x)
let leftProdf x f = (f x,x)

let fst (x,y) = x
let snd (x,y) = y
let swap (x,y) = (y,x)

let (@@) f x = f x
let (@<) f g x = f (g x)

let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

let rec fix ~eq ~f x =
  let result = f x in
  if eq result x then result else fix ~eq:eq ~f:f result

let flip f x y = f y x

let first f (x,y) = (f x, y)
let second f (x,y) = (x, f y)

