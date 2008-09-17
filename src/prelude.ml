(*
Prelude.ml: OCaml utility functions

Copyright (C) 2008  Ilmari Heikkinen <ilmari.heikkinen@gmail.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*)

(*
  #use "topfind";;
  #require "pcre";;
  #require "unix";;
  #require "netstring";;
  #use "prelude.ml";;
  interact @@ uppercase @. join "-" @. words;;
*)

include Printf
include List


(* Function combinators *)

let (@@) f x = f x
(**T
  map (multiply 2) @@ reverse (1--3) = [6; 4; 2]
  int @@ ceil @@ sqrt @@ float 144 = 12
**)
let (@.) f g x = f (g x)
(**T
  map (join "-" @. words) ["a b"; "b c"] = ["a-b"; "b-c"]
  (int @. ceil @. sqrt @. float) 144 = 12
**)
let (@..) f g x y = f (g x y)
(**T
  (reverse @.. zip) (1--3) (4--6) = [(3, 6); (2, 5); (1, 4)]
**)
let (@...) f g x y z = f (g x y z)
(**T
  (rev @... unfoldr) (lessOrEqualTo 3) (fupler succ) 1 = [1; 2; 3]
**)
let (@....) f g w x y z = f (g w x y z)
(**T
  (rev @.... unfoldrFilter) (lte 7) even (fupler succ) 1 = [2; 4; 6]
**)
let (|>) x f = f x
(**T
  (1--3) |> reverse |> map (multiply 2) = [6; 4; 2]
  144 |> float |> sqrt = 12.0
**)
let (|>.) f g x = g (f x)
(**T
  map (words |>. join "-") ["a b"; "b c"] = ["a-b"; "b-c"]
  map (float |>. sqrt |>. ceil |>. int) [1; 4; 9; 15] = [1; 2; 3; 4]
**)
let uncurry f (a, b) = f a b
(**T
  uncurry (+) (1,2) = 3
  uncurry (@) @@ unzip @@ map (fun x -> (x, x+3)) (1--3) = (1--6)
**)
let curry f a b = f (a, b)
(**T
  curry reverseTuple 1 2 = (2,1)
**)
let flip f a b = f b a
(**T
  (flip map (65--68)) chr = ['A'; 'B'; 'C'; 'D']
**)
let dup f x = f x x
(**T
  map (dup multiply) (1--3) = [1; 4; 9]
**)
let id x = x
(**T
  maybe 0 id (Some 1) = 1
**)
let const x y = x
(**T
  map (const 1) ['a'; 'b'; 'c'] = [1; 1; 1]
**)


(* Ifs *)

let ifP p x y n = if p n then x else y
let ifB b x y = if b then x else y


(* Tuple operations *)

let tuple a b = (a,b)
let tuple3 a b c = (a,b,c)
let reverseTuple (a,b) = (b,a)
let trev = reverseTuple
let fuple f g a = (f a, g a)
let fuplel f a = (f a, a)
let fupler f a = (a, f a)


(* Option combinators *)

let some x = Some x
let none x = None

let opt_or o y = match o with Some x -> x | None -> y
(**T
  opt_or None 0 = 0
  opt_or (Some 10) 0 = 10
**)
let (|?) = opt_or
let optOr y o = match o with Some x -> x | None -> y
(**T
  optOr 0 None = 0
  optOr 0 (Some 10) = 10
**)

let maybe v f o = match o with Some x -> f x | None -> v
(**T
  maybe 0 parseInt (Some "10") = 10
  maybe 0 parseInt None = 0
**)
let unmaybe b f v = if b = v then None else Some (f v)
(**T
  unmaybe 0 showInt 10 = Some "10"
  unmaybe 0 showInt 0 = None
**)
let optIf p f v = if p v then Some (f v) else None
(**T
  optIf (greaterThan 0) (add 5) 0 = None
  optIf (greaterThan 0) (add 5) 1 = Some 6
  unfoldrOpt (optIf (greaterThan 0) (fun x -> (x, x-1))) 5 = [1;2;3;4;5]
**)


(* Exception handling combinators *)

let maybeE v f o = try f o with _ -> v
(**T
  maybeE 0 last [] = 0
  maybeE 0 last [1] = 1
**)
let maybeEx ex v f o = try f o with e when ex = e -> v
(**T
  maybeEx Not_found 0 last [] = 0
  maybeEx Not_found 0 last [1] = 1
  (try maybeEx Not_found 0 raise Exit with Exit -> 1) = 1
**)
let maybeExl exl v f o =
  try f o with x -> if exists ((=) x) exl then v else raise x
(**T
  maybeExl [Not_found] 0 last [] = 0
  maybeExl [Not_found] 0 last [1] = 1
**)
let maybeEOF v f o = maybeEx End_of_file v f o
(*
  unfoldlOpt (maybeEOF None (fun ic -> Some (readInt ic, ic)) ic) stdin;;
*)
let maybeNF v f o = maybeEx Not_found v f o
(**T
  maybeNF 0 last [] = 0
  maybeNF 0 last [1;2;3] = 3
**)


(* Exceptions to options *)

let optE f o = maybeE None (some @. f) o
let optEx ex f o = maybeEx ex None (some @. f) o
let optExl exl f o = maybeExl exl None (some @. f) o
let optEOF f o = maybeEOF None (some @. f) o
let optNF f o = maybeEx Not_found None (some @. f) o
(**T
  optNF last [] = None
  optNF last [1;2;3] = Some 3
**)

let rec loop f x = f x; loop f x
(*
  loop (print_endline @. input_line) stdin;;
*)


(* Comparisons *)

let lessThan x y = (<) y x
(**T
  filter (lessThan 3) (1--10) = [1; 2]
**)
let lessOrEqualTo x y = (<=) y x
(**T
  filter (lessOrEqualTo 3) (1--10) = [1; 2; 3]
**)
let greaterThan x y = (>) y x
(**T
  filter (greaterThan 7) (1--10) = [8; 9; 10]
**)
let greaterOrEqualTo x y = (>=) y x
(**T
  filter (greaterOrEqualTo 7) (1--10) = [7; 8; 9; 10]
**)

let eq = (=)
let neq = (<>)
let equals = (=)
(**T
  filter (modulo 2 |>. equals 0) (1--10) = [2; 4; 6; 8; 10]
**)

let lt = lessThan
let lte = lessOrEqualTo
let gt = greaterThan
let gte = greaterOrEqualTo

let between a b x = x >= a && x <= b


(* Conversions *)

let array = Array.of_list
let list = Array.to_list
let int = int_of_float
let char = char_of_int
let parseInt = int_of_string
let parseFloat = float_of_string
let showInt = string_of_int
let showFloat f =
  Pcre.replace ~rex:(Pcre.regexp "\\.$") ~templ:".0" (string_of_float f)
let charCode = int_of_char
let ord = int_of_char
let chr = char_of_int


(* Float operations *)

let round f = truncate (if f > 0.0 then f +. 0.5 else f -. 0.5)
(**T
  round 0.5 = 1
  round 0.4 = 0
  round (-0.4) = 0
  round (-0.5) = -1
**)
let ceiling = ceil
let quot f i = (truncate f) / i
(**T
  quot 5.0 2 = 2
**)
let recip f = 1. /. f
let signumf f = if f > 0. then 1. else if f < 0. then (-.1.) else 0.
let logBase base f = log f /. log base
let root rt f = f ** (recip rt)
let absf f = (signumf f) *. f
let pi = 4. *. atan 1.
let addf = (+.)
let subtractf a b = b -. a
let multiplyf = ( *. )
let dividef a b = b /. a
let negatef v = (-.v)
let average2f a b = (a +. b) /. 2.0


(* Integer operations *)

let average2 a b = (a + b) / 2
let quot_rem a b =
  let q = a / b in
  (q, a - (q*b))
let rem a b = a mod b
let even x = x mod 2 == 0
let odd x = x mod 2 == 1
let signum i = if i > 0 then 1 else if i < 0 then (-1) else 0
let succ x = x + 1
let pred x = x - 1
let add = (+)
let subtract a b = b - a
(**T
  map (subtract 10) (11--13) = [1; 2; 3]
**)
let multiply = ( * )
let divide a b = b / a
(**T
  map (divide 10) [10; 20; 30] = [1; 2; 3]
**)
let modulo a b = b mod a
(**T
  filter (modulo 2 |>. equals 0) (1--10) = [2; 4; 6; 8; 10]
**)
let negate v = (-v)

let rec gcd x y = match (abs x), (abs y) with
  | 0,0 -> invalid_arg "Prelude.gcd: gcd 0 0 is undefined"
  | x,0 -> x
  | x,y -> gcd y (rem x y)

let lcm x y = match x, y with
  | _,0 | 0,_ -> 0
  | x,y -> abs ((x / (gcd x y)) * y)


(* List operations *)

let reverse = rev

let nth i l = List.nth l i
let ($$) = List.nth

let cons x xs = x::xs
let head = function [] -> raise Not_found | (h::_) -> h
let tail = function [] -> raise Not_found | (_::t) -> t
let pop l =
  let rec aux l res =
    match l with
      | [] -> raise Not_found
      | (h::[]) -> (rev res, h)
      | (h::t) -> aux t (h :: res) in
  aux l []
(**T
  pop [1;2;3] = ([1;2], 3)
**)
let popped l = fst (pop l)
(**T
  popped [1; 2; 3] = [1; 2]
**)
let last l = snd (pop l)
(**T
  last [1; 2; 3] = 3
**)
let first = head

let map f l = rev (rev_map f l)

let rec assocBy f l =
  match l with
    | [] -> raise Not_found
    | (k,v)::t when f k -> v
    | _::t -> assocBy f t

let lookup e l = optNF (assoc e) l
let lookupBy f e l = optNF (assocBy f e) l

let len = length

let all = for_all
let any = exists

let allEqual l = match l with
  | [] -> true
  | (h::t) -> all ((=) h) t

let includes x = exists (fun y -> x = y)
let has = includes
let elem = includes
let notElem x lst = not @@ elem x lst

let indexOf x lst =
  let rec aux x c l = match l with
    | [] -> raise Not_found
    | (h::t) when x = h -> c
    | (h::t) -> aux x (c+1) t in
  aux x 0 lst
(**T
  indexOf 'a' (explode "foobar") = 4
**)
let findIndex p lst =
  let rec aux p c l = match l with
    | [] -> raise Not_found
    | (h::t) -> if p h then c else aux p (c+1) t in
  aux p 0 lst
(**T
  findIndex (gt 4) (0--9) = 5
**)
let findWithIndex p lst =
  let rec aux p c l = match l with
    | [] -> raise Not_found
    | (h::t) -> if p h then (h,c) else aux p (c+1) t in
  aux p 0 lst
(**T
  findWithIndex (gt 4) (2--9) = (5,3)
**)
let rec recurseOpt f i = match f i with None -> i | Some x -> recurseOpt f x
let rec recurseWhile p f i = if p i then recurseWhile p f (f i) else i
let rec recurseUntil p f i = if p i then i else recurseUntil p f (f i)
let rec recurseTo n f i = if n = i then i else recurseTo n f (f i)
let rec recurseN f n i = if n <= 0 then i else recurseN f (n-1) (f i)

let null = function [] -> true | _ -> false

let concatMap f l = concat (map f l)
(**T
  concatMap ((--) 1) [1;2;3] = [1; 1; 2; 1; 2; 3]
**)

let pick indices l = map (flip nth l) indices
(**T
  pick [2; 3] (explode "foobar") = ['o'; 'b']
**)
let pickWith funcs l = map ((|>) l) funcs
(**T
  pickWith [first; last] (explode "foobar") = ['f'; 'r']
**)
let pickArray indices l = map (Array.get l) indices
let pickArrayWith funcs l = map ((|>) l) funcs

let span f lst =
  let rec aux f res l = match l with
    | (h::t) when f h -> aux f (h::res) t
    | x -> (rev res, x) in
  aux f [] lst
(**T
  span id [true; false; false; true] = ([true], [false; false; true])
  span (lessOrEqualTo 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
**)
let break p = span (not @. p)
(**T
  break id [false; false; true; false] = ([false; false], [true; false])
  break (greaterThan 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
**)

let takeWhile f lst = fst @@ span f lst
let take n lst =
  let rec aux c res l = match c, l with
      | x, (h::t) when x > 0 -> aux (c-1) (h::res) t
      | _ -> rev res in
  aux n [] lst

let rec dropWhile f lst = match lst with
  | (h::t) when f h -> dropWhile f t
  | _ -> lst
let rec drop n lst = match n, lst with
  | x, (h::t) when x > 0 -> drop (n-1) t
  | _ -> lst

let rec dropWhile2 f a b = match a,b with
  | (x::xs), (y::ys) when f x y -> dropWhile2 f xs ys
  | _ -> a,b
let rec drop2 n a b = match n,a,b with
  | c, (x::xs), (y::ys) when c > 0 -> drop2 c xs ys
  | _ -> a,b

let splitAt n xs = (take n xs, drop n xs)
(**T
  splitAt 3 (explode "foobar") = (['f'; 'o'; 'o'], ['b'; 'a'; 'r'])
**)

let sub first len lst =
  let rec f l fst ln c res = match l with
    | [] -> res
    | h::t when c >= (fst + ln) -> res
    | h::t when c >= fst -> f t fst ln (c+1) (h::res)
    | h::t -> f t fst ln (c+1) res in
  let first = if first < 0 then length lst + first else first in
  List.rev (f lst first len 0 [])
(**T
  sub 2 3 (explode "foobar") = ['o'; 'b'; 'a']
  sub (-3) 2 (explode "foobar") = ['b'; 'a']
**)
let slice first last lst =
  let len = if first < 0 || last < 0 then length lst else 0 in
  let first = if first < 0 then len + first else first in
  let last = if last < 0 then len + last else last in
  sub first (last-first+1) lst
(**T
  slice 2 3 (explode "foobar") = ['o'; 'b']
  slice (-3) (-1) (explode "foobar") = ['b'; 'a'; 'r']
**)

let interlace elem l =
  let rec aux l l2 = match l with
      | [] -> (match l2 with [] -> [] | (h::t) -> List.rev t)
      | (h::t) -> aux t (elem :: h :: l2) in
  aux l []
(**T
  interlace 0 [1; 2; 3] = [1; 0; 2; 0; 3]
  implode @@ interlace '-' @@ explode "abcde" = "a-b-c-d-e"
**)

let compact l = map (function Some x -> x | _ -> failwith "compact")
                    (filter ((!=) None) l)
(**T
  compact [None; Some 10; Some 5; None; None; Some 8] = [10; 5; 8]
  compact @@ map (optIf (greaterThan 0) id) (-3--3) = [1; 2; 3]
**)

let squeeze l =
  let rec aux x l1 l2 = match l1 with
    | [] -> (rev l2)
    | (h::t) when h = x -> aux x t l2
    | (h::t) -> aux h t (h::l2)
  in
  match l with [] -> [] | (h::t) -> aux h t [h]
(**T
  squeeze [1;2;2;2;3;3;1] = [1; 2; 3; 1]
  squeeze @@ sort [1;2;2;2;3;3;1] = [1; 2; 3]
**)

let sort ?(cmp=compare) l = List.sort cmp l
let sortBy ?(cmp=compare) f l =
  map (fupler f) l |> sort ~cmp:(fun (_,a) (_,b) -> cmp a b) |> map fst
let uniq ?cmp l = squeeze (sort ?cmp l)
(**T
  uniq [3;1;2;2;2;3;3;1] = [1; 2; 3]
**)

let reject f l = filter (not @. f) l
(**T
  reject (gt 4) (1--5) = (1--4)
**)

let without x l = filter ((<>) x) l
(**T
  without 4 [1; 2; 4; 1; 2; 4] = [1; 2; 1; 2]
**)

let rec neighbours item items = match items with
  | (p::i::n::t) when i == item -> (Some p, Some n)
  | (i::n::t) when i == item -> (None, Some n)
  | (p::i::[]) when i == item -> (Some p, None)
  | (h::t) -> neighbours item t
  | [] -> (None, None)
(**T
  neighbours 2 (1--10) = (Some 1, Some 3)
  neighbours 10 (1--10) = (Some 9, None)
  neighbours 1 (1--10) = (None, Some 2)
  neighbours 0 (1--10) = (None, None)
  neighbours 11 (1--10) = (None, None)
**)

let neighbourLists item n items =
  let rec aux prev lst =
    match lst with
      | [] -> ([], [])
      | (i::[]) when i = item -> (prev, [])
      | (i::t) when i = item -> (prev, take n t)
      | (h::t) -> aux (take n (h::prev)) t
  in
  aux [] items
(**T
  neighbourLists 5 2 (1--10) = ([4; 3], [6; 7])
  neighbourLists 7 3 (1--10) = ([6; 5; 4], [8; 9; 10])
  neighbourLists 2 5 (1--10) = ([1], [3; 4; 5; 6; 7])
  neighbourLists 9 3 (1--10) = ([8; 7; 6], [10])
  neighbourLists 0 4 (1--10) = ([], [])
**)

let mapWindow f n l =
  let rec aux f wnd lst res =
    match lst with
      | [] -> rev res
      | (h::t) ->
        let wnd = tail wnd @ [h] in
        aux f wnd t ((f wnd) :: res) in
  let wnd, t = splitAt n l in
  aux f wnd t [f wnd]
(**T
  mapWindow sum 1 (1--4) = (1--4)
  mapWindow sum 2 (1--4) = [3; 5; 7]
  mapWindow sum 3 (1--4) = [6; 9]
**)

let foldl = fold_left
(**T
  foldl (+) 0 (1--10) = 55
  foldl (fun s b -> s ^ (string_of_int b)) "--" (1--3) = "--123"
**)
let foldl1 f l = foldl f (head l) (tail l)
(**T
  foldl1 (+) (1--10) = 55
  foldl1 (fun s i -> s ^ i) ["foo"; "bar"; "baz"] = "foobarbaz"
**)

let foldr f s l = fold_right f l s
(**T
  foldr (+) 0 (1--10) = 55
  foldr (fun a s -> s ^ (string_of_int a)) "--" (1--3) = "--321"
**)
let foldr1 f l = let l,i = pop l in foldr f i l
(**T
  foldr1 (+) (1--10) = 55
  foldr1 (fun a s -> s ^ a) ["foo"; "bar"; "baz"] = "bazbarfoo"
**)

let scanl f init lst = rev @@ snd @@
  foldl (fun (s,l) i -> let s' = f s i in (s', s'::l)) (init, [init]) lst
(**T
  scanl multiply 1 (2--5) = [1; 2; 6; 24; 120]
**)
let scanl1 f l = scanl f (head l) (tail l)
(**T
  scanl1 multiply (1--5) = [1; 2; 6; 24; 120]
**)

let scanr f init lst = snd @@
  foldr (fun i (s,l) -> let s' = f s i in (s', s'::l)) (init, [init]) lst
(**T
  scanr multiply 1 @@ [5;4;3;2] = [120; 24; 6; 2; 1]
**)
let scanr1 f l = let l,i = pop l in scanr f i l
(**T
  scanr1 multiply @@ [5;4;3;2;1] = [120; 24; 6; 2; 1]
**)

let unfoldrOpt f init =
  let rec aux f v l =
    match f v with
      | None -> l
      | Some (a, b) -> aux f b (a::l) in
  aux f init []
(**T
  unfoldrOpt (fun x -> if x > 3 then None else Some (x, x+1)) 1 = [3; 2; 1]
  unfoldrOpt (fun i -> if i > 67 then None else Some (char i, i+1)) 65 = ['C';'B';'A']
**)
let unfoldlOpt f init = rev (unfoldrOpt f init)
(**T
  unfoldlOpt (fun x -> if x > 3 then None else Some (x, x+1)) 1 = [1; 2; 3]
  unfoldlOpt (fun i -> if i > 67 then None else Some (char i, i+1)) 65 = ['A';'B';'C']
**)

let unfoldr p f init = unfoldrOpt (optIf p f) init
(**T
  unfoldr (lessThan 4) (fupler succ) 1 = [3; 2; 1]
  unfoldr (lessThan 68) (fuple char succ) 65 = ['C'; 'B'; 'A']
**)
let unfoldl p f init = rev (unfoldr p f init)
(**T
  unfoldl (lessThan 4) (fupler succ) 1 = [1; 2; 3]
  unfoldl (lessThan 68) (fuple char succ) 65 = ['A'; 'B'; 'C']
**)
let unfoldrWhile = unfoldr
let unfoldlWhile = unfoldl

let unfoldrUntil p f init = unfoldr (not @. p) f init
let unfoldlUntil p f init = unfoldl (not @. p) f init

let unfoldrFilter p s f init =
  let rec aux p f v l =
    if not (p v) then l
    else let a,b = f v in
         aux p f b (if s v then (a::l) else l) in
  aux p f init []
(**T
  unfoldrFilter (lt 7) even (fupler succ) 1 = [6; 4; 2]
  unfoldrFilter (lt 7) even (fuple (divide 2) succ) 2 = [3; 2; 1]
**)
let unfoldlFilter p s f init = rev @@ unfoldrFilter p s f init
(**T
  unfoldlFilter (lt 7) even (fupler succ) 1 = [2; 4; 6]
  unfoldlFilter (lt 7) even (fuple (divide 2) succ) 2 = [1; 2; 3]
**)

let unfoldlN f n i =
  unfoldlWhile (snd |>. gt 0) (fun (s,c) -> (f s, (s, pred c))) (i, n)

let forN f n = for i=0 to (n-1) do f i done

let generateOpt f init =
  unfoldlOpt (fun x -> match f x with Some a -> Some (x,a) | None -> None) init
(**T
  generateOpt (fun x -> if x > 3 then None else Some (x+1)) 1 = [1; 2; 3]
**)
let generate p f init = unfoldl p (fupler f) init
(**T
  generate (lessOrEqualTo 3) succ 1 = [1; 2; 3]
**)
let generateUntil p f init = generate (not @. p) f init

let generateOptR f init =
  unfoldrOpt (fun x -> match f x with Some a -> Some (x,a) | None -> None) init
(**T
  generateOptR (fun x -> if x > 3 then None else Some (x+1)) 1 = [3; 2; 1]
**)
let generateR p f init = unfoldr p (fupler f) init
(**T
  generateR (lte 3) succ 1 = [3; 2; 1]
**)
let generateUntilR p f init = generateR (not @. p) f init

let zipWith f a b =
  let rec aux f a b l = match a,b with
      | (x::xs), (y::ys) -> aux f xs ys ((f x y)::l)
      | _ -> l in
  rev @@ aux f a b []
let zip a b = zipWith tuple a b
let unzip = split

let rec zipWith3 f a b c = match a,b,c with
  | (h1::t1), (h2::t2), (h3::t3) -> (f h1 h2 h3) :: (zipWith3 f t1 t2 t3)
  | _ -> []
let zip3 a b c = zipWith3 tuple3 a b c
let unzip3 l =
  foldr (fun (a,b,c) (t1,t2,t3) -> (a::t1, b::t2, c::t3)) ([],[],[]) l

let iterWithIndex f l = ignore (foldl (fun j i -> f i j; j+1) 0 l)
let each = iter
let eachWithIndex = iterWithIndex
let mapWithIndex f l =
  rev (snd (foldl (fun (j,r) i -> (j+1, (f i j)::r)) (0, []) l))

let diffSorted a b =
  let rec aux a b l =
    match b with
      | [] -> (rev l) @ a
      | (x::xs) -> begin
        match a with
          | [] -> rev l
          | (y::ys) ->
            if y = x then aux ys xs l
            else if y > x then aux a xs l
            else aux ys b (y::l)
      end in
  aux a b []
(**T
  diffSorted (1--10) (5--15) = [1; 2; 3; 4]
  diffSorted (5--15) (1--10) = [11; 12; 13; 14; 15]
  diffSorted [3;2;1] [1;0] = [3; 2; 1]
**)

let diff a b =
  let rec aux a b l =
    match b with
      | [] -> (rev l) @ a
      | (x::xs) -> begin
        match a with
          | [] -> rev l
          | ((y,i)::ys) ->
            if y = x then aux ys xs l
            else if y > x then aux a xs l
            else aux ys b ((y,i)::l)
      end in
  let diffs =
    aux (List.sort (fun (y,_) (y',_) -> compare y y') (mapWithIndex tuple a))
        (sort b) [] in
  map fst (List.sort (fun (_,i) (_,i') -> compare i i') diffs)
(**T
  diff (1--10) (5--15) = [1; 2; 3; 4]
  diff (5--15) (1--10) = [11; 12; 13; 14; 15]
  diff [3;2;1] [1;0] = [3; 2]
**)

let product lst = foldl ( * ) 1 lst
let productf lst = foldl ( *. ) 1. lst
let sum lst = foldl (+) 0 lst
let sumf lst = foldl (+.) 0. lst
let average lst = (sum lst) / (length lst)
let averagef lst = (sumf lst) /. (float (length lst))

let cycle n l =
  let rec aux c lst res =
    if c == 0 then res
    else match lst with
           | [] -> aux c l res
           | (h::t) -> aux (c-1) t (h::res) in
  match l with
    | [] -> invalid_arg "cycle"
    | _ -> reverse @@ aux n l []
(**T
  cycle 5 (1--3) = [1; 2; 3; 1; 2]
  cycle 3 (1--10) = [1; 2; 3]
**)

let range s e =
  if s <= e
  then generateR (greaterOrEqualTo s) pred e
  else generateR (lessOrEqualTo s) succ e
(**T
  range 1 3 = [1; 2; 3]
  range 1 1 = [1]
  range 1 0 = [1; 0]
**)
let generateN f n =
  let rec aux f n res =
    if n < 0 then res
    else aux f (n-1) ((f n) :: res) in
  aux f (n-1) []
let init = generateN
(**T
  init succ 10 = (1--10)
  init pred 10 = ((-1)--8)
**)
let step d s e =
  if d == 0 then failwith "Prelude.step: zero step" else
  if s == e then [s] else
  if s < e
  then (if d < 0 then [] else generate (lte e) (add d) s)
  else (if d > 0 then [] else generate (gte e) (add d) s)
(**T
  step 2 0 5 = [0; 2; 4]
  step 2 1 5 = [1; 3; 5]
  step (-2) 5 1 = [5; 3; 1]
  step (-2) 5 0 = [5; 3; 1]
**)
let (--) = range
(**T
  (1--3) = [1; 2; 3]
  (1--1) = [1]
  (1--0) = [1; 0]
**)

let replicate n v = init (const v) n
(**T
  replicate 5 '-' = ['-'; '-'; '-'; '-'; '-']
  replicate 0 '-' = []
  replicate (-1) '-' = []
**)
let repeat v n = init (const v) n
(**T
  repeat '-' 5 = ['-'; '-'; '-'; '-'; '-']
  repeat '-' 0 = []
  repeat '-' (-1) = []
**)
let times n l = concat @@ replicate n l
(**T
  times 3 [1; 2; 3] = [1; 2; 3; 1; 2; 3; 1; 2; 3]
**)
let (@*) l n = times n l
(**T
  [1; 2; 3] @* 3 = [1; 2; 3; 1; 2; 3; 1; 2; 3]
**)
let (^*) s n = String.concat "" (repeat s n)
(**T
  "foo" ^* 3 = "foofoofoo"
**)
let (@|) a1 a2 = Array.concat [a1; a2]
(**T
  (1--|3) @| (4--|6) = (1--|6)
**)
let (@|*) a n = Array.concat (repeat a n)
(**
  (1--|2) @|* 3 = [|1;2;1;2;1;2|]
**)

let explode s =
  unfoldr (greaterOrEqualTo 0) (fun i -> (String.unsafe_get s i, i-1))
  (String.length s - 1)
(**T
  explode "foo" = ['f'; 'o'; 'o']
**)
let implode lst =
  fst @@ foldl (fun (s,c) i -> String.unsafe_set s c i; (s, c+1))
         (String.create @@ length lst, 0) lst
(**T
  implode @@ explode "foo" = "foo"
**)

let iterate f n s = scanl (fun s i -> f s) s (2--n)
(**T
  iterate succ 10 1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  iterate pred 4 1 = [1; 0; -1; -2]
**)
let maximum lst = foldl1 max lst
(**T
  maximum [1;2;3;0;1;4;3;1] = 4
**)
let maxBy f a b = if (f a) >= (f b) then a else b
let maximumBy f lst = foldl1 (maxBy f) lst
let maximumByWith f lst = maximumBy snd (map (fupler f) lst)
let minimum lst = foldl1 min lst
(**T
  minimum [1;2;3;0;1;4;3;1] = 0
**)
let minBy f a b = if (f a) <= (f b) then a else b
let minimumBy f lst = foldl1 (minBy f) lst
let minimumByWith f lst = minimumBy snd (map (fupler f) lst)

let groupsOf n l = if n <= 1 then [l]
  else unfoldlUntil null (splitAt n) l
(**T
  groupsOf 3 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
**)
let splitInto n l = if n <= 1 then [l]
  else groupsOf (int (ceil (float (len l) /. float n))) l
(**T
  splitInto 4 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
**)
let groupBy p l =
  let rec aux p v l rl res = match l with
    | [] -> (rev rl) :: res
    | (h::t) when p v h -> aux p v t (h::rl) res
    | (h::t) -> aux p h t [h] ((rev rl) :: res) in
  match l with [] -> []
    | (h::t) -> rev (aux p h t [h] [])
(**T
  groupBy (fun x y -> x*x = y*y) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
**)
let groupAs f l =
  let rec aux f v l rl res = match l with
    | [] -> (rev rl) :: res
    | (h::t) when (f h) = v -> aux f v t (h::rl) res
    | (h::t) -> aux f (f h) t [h] ((rev rl) :: res) in
  match l with [] -> []
    | (h::t) -> rev @@ aux f (f h) t [h] []
(**T
  groupAs (fun x -> x*x) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
**)
let group l = groupAs id l
(**T
  group [1;1;2;2;3;1] = [[1;1]; [2;2]; [3]; [1]]
**)
let count p l =
  let rec aux c p l = match l with
    | [] -> c
    | (h::t) -> aux (c + (if p h then 1 else 0)) p t in
  aux 0 p l
(**T
  count (gt 5) (0--10) = 5
**)
let rotate n l =
  let len = length l in
  let n = (-n) mod len in
  let n = if n >= 0 then n else len + n in
  uncurry (@) (reverseTuple (splitAt n l))
(**T
  rotate 1 [1;2;3] = [3;1;2]
  rotate 2 [1;2;3] = [2;3;1]
  rotate 3 [1;2;3] = [1;2;3]
  rotate (-1) [1;2;3] = [2;3;1]
  rotate (-2) [1;2;3] = [3;1;2]
  rotate (-3) [1;2;3] = [1;2;3]
**)


(* Array operations *)

let auget = Array.unsafe_get
let auset = Array.unsafe_set
let amake = Array.make
let acreate = Array.create
let alen = Array.length
(**T
  alen (1--|10) = 10
**)
let amap = Array.map
(**T
  amap succ (1--|10) = (2--|11)
**)
let aiter = Array.iter
let aconcat = Array.concat
let ainit f l = Array.init l f
(**T
  ainit succ 10 = (1--|10)
**)
let areverse a =
  let l = alen a in
  ainit (fun i -> auget a (l-1-i)) l
let arev = areverse
(**T
  arev (1--|10) = (10--|1)
**)
let afoldl = Array.fold_left
let afoldl1 f a =
  let rec aux f i acc len a =
    if i >= len then acc
    else aux f (i+1) (f acc (auget a i)) len a in
  let len = alen a in
  if len < 1 then raise Not_found;
  aux f 1 (auget a 0) len a
(**T
  afoldl1 (^) [|"foo"; "bar"; "baz"|] = "foobarbaz"
  afoldl1 dividef [|5.; 6.; 7.|] = dividef (dividef 5. 6.) 7.
**)

let afoldr f a i = Array.fold_right f i a
let afoldr1 f a =
  let rec aux f i acc a =
    if i < 0 then acc
    else aux f (i-1) (f (auget a i) acc) a in
  let len = alen a in
  if len < 1 then raise Not_found;
  aux f (len-2) (auget a (len-1)) a
(**T
  afoldr1 (^) [|"foo"; "bar"; "baz"|] = "foobarbaz"
  afoldr1 dividef [|5.; 6.; 7.|] = dividef 5. (dividef 6. 7.)
**)

let afilter f a = array (afoldr (fun i res -> if f i then i::res else res) [] a)
(**T
  afilter (gt 3) (1--|5) = (4--|5)
  afilter (lt 3) (1--|5) = (1--|2)
**)

let asum a = afoldl1 (+) a
(**T
  asum [|1; 2; 3|] = 6
**)
let asumf a = afoldl1 (+.) a
(**T
  asumf [|1.; 2.; 3.|] = 6.
**)
let aproduct a = afoldl1 ( * ) a
let aproductf a = afoldl1 ( *. ) a
let aaverage a = asum a / alen a
(**T
  aaverage (1--|10) = 5
**)
let aaveragef a = asumf a /. float (alen a)
(**T
  aaveragef (amap float (1--|10)) = 5.5
**)

let arange s e =
  if s > e
  then ainit ((-) s) (s-e+1)
  else ainit ((+) s) (e-s+1)
let (--|) = arange
(**T
  (1--|3) = [|1;2;3|]
  (3--|1) = [|3;2;1|]
  (1--|1) = [|1|]
**)
let aZipWith f a b =
  let len = min (alen a) (alen b) in
  ainit (fun i ->
    f (Array.unsafe_get a i) (Array.unsafe_get b i)
  ) len
let amap2 = aZipWith
(**T
  aZipWith (fun x y -> x / y) (1--|3) (4--|2) = [|0; 0; 1;|]
  amap2 (+) (1--|10) (10--|1) = amake 10 11
**)
let arrayIndex i s = if i < 0 then (alen s) + i else i
let aslice i j s =
  let i = arrayIndex i s
  and j = arrayIndex j s + (if j < 0 then 1 else 0) in
  let len = j - i in
  Array.sub s i len
(**T
  aslice 0 2 (1--|6) = (1--|2)
  aslice 1 (-1) (1--|6) = (2--|6)
  aslice 1 (-2) (1--|6) = (2--|5)
  aslice (-3) (-2) (1--|6) = [|4; 5|]
  aslice (-3) (-3) (1--|6) = [|4|]
  aslice (-3) (-4) (1--|6) = [||]
  maybeE true (fun v -> ignore (aslice (-3) (-5) v); false) (1--|6)
**)
let asub i len s = Array.sub s (arrayIndex i s) len
(**T
  asub 0 2 (1--|6) = (1--|2)
  asub 1 2 (1--|6) = (2--|3)
  asub (-2) 2 (1--|6) = (5--|6)
  maybeE true (fun v -> ignore (asub 3 (-1) v); false) (1--|6)
**)
let asubStride stride i len a =
  let i = arrayIndex i a in
  if i + (len-1) * stride >= alen a
  then invalid_arg "Prelude.asubStride: index out of bounds";
  ainit (fun j -> auget a (i + j*stride)) len
(**T
  asubStride 2 0 3 (1--|10) = [|1; 3; 5|]
  asubStride 2 1 3 (1--|10) = [|2; 4; 6|]
  asubStride 2 (-5) 3 (1--|10) = [|6; 8; 10|]
  maybeE true (fun v -> ignore (asubStride 2 (-2) 2 v); false) (1--|6)
**)
let agroupsOf n a =
  let count, rem = quot_rem (alen a) n in
  unfoldrWhile (gte 0) (fun i -> asub (i*n) n a, i-1) (count-1) @
  if rem = 0 then [] else [asub (-rem) rem a]
(**T
  agroupsOf 2 (1--|4) = [ 1--|2; 3--|4 ]
  agroupsOf 2 (1--|6) = [ 1--|2; 3--|4; 5--|6 ]
  agroupsOf 3 (1--|10) = [ 1--|3; 4--|6; 7--|9; [|10|] ]
**)
let asplitInto n range =
  let len = alen range in
  let plen = int (ceil (float len /. float n)) in
  agroupsOf plen range
(**T
  asplitInto 2 (1--|4) = [ 1--|2; 3--|4 ]
  asplitInto 4 (1--|10) = [ 1--|3; 4--|6; 7--|9; [|10|] ]
**)



(* String operations *)

let suget = String.unsafe_get
let suset = String.unsafe_set
let smake = String.make
let screate = String.create
let sinit f l =
  let s = String.create l in
  for i=0 to l-1 do String.unsafe_set s i (f i) done;
  s
let binit f l =
  let s = String.create l in
  for i=0 to l-1 do String.unsafe_set s i (chr (f i)) done;
  s
let sreverse s =
  let len = String.length s in
  let s2 = String.create len in
  let mlen = len - 1 in
  for i=0 to mlen do
    String.unsafe_set s2 (mlen-i) (String.unsafe_get s i)
  done;
  s2
let srev = sreverse
let slen = String.length
let stringIndex i s = if i < 0 then (slen s) + i else i

let siter = String.iter
let smap f s = sinit (fun i -> suget s i) (slen s)
let sfilter f s =
  let rec aux f s i res =
    if i < 0 then implode res
    else
      let c = suget s i in
      let res = if f c then c::res else res in
      aux f s (i-1) res in
  aux f s (slen s - 1) []

let biter f s = siter (fun c -> f (ord c)) s
let bmap f s = smap (fun c -> char (f (ord c))) s
let bfilter f s = sfilter (fun c -> f (ord c)) s

let azipWith f a b =
  let len = min (alen a) (alen b) in
  ainit (fun i -> f (auget a i) (auget b i) ) len
let amap2 = azipWith
(**T
  azipWith (+) (1--|3) (1--|4) = [| 2; 4; 6 |]
**)

let szipWith f a b =
  let len = min (slen a) (slen b) in
  sinit (fun i -> f (suget a i) (suget b i) ) len
let smap2 = szipWith
(**T
  szipWith (fun x y -> chr ((ord x + ord y) / 2)) "abc" "cde" = "bcd"
  smap2 (fun x y -> x) "foo" "barq" = "foo"
**)
let bzipWith f a b =
  let len = min (slen a) (slen b) in
  sinit (fun i -> chr (f (ord (suget a i)) (ord (suget b i))) ) len
let bmap2 = bzipWith
(**T
  bzipWith average2 "abc" "cde" = "bcd"
  bmap2 (fun x y -> x) "foo" "bar" = "foo"
**)

let implodeArray a = sinit (auget a) (alen a)
(**T
  implodeArray [|'f'; 'o'; 'o'; 'b'; 'a'; 'r'|] = "foobar"
  implodeArray (explodeArray "") = ""
  implodeArray (explodeArray "foo") = "foo"
**)
let explodeArray s = ainit (suget s) (slen s)
(**T
  explodeArray "foobar" = [|'f'; 'o'; 'o'; 'b'; 'a'; 'r'|]
  explodeArray "" = [||]
**)
let char_array = explodeArray

let byte_array s = ainit (fun i -> ord (suget s i)) (slen s)

let sfoldl f init s =
  let rec aux f s len v i =
    if i >= len then v else aux f s len (f v (suget s i)) (i+1) in
  aux f s (slen s) init 0
let sfoldl1 f a =
  let rec aux f i acc len a =
    if i >= len then acc
    else aux f (i+1) (f acc (suget a i)) len a in
  let len = slen a in
  if len < 1 then raise Not_found;
  aux f 1 (suget a 0) len a
let sfoldr f init s =
  let rec aux f s v i =
    if i < 0 then v else aux f s (f (suget s i) v) (i-1) in
  aux f s init (slen s - 1)
let sfoldr1 f a =
  let rec aux f i acc a =
    if i < 0 then acc
    else aux f (i-1) (f (suget a i) acc) a in
  let len = slen a in
  if len < 1 then raise Not_found;
  aux f (len-2) (suget a (len-1)) a

let bfoldl f init s =
  let rec aux f s len v i =
    if i >= len then v else aux f s len (f v (ord (suget s i))) (i+1) in
  aux f s (slen s) init 0
let bfoldl1 f a =
  let rec aux f i acc len a =
    if i >= len then acc
    else aux f (i+1) (f acc (ord (suget a i))) len a in
  let len = slen a in
  if len < 1 then raise Not_found;
  aux f 1 (ord (suget a 0)) len a
let bfoldr f init s =
  let rec aux f s v i =
    if i < 0 then v else aux f s (f (ord (suget s i)) v) (i-1) in
  aux f s init (slen s - 1)
let bfoldr1 f a =
  let rec aux f i acc a =
    if i < 0 then acc
    else aux f (i-1) (f (ord (suget a i)) acc) a in
  let len = slen a in
  if len < 1 then raise Not_found;
  aux f (len-2) (ord (suget a (len-1))) a

let substring i len s = String.sub s (stringIndex i s) len
(**T
  substring 0 2 "foobar" = "fo"
  substring 1 2 "foobar" = "oo"
  substring (-2) 2 "foobar" = "ar"
  maybeE true (fun v -> ignore (substring 3 (-1) v); false) "foobar"
**)
let ssub = substring

let sgroupsOf n a =
  let count, rem = quot_rem (slen a) n in
  unfoldrWhile (gte 0) (fun i -> ssub (i*n) n a, i-1) (count-1) @
  if rem = 0 then [] else [ssub (-rem) rem a]
(**T
  sgroupsOf 2 "foobar" = [ "fo"; "ob"; "ar" ]
  sgroupsOf 3 "foobarbazq" = [ "foo"; "bar"; "baz"; "q" ]
**)
let ssplitInto n range =
  let len = slen range in
  let plen = int (ceil (float len /. float n)) in
  sgroupsOf plen range
(**T
  ssplitInto 2 "foobar" = [ "foo"; "bar" ]
  ssplitInto 4 "foobar" = [ "fo"; "ob"; "ar" ]
  ssplitInto 4 "foobarbazq" = [ "foo"; "bar"; "baz"; "q" ]
**)

let ssum a = bfoldl (+) 0 a
(**T
  ssum "\001\002\003" = 6
**)
let ssumf a = float (bfoldl (+) 0 a)
(**T
  ssumf "\001\002\003" = 6.
**)
let saverage a = ssum a / slen a
(**T
  saverage "ABC" = 66
**)
let saveragef a = ssumf a /. float (slen a)
(**T
  saveragef "ABC" = 66.
**)
let ssubStride stride i len a =
  let i = stringIndex i a in
  if i + (len-1) * stride >= slen a
  then invalid_arg "Prelude.ssubStride: index out of bounds";
  sinit (fun j -> suget a (i + j*stride)) len

let strip = Pcre.replace ~rex:(Pcre.regexp "^\\s+|\\s+$") ~templ:""

let split ?n sep s = Pcre.split ?max:n ~pat:sep s
let rsplit ?n sep s = rev @@ map srev @@ split ?n sep @@ srev s
let nsplit sep n s = split ~n sep s
let nrsplit sep n s = rsplit ~n sep s

let rx = Pcre.regexp
let rex = Pcre.regexp
let escape_rex = Pcre.quote

let rexsplit ?n rex s =
  map (function Pcre.Text s -> s | _ -> "") @@
  filter (function Pcre.Text _ -> true | _ -> false) @@
  Pcre.full_split ?max:n ~rex s
let rexrsplit ?n rex s = rev @@ map srev @@ rexsplit ?n rex @@ srev s
let xsplit ?n rexs s = rexsplit ?n (rx rexs) s
let xrsplit ?n rexs s = rexrsplit ?n (rx rexs) s
let xnsplit rexs n s = xsplit ~n rexs s
let xnrsplit rexs n s = xrsplit ~n rexs s

let rexscan rex s =
  try Array.to_list @@ Array.map Array.to_list @@ Pcre.extract_all ~rex s
  with _ -> []
let scan rexs s = rexscan (rx rexs) s

let rexscan_nth rex n s =
  try
    let arr = Pcre.extract_all ~rex s in
    list (amap (fun a ->
      if alen a <= n
      then invalid_arg "Prelude.rexscan_nth: index out of bounds";
      a.(n)
    ) arr)
  with _ -> []
let scan_nth rexs n s = rexscan_nth (rx rexs) n s

let xfind x s = first (scan_nth x 0 s)
let xfindOpt x s = optNF first (scan_nth x 0 s)

let smatch pat = Pcre.pmatch ~pat
let rexmatch rex = Pcre.pmatch ~rex
let xmatch s = rexmatch (rx s)

let sreplace pat templ = Pcre.replace ~pat ~templ
let rexreplace rex templ = Pcre.replace ~rex ~templ
let xreplace s = rexreplace (rx s)

let frexreplace f rex s =
  let split = Pcre.full_split ~rex s in
  let processed = map (function
    | Pcre.Text s -> s
    | Pcre.Delim s -> f s
    | _ -> "") split in
  String.concat "" processed
let fxreplace f s = frexreplace f (rx s)

let quote l r s = l ^ s ^ r
let join = String.concat

let xreplaceMulti x_rep s =
  let pat = x_rep |> map (quote "(" ")" @. fst) |> join "|" in
  frexreplace (fun p -> assocBy (fun x -> xmatch x p) x_rep) (rex pat) s
(**T
  xreplaceMulti ["f.o","bar"; "b.r","foo"] "foobar" = "barfoo"
  xreplaceMulti ["f.o","bar"; "bar","foo"] "foobar" = "barfoo"
**)

let sreplaceMulti pat_rep s =
  let pat = pat_rep |> map fst |> map escape_rex |> join "|" in
  frexreplace (flip assoc pat_rep) (rex pat) s
(**T
  sreplaceMulti ["foo","bar"; "bar","foo"] "foobar" = "barfoo"
  sreplaceMulti ["f.o","bar"; "bar","foo"] "foobar" = "foofoo"
**)

let ajoin s a = join s (Array.to_list a)
let uppercase = String.uppercase
let lowercase = String.lowercase
let capitalize = String.capitalize
let sslice i j s =
  let i = stringIndex i s
  and j = stringIndex j s + (if j < 0 then 1 else 0) in
  let len = j - i in
  String.sub s i len
(**T
  sslice 0 2 "foobar" = "fo"
  sslice 1 (-1) "foobar" = "oobar"
  sslice 1 (-2) "foobar" = "ooba"
  sslice (-3) (-2) "foobar" = "ba"
  sslice (-3) (-3) "foobar" = "b"
  sslice (-3) (-4) "foobar" = ""
  maybeE true (fun v -> ignore (sslice (-3) (-5) v); false) "foobar"
**)
let ssumSub i len a =
  let rec aux s i l c =
    if l = 0 then c else
    aux s (i+1) (l-1) (c + ord (suget s i)) in
  if i < 0 || len < 0 || i + len > slen a
  then invalid_arg "Prelude.ssumSub: index ouf of bounds"
  else aux a i len 0
(**T
  ssumSub 0 3 "foo" = ssum "foo"
  ssumSub 0 3 "foobar" = ssum "foo"
  ssumSub 3 3 "foobar" = ssum "bar"
  maybeE true (fun s -> ignore @@ ssumSub (-1) 3 s; false) "foo"
  maybeE true (fun s -> ignore @@ ssumSub 1 3 s; false) "foo"
**)
let ssumSubf i len a = float (ssumSub i len a)
let ssumSlice i j s =
  let i = stringIndex i s
  and j = stringIndex j s + (if j < 0 then 1 else 0) in
  let len = j - i in
  ssumSub i len s
(**T
  ssumSlice 0 3 "foo" = ssum "foo"
  ssumSlice 0 3 "foobar" = ssum "foo"
  ssumSlice 3 3 "foobar" = ssum ""
  ssumSlice 3 (-1) "foobar" = ssum "bar"
  maybeE true (fun s -> ignore @@ ssumSlice 1 4 s; false) "foo"
**)
let ssumSlicef i len a = float (ssumSlice i len a)
let saverageSub i len a = ssumSub i len a / len
(**T
  saverageSub 0 3 "foo" = saverage "foo"
  saverageSub 0 3 "foobar" = saverage "foo"
  saverageSub 3 3 "foobar" = saverage "bar"
**)
let saverageSubf i len a = ssumSubf i len a /. float len
(**T
  saverageSubf 0 3 "foo" = saveragef "foo"
  saverageSubf 0 3 "foobar" = saveragef "foo"
  saverageSubf 3 3 "foobar" = saveragef "bar"
**)
let saverageSlice i j s =
  let i = stringIndex i s
  and j = stringIndex j s + (if j < 0 then 1 else 0) in
  let len = j - i in
  ssumSub i len s / len
(**T
  saverageSlice 0 3 "foo" = saverage "foo"
  saverageSlice 0 3 "foobar" = saverage "foo"
  saverageSlice 3 (-1) "foobar" = saverage "bar"
**)
let saverageSlicef i j s =
  let i = stringIndex i s
  and j = stringIndex j s + (if j < 0 then 1 else 0) in
  let len = j - i in
  float (ssumSub i len s) /. float len
(**T
  saverageSlicef 0 3 "foo" = saveragef "foo"
  saverageSlicef 0 3 "foobar" = saveragef "foo"
  saverageSlicef 3 (-1) "foobar" = saveragef "bar"
**)

let words s = rexsplit (rx "\\s+") s
let unwords a = join " " a

let lines s = split "\n" s
let unlines a = join "\n" a ^ "\n"

let rexsplitPartition rex s =
  let rec aux splits l = match splits with
    | [] -> (rev l, None)
    | (a::[]) -> (rev l, Some a)
    | (a::b::t) -> aux t ((a,b)::l) in
  let cleaned_split =
    Pcre.full_split ~rex s |>
    filter (function Pcre.Text _ | Pcre.Delim _ -> true | _ -> false) in
  let padded_split = match cleaned_split with
    | (Pcre.Delim _ :: t) -> (Pcre.Text "") :: cleaned_split
    | _ -> cleaned_split in
  let string_split =
    map (function Pcre.Text s | Pcre.Delim s -> s | _ -> "") padded_split in
  aux string_split []
let xsplitPartition x s = rexsplitPartition (rex x) s

let pickStr indices l = explode l |> pick indices
let pickStrWith funcs l = explode l |> pickWith funcs


(* File and IO operations *)

let putStr = print_string
let putStrLn = print_endline
let puts s = if rexmatch (rx "\n$") s
             then print_string s
             else print_endline s
let output_line oc line =
  output_string oc line;
  output_char oc '\n'

let readLine = input_line
let readChar = input_char
let readByte = input_byte
let readInt = readLine |>. parseInt
let readFloat = readLine |>. parseFloat

let open_append = open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666
let open_append_bin = open_out_gen [Open_wronly; Open_creat; Open_append; Open_binary] 0o666

let fileExists = Sys.file_exists

let finally finaliser f x =
  let r = try f x with e ->
    ( try finaliser x with _ -> () );
    raise e in
  finaliser x;
  r

let withFile filename f = finally close_in f (open_in_bin filename)
let withFileOut filename f = finally close_out f (open_out_bin filename)
let withFileAppend filename f = finally close_out f (open_append_bin filename)

let withUnixFile ?(flags=[Unix.O_RDONLY]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileOut ?(flags=[Unix.O_WRONLY;Unix.O_TRUNC;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileAppend ?(flags=[Unix.O_APPEND;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)


let read ?buf bytes ch =
  let rec aux ch bytes c buf =
    match input ch buf c (bytes-c) with
      | 0 when c = 0 -> raise End_of_file
      | 0 -> String.sub buf 0 c
      | b when c + b = bytes -> buf
      | b -> aux ch bytes (c+b) buf in
  let buf = match buf with
    | None -> String.create bytes
    | Some s ->
      if slen s = bytes then s
      else invalid_arg (sprintf
                        "Prelude.read: buffer size %d differs from read size %d"
                        (slen s) bytes) in
  aux ch bytes 0 buf

let write = output_string

let readAll ch =
  let rec aux ch ret buf =
    match input ch buf 0 4096 with
      | 0 -> Buffer.contents ret
      | b -> Buffer.add_substring ret buf 0 b;
             aux ch ret buf in
  let ret = Buffer.create 4096 in
  let buf = String.create 4096 in
  aux ch ret buf

let stat = Unix.stat

let fileSize filename = (stat filename).Unix.st_size

let fileKind fn = (stat fn).Unix.st_kind
let isKind kind fn = fileKind fn = kind
let isDir = isKind Unix.S_DIR
let isFile = isKind Unix.S_REG
let isLink = isKind Unix.S_LNK
let isFIFO = isKind Unix.S_FIFO
let isSocket = isKind Unix.S_SOCK
let isCharDev = isKind Unix.S_CHR
let isBlockDev = isKind Unix.S_BLK

let fileInode fn = (stat fn).Unix.st_ino
let filePermissions fn = (stat fn).Unix.st_perm
let fileDevice fn = (stat fn).Unix.st_dev
let fileOwner fn = (stat fn).Unix.st_uid
let fileGroup fn = (stat fn).Unix.st_gid

let atime fn = (stat fn).Unix.st_atime
let mtime fn = (stat fn).Unix.st_mtime
let ctime fn = (stat fn).Unix.st_ctime

let readFile filename = withFile filename readAll
let writeFile filename str = withFileOut filename (flip output_string str)
let appendFile filename str = withFileAppend filename (flip output_string str)

let readLines = lines @. readFile

let tokenize t ic = unfoldlOpt (maybeEOF None (fun ic -> Some (t ic, ic))) ic
let tokenizeN t n ic = unfoldlN t n ic
let tokenizeIter t f ic = maybeEOF () (loop (f @. t)) ic
let tokenizeMap t f ic = tokenize (f @. t) ic
let tokenizeFile t filename = withFile filename (tokenize t)
let tokenizeFileN t n fn = withFile fn (tokenizeN t n)

let icEachLine f ic = tokenizeIter input_line f ic
let icMapLines f ic = tokenizeMap input_line f ic
let eachLine f = flip withFile (icEachLine f)
let mapLines f = flip withFile (icMapLines f)

let output_line_flush oc s = output_line oc s; flush oc


(* Filesystem paths *)

(**T
(* Simple relative *)
  expand_path "foo" = (Filename.concat (Unix.getcwd ()) "foo")

(* Absolute *)
  expand_path "/foo" = "/foo"

(* /./ *)
  expand_path "/foo/./bar/./baz/./" = "/foo/bar/baz"

(* /. *)
  expand_path "/foo/bar/." = "/foo/bar"

(* /../ *)
  expand_path "/foo/../bar/../baz" = "/baz"

(* /../ 2 *)
  expand_path "/foo/../bar/../baz/../" = "/"

(* /.. *)
  expand_path "/foo/bar/.." = "/foo"

(* Mixed /./ and /../ *)
  expand_path "/foo/../bar/./baz/qux/./.." = "/bar/baz"

(* Trailing / (absolute) *)
  expand_path "/foo/" = "/foo"

(* Trailing / (relative) *)
  expand_path "foo/" = (Filename.concat (Unix.getcwd ()) "foo")

(* Root *)
  expand_path "/" = "/"

(* Current dir *)
  expand_path "" = (Unix.getcwd ())
**)
let expand_path path =
  let rec replace re tmpl s =
    let s' = Pcre.replace ~rex:(Pcre.regexp re) ~templ:tmpl s in
    if s = s' then s
              else replace re tmpl s' in
  let p1 = if not (Filename.is_relative path) then path
           else Filename.concat (Sys.getcwd ()) path in
  let p2 = replace "/\\.(/|$)" "/" p1 in
  let p3 = replace "/[^/]+/\\.\\.(/|$)" "/" p2 in
  if String.length p3 > 1
  then replace "/$" "" p3
  else p3

module Path =
struct
  type t = Path of string list

  let absolute a =
    let rec aux a lst = match a with
      | [] -> rev lst
      | (""::t) -> aux t [""]
      | (".."::t) -> aux t (maybeNF [] tail lst)
      | (h::t) -> aux t (h::lst) in
    aux a []
  let make s =
    let s = xreplace "/+" "/" s in
    let s = xreplace "/$" "" s in
    Path (split "/" s)
  let to_s (Path a) = if a = [""] then "/" else join "/" a

  let join_path (Path a) (Path b) = Path (absolute (a @ b))

  let join_list path ss = foldl join_path path (map make ss)
  let join path s = join_path path (make s)

  let join_list_to_s path ss = to_s (join_list path ss)
  let join_to_s path s = to_s (join path s)

  let expand path = make (expand_path (to_s path))
end
(**T
  Path.to_s (Path.make "/home") = "/home"
  Path.to_s (Path.make "/home/foo") = "/home/foo"
  Path.to_s (Path.make "/home/") = "/home"
  Path.to_s (Path.join (Path.make "/home/") "foo") = "/home/foo"
  Path.to_s (Path.join (Path.make "/home/") "/foo") = "/foo"
  Path.to_s (Path.join (Path.make "/home/") "..") = "/"
  Path.to_s (Path.join_list (Path.make "/home/") [".."; "tmp"]) = "/tmp"
  Path.join_to_s (Path.make "/home/") "/foo" = "/foo"
  Path.join_to_s (Path.make "/home/") ".." = "/"
  Path.join_list_to_s (Path.make "/home/") [".."; "tmp"] = "/tmp"
**)

let expandPath = expand_path

let (^/) = Filename.concat
let dirExists d = fileExists d && isDir d
let isRoot d = fileInode d = fileInode "/" && fileDevice d = fileDevice "/"
let parentDirs d =
  generateUntil (eq "") (nrsplit "/" 2 |>. first) (expandPath d) @ ["/"]

let dirSeparator = sslice 1 (-2) ("a" ^/ "b")
let splitPath p = match p with
  | "/" -> ["/"]
  | p ->
    begin match split dirSeparator p with
      | (""::t) -> "/"::t
      | ps -> ps
    end
let joinPath ps = foldl1 (^/) ps
(**T
  joinPath (splitPath "/foo/bar/baz") = "/foo/bar/baz"
  joinPath (splitPath "/foo/") = "/foo"
  joinPath (splitPath "/foo") = "/foo"
  joinPath (splitPath "/") = "/"
**)
let relativePath path =
  let cp = splitPath (expandPath ".") in
  let pp = splitPath (expandPath path) in
  let cp, pp = dropWhile2 (=) cp pp in
  joinPath (replicate (len cp) ".." @ pp)

let dirname = Filename.dirname
let basename = Filename.basename


(* Running commands *)

let shell_escape =
  let re = Pcre.regexp "(?=[^a-zA-Z0-9._+/-])" in
  Pcre.replace ~rex:re ~templ:"\\"

let escape_cmd args = join " " (map shell_escape args)

exception Command_error of int * string
let command args =
  let cmd = escape_cmd args in
  let retcode = Sys.command cmd in
  if retcode <> 0 then
    raise (Command_error (retcode, (sprintf "Command failed with %d: %S" retcode cmd)))
  else
    ()

let runCmd = command
let cmdCode args = try command args; 0 with Command_error (rv,_) -> rv

let withRawCmd cmd f =
  let ic,oc = Unix.open_process cmd in
  finally (fun _ -> maybeE () close_out oc; maybeE () close_in ic)
          (f ic) oc
let withRawCmdStdin args f =
  withRawCmd args (fun ic oc -> maybeE () close_in ic; f oc)
let withRawCmdStdout args f =
  withRawCmd args (fun ic oc -> maybeE () close_out oc; f ic)

let withCmd args = withRawCmd (escape_cmd args)
let withCmdStdin args = withRawCmdStdin (escape_cmd args)
let withCmdStdout args = withRawCmdStdout (escape_cmd args)

let readCmd args = withCmdStdout args readAll
let readRawCmd args = withRawCmdStdout args readAll


(* IO piping *)

let pipeWith f init i o = recurseOpt (f i o) init
let pipeChan f = pipeWith (optEOF @.. f)
let unitPipe t f = t (fun ic () -> f ic, ())
let pipeTokenizer input output f ic oc init =
  let line, acc = f (input ic) init in
  output oc line;
  acc

let linePiper = pipeTokenizer input_line output_line_flush
let blockPiper ?buf block_sz = pipeTokenizer (read ?buf block_sz) write

let pipeLines f = pipeChan (linePiper f)
let pipeBlocks block_sz f =
  let buf = String.create block_sz in
  pipeChan (blockPiper ~buf block_sz f)

let withFiles f infile outfile =
  withFile infile (fun ic -> withFileOut outfile (fun oc -> f ic oc))
let withFilesAppend f infile outfile =
  withFile infile (fun ic -> withFileAppend outfile (fun oc -> f ic oc))

let pipeFiles f init = withFiles (pipeChan f init)
let pipeFileLines f init = withFiles (pipeLines f init)
let pipeFileBlocks block_sz f init = withFiles (pipeBlocks block_sz f init)

let pipeAppend f init = withFilesAppend (pipeChan f init)
let pipeAppendLines f init = withFilesAppend (pipeLines f init)
let pipeAppendBlocks block_sz f init = withFilesAppend (pipeBlocks block_sz f init)

let interactWith f = pipeChan (unitPipe linePiper f) ()
let interact f = interactWith f stdin stdout
let interactFiles f = pipeFiles (unitPipe linePiper f) ()
let interactAppend f = pipeAppend (unitPipe linePiper f) ()

let pipeCmd f init args = withCmd args (pipeChan f init)
let pipeCmdLines f init args = withCmd args (pipeLines f init)
let interactWithCmd f args = withCmd args (interactWith f)

let pipeRawCmd f init args = withRawCmd args (pipeChan f init)
let pipeRawCmdLines f init args = withRawCmd args (pipeLines f init)
let interactWithRawCmd f args = withRawCmd args (interactWith f)


(* Common filesystem operations *)

let rename = Sys.rename

let ls d = Array.to_list (Sys.readdir d)
let rm = Sys.remove
let cp s d = pipeFileBlocks 4096 tuple () s d
let mv s d =
  try rename s d
  with Sys_error "Invalid cross-device link" -> cp s d; rm s
let ln_s = Unix.symlink
let ln = Unix.link
let mkdir ?(perm=0o755) s = Unix.mkdir s perm
let rmdir = Unix.rmdir
let mkdir_p ?(perm=0o755) s =
  let nex, ex = span (not @. fileExists) (parentDirs s) in
  iter (mkdir ~perm) (reverse nex)

let getcwd = Sys.getcwd
let pwd = Sys.getcwd
let chdir = Unix.chdir
let cd = Unix.chdir

let chmod perm fn = Unix.chmod fn perm


(* Time *)

let timeNow = Unix.gettimeofday
let timeZone = Netdate.localzone
let formatTime ?(zone=timeZone) fmt f = Netdate.format ~fmt (Netdate.create ~zone f)
let showTime = formatTime "%Y-%m-%d %H:%M:%S%z"
let showDate = formatTime "%Y-%m-%d"
let httpDate = formatTime ~zone:0 "%a, %d %b %Y %H:%M:%S GMT"

let second = 1.0
let minute = 60.0 *. second
let hour = 60.0 *. minute
let day = 24.0 *. hour
let week = 7.0 *. day
let month = 31.0 *. day
let year = 365.0 *. day

let sleep = Unix.sleep


(* Extra file operations *)

let withTempFile suffix f =
  let tmpfilename _ =
    "/tmp" ^/ (showInt (Random.int 1000000) ^ showFloat (timeNow ()) ^ "." ^ suffix) in
  let fn = (0--1000)
    |> find (fun i -> not (fileExists (tmpfilename i)))
    |> tmpfilename in
  finally (fun fn -> if fileExists fn then rm fn else ()) f fn

let appendFileTo oc filename =
  withFile filename (fun ic -> pipeBlocks 4096 tuple () ic oc)

let prependFile filename str =
  if fileSize filename > 32000000 (* use temp file if larger than 32 megs *)
  then withTempFile filename (fun fn ->
    withFileOut fn (fun oc -> write oc str; appendFileTo oc filename);
    mv fn filename)
  else writeFile filename (str ^ readFile filename)

