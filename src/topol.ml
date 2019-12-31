(* Sortowanie topologiczne  *)
(* Autor: Jan Wawszczak *)
(* Reviewer: Wojciech Przytuła  *)

open PMap

exception Cykliczne

(* Zmienia listę par (element, połączenia) na mapę,  *)
(* gdzie kluczem jest element a wartością połączenia *)
let vertList2VertMap lst =
	let rec helper l acc =
		match l with
		| [] -> acc
		| (elem, cons) :: t ->
			let allcons = try find elem acc with Not_found -> []
			in helper t (add elem (cons @ allcons) acc)
	in helper lst empty

(* Przechodzi DFSem po drzewie rozpoczynając w wierzchołku [v]		 				*)
(* przypisując każdemu wierzchołkowi czas wejścia do niego w mapie [pre] 			*)
(* oraz czas wyjścia z niego w mapie [post] jednocześnie dodając 					*)
(* do listy [sorted] wierzchołki w kolejności takiej, w jakiej DFS z ncih wyszedł 	*)
let rec dfs_tsort v verts pre post timer sorted =
	if mem v !pre then begin (* Jeśli do wierzchołka wszedł już wcześniej DFS *)
		if not (mem v !post) then raise Cykliczne (* ale jeśli z niego nie wyszedł, to jest cykl *)
		else sorted	(* jeśli z niego wyszedł, to był już odwiedzony i nie trzeba nic zmieniać *)
	end else begin 
		pre := add v !timer !pre; (* przypisanie wierzchołkowi czasu wejścia *)
		timer := !timer + 1;
		(* Odwiedzenie wszystkich dzieci akumulując wynikową listę *)
		let rec visit lst acc =
			match lst with
			| [] -> acc
			| elem :: t ->
				visit t (dfs_tsort elem verts pre post timer acc)
		in
			let visitResult = try visit (find v verts) sorted
							  with Not_found -> sorted in
			post := add v !timer !post; (* przypisanie wierzchołkowi czasu wyjścia *)
			timer := !timer + 1;
			v :: visitResult
		end

let topol lst =
	let verts = vertList2VertMap lst
	and pre =  ref empty
	and post = ref empty
	and timer = ref 1 in
	(* Odwiedzenie wszystkich wierzchołków DFSem akumulując wynikową listę *)
	let rec visit l acc =
		match l with
		| [] -> acc
		| (elem, _) :: t ->
			visit t (dfs_tsort elem verts pre post timer acc)
	in visit lst []
;;
(* Testy - ze wspólnej puli  *)
(*
exception WA;;

let debug = true;;

(* True if the given order is a correct topological order, *)
(* false otherwise. Checks all edges.                      *)
let test graph order =
  let hashtbl = Hashtbl.create (List.length order)
  in
  List.iteri (fun i x -> Hashtbl.add hashtbl x i) order;
  let check_one (v, l) =
    List.iter (fun u ->
      if (Hashtbl.find hashtbl v) > (Hashtbl.find hashtbl u)
      then raise WA;) l
  in
  try (List.iter check_one graph; true)
  with WA -> false

(* Tests if Topol raises Cykliczne for the given graph *)
let test_cyclic g =
  try let _ = topol g in false
  with Cykliczne -> true

;;
      
if debug then print_endline "Acyclic correctness tests...";;
      
let g = [
  ("1", ["2"; "3"]);
  ("3", ["2"]);
  ("4", ["3"; "2"])
];;

assert(test g (topol g));;

let g = [
  ("first", ["second"; "fourth"; "eighth"]);
  ("second", ["fourth"; "eighth"]);
  ("third", ["fourth"; "fifth"; "sixth"]);
  ("fourth", ["eighth"]);
  ("fifth", ["sixth"; "seventh"]);
  ("sixth", ["eighth"; "first"]);
  ("seventh", ["eighth"]);
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3]);
  (2, [4]);
  (3, [4]);
  (4, [5; 6]);
  (5, [7]);
  (6, [7]);
];;

assert(test g (topol g));;

let g = [
  (1, [7; 2]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 4; 8]);
  (2, [16; 32]);
  (4, [64; 128]);
  (8, [256; 512]);
  (16, [1024]);
  (32, [2048]);
  (64, [4096]);
  (128, [8192]);
  (256, [16384]);
  (512, [32768]);
];;

assert(test g (topol g));;

let g = [
  ("Lorem", ["sit"]);
  ("ipsum", ["sit"; "amet"]);
  ("dolor", ["amet"; "elit"]);
  ("sit", ["consectetur"; "adipiscing"; "elit"]);
];;

assert(test g (topol g));;

let g = [];;

assert(test g (topol g));;

let g = [
  ("through", ["the"; "gates"; "of"; "hell"]);
  ("hell", ["as"; "we"; "make"; "our"; "way"; "to"; "heaven"]);
  ("PRIMO", ["VICTORIA"]);
];;

assert(test g (topol g));;

let g = [
  ("one", ["three"]);
  ("one", ["two"]);
  ("two", []);
  ("two", []);
  ("two", ["three"]);
];;

assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Cyclic correctness tests...";;

let g = [
  (10.001, [10.002]);
  (10.002, [10.001])
];;

assert(test_cyclic g);;

let g = [
  (1, [7; 2; 3]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (2, [3]);
  (3, [4; 5; 3]);
];;

assert(test_cyclic g);;

let g = [
  ("pole", ["pole"; "lyse"; "pole"])
];;

assert(test_cyclic g);;

let g = [
  ("tu", ["tudu"]);
  ("tudu", ["tudu"; "tudu"; "tudu"])
];;

assert(test_cyclic g);;

if debug then print_endline "OK";;

if debug then print_endline "Marcin Michorzewski's acyclic correctness tests...";;

let g = [
  (11, [12]);
  (12, [13]);
  (7, [8]);
  (8, [9]);
  (1, [2]);
  (13, [6]);
  (3, [4]);
  (5, [6]);
  (6, [7]);
  (10, [11])
];;

assert(test g (topol g));;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14]);
  (15, [16; 8])
];;

assert(test g (topol g));;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14]);
  (15, [16; 8]);
  (8, [14])
];;

assert(test g (topol g));;

let g = [
  (1, [2]);
  (2, []);
  (3, [2])
];;

assert(test g (topol g));;

let g = [
  ('a', ['e']);
  ('b', ['a'; 'c']);
  ('c', ['a']);
  ('e', [])
];;

assert(test g (topol g));;

if debug then print_endline "OK";;

if debug then print_endline "Marcin Michorzewski's cyclic correctness tests...";;

let g = [
  (3, [4]);
  (5, [6]);
  (6, [7]);
  (10, [11]);
  (11, [12]);
  (12, [13]);
  (7, [8]);
  (9, [13]);
  (8, [9]);
  (1, [2]);
  (13, [6])
];;

assert(test_cyclic g);;

let g = [
  ("Polska", ["Niemcy"]);
  ("Niemcy", ["Polska"])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (2, [5])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (1, [5]);
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (2, [6])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (3, [4]);
  (4, [1]);
  (5, [6]);
  (6, [3]);
  (1, [6])
];;

assert(test_cyclic g);;

let g = [
  (1, [2]);
  (2, [3]);
  (3, [1])
];;

assert(test_cyclic g);;

let g = [
  (1, [2; 3; 4]);
  (3, [7; 8]);
  (4, [9; 10]);
  (10, [15; 16]);
  (2, [5; 6]);
  (13, [4; 10]);
  (11, [12]);
  (12, [13; 14]);
  (15, [16; 8]);
  (8, [12])
];;

assert(test_cyclic g);;
if debug then print_endline "OK";;
*)