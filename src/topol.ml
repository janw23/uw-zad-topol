open PMap

exception Cykliczne

(* Zmienia listę par (element, połączenia) na mapę,  *)
(* gdzie kluczem jest element a wartością połączenia *)
let vertList2VertMap lst =
	let rec helper l acc =
		match l with
		| [] -> acc
		| (elem, cons) :: t ->
			helper t (add elem cons acc)
	in helper lst empty

let rec dfs_tsort v verts pre post timer sorted =
	if mem v !pre then begin
		if not (mem v !post) then raise Cykliczne
		else sorted
	end else begin 
		pre := add v !timer !pre; (* Ustawienie czasu wejścia do wierzchołka v *)
		timer := !timer + 1;
		let rec visit lst acc =
			match lst with
			| [] -> acc
			| elem :: t ->
				visit t (dfs_tsort elem verts pre post timer acc)
		in
			let visitResult = visit (find v verts) sorted in
			post := add v !timer !post;
			timer := !timer + 1;
			v :: visitResult
		end

let topol lst =
	let verts = vertList2VertMap lst
	and pre =  ref empty
	and post = ref empty
	and timer = ref 1 in
	let rec visit l acc =
		match l with
		| [] -> acc
		| (elem, _) :: t ->
			visit t (dfs_tsort elem verts pre post timer acc)
	in visit lst []
