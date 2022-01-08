open Graph
open Tools
open Printf

module Int_set = Set.Make(struct
    type t = int
    let compare = compare
  end)

module Int_map = Map.Make(struct
    type t = int
    let compare = compare
  end) 

(*Le path est une liste de couple : id1 (noeud1), id2 (noeud2)*)
type 'a path = (id * id ) list 


type 'a reseau = {
    graph : 'a graph; 
    origine : id; 
    destination : id; 
} 

type flot ={
    courant : int; 
    capacite: int; 
    
} 
(*initialiser_graphe nous permet de convertir un int graph en flot graph avec une capacité actuelle de 0 pour chaque arc*)
let initialiser_graphe gr = gmap gr (fun c ->{courant=0; capacite= c} )

(*add_res_arc ajoute un arc au graphe seulement si sa valeur est différente de 0 *)
let add_res_arc gr origine destination valeur = match valeur with 
    | 0 -> gr
    | _ -> add_arc gr origine destination valeur
    
(*build_res_graphe nous permet de construire le residual graph à partir d'un graphe de flot  *)
let build_res_graphe gr = let gr_res = clone_nodes gr in 
    e_fold gr (fun g origine destination fl ->
        let prochain = fl.capacite - fl.courant in 
        let precedent = fl.courant in 
        let new_g = add_res_arc g origine destination prochain in 
        add_res_arc new_g destination origine precedent)
    gr_res


(*predesseur_a_chemin convertit les prédécesseurs vers un path d'une origine à une destination. *)
(*Une liste vide est retournée lorsque aucun chemin n'est trouvé *)
let predesseur_a_chemin predesseur origine destination = 
    let rec loop o d acu = if o = d then 
        acu 
        else 
            match Int_map.find_opt d predesseur with 
            | None ->[]
            | Some x -> let new_acu = (x,d) :: acu in 
              if x = o then new_acu else 
                loop o x new_acu
    in 
    loop origine destination []  
    
(*trouver_path nous permet de trouver le chemin entre le noeud d'origine et le noeud de destination d'un graphe résiduel*)
let rec trouver_path res_reseau = let queue = Queue.create () in
  let () = Queue.add res_reseau.origine queue in
  let visite = Int_set.add res_reseau.origine Int_set.empty in
  let rec trouver_path_rec q v pred = match Queue.take_opt q with
    | None -> predesseur_a_chemin pred res_reseau.origine res_reseau.destination
    | Some x -> if x = res_reseau.destination then
        predesseur_a_chemin pred res_reseau.origine res_reseau.destination
      else
        let arcs = out_arcs res_reseau.graph x in
        let filtered_arcs = List.filter (fun (dest, _) -> not (Int_set.mem dest v) ) arcs in
        let successeur_seq = List.to_seq ( 
            List.map (fun (dest, _) -> dest) filtered_arcs
          ) in
        let predesseur_seq = List.to_seq ( 
            List.map (fun (dest, _) -> (dest, x)) filtered_arcs
          ) in 
        let new_pred = Int_map.add_seq predesseur_seq pred in
        let new_v = Int_set.add_seq successeur_seq v in
        let () = Queue.add_seq q successeur_seq in
        trouver_path_rec q new_v new_pred
  in 
  trouver_path_rec queue visite Int_map.empty

(* trouver_min permet de trouver la capacité minimale d'un chemin donné d'un graphe *)
let trouver_min gr path = List.fold_left (fun current_min (s, d) -> 
    let current = find_arc gr s d in
    match current with
    | None -> raise Not_found
    | Some x -> min x current_min)
    max_int path 

(* apply_path applique l'augmentation ou la réduction des flots tout au long du chemin *)
let apply_path gr path aug = List.fold_left (fun new_gr (s, d) -> 
    match find_arc new_gr s d with
    | None -> (match find_arc new_gr d s with
        | None -> raise Not_found
        | Some x -> new_arc new_gr d s {courant = x.courant - aug; capacite = x.capacite})
    | Some x -> new_arc new_gr s d {courant = x.courant + aug; capacite = x.capacite})
    gr path

(* get_flot_max retourne le flot maximale d'un graphe donné *)
let get_flot_max flot_reseau = let arcs = out_arcs flot_reseau.graph flot_reseau.origine in
  List.fold_left (fun current (dest, flot) -> current + flot.courant) 0 arcs

(* run_ff exécute l'algorithme ford fulkerson sur un réseau donné*)
let run_ff reseau = let flot_graph = initialiser_graphe reseau.graph in
  let rec loop fl = 
    let res_graph = build_res_graphe fl in
    let path = trouver_path {graph = res_graph; origine = reseau.origine; destination = reseau.destination } in
    match path with
    | [] -> {graph = fl; origine = reseau.origine; destination = reseau.destination }
    | _ ->
      let aug = trouver_min res_graph path in 
      let new_fl = apply_path fl path aug in
      loop new_fl
  in loop flot_graph








 
    

    

