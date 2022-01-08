open Graph

(*clone_nodes nous permet de cloner les noeuds d'un graphe, on aura le même graphe sans les arcs 
 *)
let clone_nodes gr = 
    let res = empty_graph in n_fold gr (fun aux id -> new_node aux id) res

(*gmap nous permet de prendre le graphe sans arcs et de rajouter un arc entre chaque 2 noeuds avec son cout noté label
On aura un graphe avec des arcs et leurs couts *)
let gmap gr f = 
    let res = clone_nodes gr in e_fold gr (fun aux id1 id2 label -> new_arc aux id1 id2 (f label))res

(* Dans add_arc on utilise find_arc qui nous permet de renvoyer le cout de l'arc entre le noeud id1 et le noeud id2
d'un graphe gr. Nous avons 2 cas : si on trouve un arc on lui rajoute c à son cout 
Si on ne trouve pas d'arc on renvoi un nouvel arc avec le cout c *)
let add_arc gr id1 id2 c = 
    match (find_arc gr id1 id2) with
        |Some a -> new_arc gr id1 id2 (a + c)
        |None -> new_arc gr id1 id2 c

