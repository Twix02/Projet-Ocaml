open Graph 

(*clone_nodes nous permet de cloner les noeuds d'un graphe, on aura le même graphe sans les arcs 
 *)
val clone_nodes: 'a graph -> 'b graph

(*gmap nous permet de prendre le graphe sans arcs et de rajouter un arc entre chaque 2 noeuds avec son cout*)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(*add_arc nous permet de rajouter les couts à chaque arc du graphe *)
val add_arc: int graph -> id -> id -> int -> int graph
 


