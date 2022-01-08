open Graph

 (*Type du chemin où les arcs ont des labels de type 'a. (id,id) est le couple des noeuds *)
type 'a path = (id*id) list

(*Le réseau est un type constituté d'un graphe, du noeud d'origine et du noeud de destination*)
type 'a reseau ={ 
    graph: 'a graph; 
    origine: id; 
    destination: id; 
} 

(* flot est un type constitué du flot courant et de la capacité de chaque arc*)
type flot ={
    courant:int; 
    capacite: int;  
} 

(*initialiser_graphe permet l'initialisation du graphe, c'est-a-dire on met en argument un graphe constitués d'entiers et nous retourne le graphe constitué de flots associé*)
(*initialiser_graphe nous permet de convertir un int graph en flot graph avec une capacité actuelle de 0 pour chaque arc*)
val initialiser_graphe: int graph -> flot graph 

(*build_res_graphe nous permet de construire le graphe résiduel associé au graphe de flots. Le graphe résiduel est le graphe qui modélise sur chaque arc l'écart entre le flot et la capacité de l'arc *)
val build_res_graphe: flot graph -> int graph 

(*trouver_path nous permet de trouver le chemin entre le noeud d'origine et le noeud de destination définis dans le réseaus*)
val trouver_path : int reseau -> 'a path 

(*trouver_min retourne la capacité minimale du chemin d'un graphe donné*)
val trouver_min : int graph -> 'a path -> int 

(*apply_path applique l'augmentation ou la réduction des flots tout au long du chemin*)
val apply_path: flot graph -> 'a path -> int -> flot graph 

(*get_flot_max permet de trouver le flot maximale sur un réseau de flots donné*) 
val get_flot_max: flot reseau -> int 

(*run_ff exécute l'algorithme Ford Fulkerson sur le réseau donné*)
val run_ff: int reseau -> flot reseau 





