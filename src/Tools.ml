open Graph

let clone_nodes gr = (let res = empty_graph
                        in n_fold gr(fun aux id -> new_node aux id) res)

let gmap gr f = (let res = clone_nodes gr in 
                e_fold gr (fun aux id1 id2 label -> new_arc aux id1 id2 (f label))res)

let add_arc g id1 id2 n = match (find_arc g id1 id2) with
    |Some a -> new_arc g id1 id2 (a + n)
    |None -> new_arc g id1 id2 n

