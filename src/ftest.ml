open Gfile
open Tools
open FordFulkerson
open Graph


let flot_to_string gr = gmap gr (fun fl -> String.concat "/" [(string_of_int fl.courant); (string_of_int fl.capacite)])

let test_ffrun_init gr =
  let graph_init = initialiser_graphe gr in
  let () = export "test/graph_init" (flot_to_string graph_init) in
  graph_init

let test_ffrun_apply gr =
  let graph_apply = apply_path gr [(0, 3); (3, 1); (1, 4); (4, 5)] 1 in
  let () = export "test/graph_apply" (flot_to_string graph_apply) in
  graph_apply

let test_ffrun_res gr =
  let graph_res = build_res_graphe gr in
  let () = export "test/graph_res" (gmap graph_res string_of_int) in
  graph_res

let test_ffrun_min gr =
  let min = trouver_min gr [(0, 3); (3, 1); (1, 4); (4, 5)] in
  Printf.printf "min: %d\n%!" min

let test_ffrun_path gr =   
  let predesseur = trouver_path {
      graph = gr;
      origine = 0;
      destination = 4;
    } in
  List.iter (fun (o, d) -> Printf.printf "%d -> %d\n%!" o d ) predesseur 

let test_ffrun gr = 
  let graph_init = test_ffrun_init gr in
  let graph_apply = test_ffrun_apply graph_init in
  let graph_res = test_ffrun_res graph_apply in
  let () = test_ffrun_min gr in
  test_ffrun_path graph_res


let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
    in
    (* Open file *)
    let graph = from_file infile in
    (* Convert graph *)
    let int_graph = (gmap graph int_of_string) in
    (* Execute FF *)
    let flot_reseau = run_ff {graph = int_graph; origine = source; destination = sink} in
    (* Print flot max *)
    let () = Printf.printf "Flot max : %d\n%!" (get_flot_max flot_reseau) in
    (* Convert to string for export *)
    let final_graph = flot_to_string flot_reseau.graph in
    (* Rewrite the graph that has been read. *)
    let () = export outfile final_graph in 
    
    ()
    (* TESTS *)
    (*
    (* Open file *)
    let graph = from_file infile in
    let () = test_ffrun (gmap graph int_of_string) in
        let graph = clone_nodes graph in
        let graph = gmap (gmap (gmap graph int_of_string) (fun x -> x + 1)) string_of_int in
        let graph = gmap (add_arc (gmap graph int_of_string) 2 1 13) string_of_int in *)
    (* END TESTS*)

