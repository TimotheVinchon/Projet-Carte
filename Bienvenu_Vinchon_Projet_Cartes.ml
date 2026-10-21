open Graphics
open Sys
open Node
open Edge
           
let draw_edge edge node_list x_scale y_scale min_longitude min_latitude =
  let start_node = find_node edge.uid_start_node node_list in
  let end_node = find_node edge.uid_end_node node_list in
  let x_s = int_of_float ((get_longitude start_node -. min_longitude) *. x_scale) in
  let y_s = int_of_float ((get_latitude start_node -. min_latitude) *. y_scale) in
  let x_e = int_of_float ((get_longitude end_node -. min_longitude) *. x_scale) in
  let y_e = int_of_float ((get_latitude end_node -. min_latitude) *. y_scale) in
  if get_nature edge = "footway" then
    set_color green
  else
    set_color blue;
  moveto x_s y_s;
  lineto x_e y_e

let draw_edges node_list edge_list x_size y_size =
 List.iter (fun edge -> draw_edge edge node_list (float_of_int x_size /. ((get_max_longitude node_list) -. (get_min_longitude node_list))) (float_of_int y_size /. ((get_max_latitude node_list) -. (get_min_latitude node_list))) (get_min_longitude node_list) (get_min_latitude node_list)) edge_list
 
let open_map city node_list edge_list =
  let x_size = 1000 in
 let y_size = 700 in
 open_graph " 1000 x700";
 resize_window x_size y_size ;
 set_window_title ("Carte de " ^ city);
 draw_edges node_list edge_list x_size y_size ;
 ignore ( wait_next_event [ Key_pressed ]);
 close_graph ()
  let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Usage: %s <city>\n" Sys.argv.(0)
  else
    let city_name = Sys.argv.(1) in
    Printf.printf "The city name is : %s\n" city_name;
    match load_node_list city_name with
    | [] -> Printf.printf "No nodes loaded for the specified city. \n"
    | first_node :: rest_nodes ->
        Printf.printf "Information of the first node in city %s:\n" city_name;
        print_node first_node;
        let min_lon = get_min_longitude rest_nodes in
        let max_lon = get_max_longitude rest_nodes in
        let min_lat = get_min_latitude rest_nodes in
        let max_lat = get_max_latitude rest_nodes in
        Printf.printf "Longitude limits: min = %f, max = %f\nLatitude limits: min = %f, max = %f\n" min_lon max_lon min_lat max_lat; 
        let node_uid = 32215316 in 
        let found_node = find_node node_uid rest_nodes in
        Printf.printf "Node with UID %d found:\n" node_uid;
        print_node found_node;
        (*let node_uid2 = 372215316 in 
        let found_node = find_node node_uid2 rest_nodes in
        Printf.printf "Node with UID %d found:\n" node_uid2;
        print_node found_node;*)

   Printf.printf "-------------------------------\n";

   Printf.printf "Test Edge :\n";
   let rec take n list =
   if n <= 0 then
     []
   else match list with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs
   in
   let load = load_edge_list city_name in
   match load with
    | [] -> Printf.printf"No edges loaded for the specified city. \n"
    | edges ->
        Printf.printf "Information of the first 10 edges in city %s:\n" city_name;
        let first_10_edges = take 10 load in
        let first_20_edges = take 20 load in
        List.iter print_edge first_10_edges;

        Printf.printf "---------------------\n";

        let test_contains1 = str_contains "Hello world!" "world" in
        Printf.printf "str_contains (Hello world!) (world) :\n%b \n" test_contains1;
        let test_contains2 = str_contains "Hello world!" "Hello world!" in
        Printf.printf "str_contains (Hello world!) (Hello world!)  :\n%b \n" test_contains2;
        let test_contains3 = str_contains "Hello world!" "Not in" in
        Printf.printf "str_contains (Hello world!) (Not in) :\n%b \n" test_contains3;
        let test_contains4 = str_contains "Hello world!" "" in
        Printf.printf "str_contains (Hello world!) () :\n%b \n" test_contains4;
        let test_contains5 = str_contains "Hello world!" "Longer than first str" in
        Printf.printf "str_contains (Hello world!) (Longer than first str) :\n%b \n" test_contains5;
        let test_contains6 = str_contains "Hello" "world" in
        Printf.printf "str_contains (Hello)  (world) :\n%b \n" test_contains6;
        let test_contains7 = str_contains "Hello" "Hello" in
        Printf.printf "str_contains (Hello) (Hello) :\n%b \n" test_contains7;

        Printf.printf "---------------------\n";

        Printf.printf "find_edges (Avenue) (first_20_edges) :\n";
        print_edge_list (find_edges "Avenue" (first_20_edges));
        Printf.printf "find_edges (nan) (first_10_edges) :\n";
        print_edge_list (find_edges "nan" (first_10_edges));
        Printf.printf "find_edges (Not in) (first_10_edges) :\n";
        print_edge_list (find_edges "Not in" (first_10_edges));
        Printf.printf "find_edges () (first_10_edges) :\n";
        print_edge_list (find_edges "" (first_10_edges));
 
        let edges_file = "./city_maps/"^city_name^"_edge.txt" in
        let nodes_file = "./city_maps/"^city_name^"_node.txt" in
        if file_exists (nodes_file) && file_exists (edges_file) then
          begin
            let node_list = load_node_list nodes_file in
            let edge_list = load_edge_list edges_file in
            open_map city_name node_list edge_list; 
            end

