type edge = {
    uid_start_node : int;
    uid_end_node : int;
    name : string;
    nature : string;
    speed_limit : int;
    one_way : bool;
  }



let get_uid_start_node edge =
  edge.uid_start_node


let get_uid_end_node edge =
  edge.uid_end_node


let get_name edge =
  edge.name


let get_nature edge =
  edge.nature


let get_speed_limit edge =
  edge.speed_limit


let get_one_way edge =
  edge.one_way


let read_edge_file city =
  let filename = "city_maps/" ^city^  "_edges.txt" in
  try
    let channel = open_in filename in
    try
      while true do
      let line = input_line channel in
      Printf.printf "%s\n" line
    done
    with
    | End_of_file -> close_in channel
  with
  | Sys_error err -> Printf.printf "Error : %s\n" err


let edge_of_line line =
  Scanf.sscanf line "{ %d, %d, %S, %S, %d, %b }" (fun uid_start_node uid_end_node name nature speed_limit one_way  -> {uid_start_node ; uid_end_node ; name ; nature ; speed_limit ; one_way})

let print_edge n =
  Printf.printf "%d -> %d %s = %s, %d, %b\n" n.uid_start_node n.uid_end_node n.name n.nature n.speed_limit n.one_way


let rec print_edge_list edges =
  match edges with
   | [] -> ()
   | n :: t -> print_edge n;
               print_edge_list t

let rec load_edge_list city =
  let filename = "city_maps/" ^city^  "_edges.txt" in
  try
    let channel = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line channel in
        let edge = edge_of_line line in
        read_lines (edge :: acc)
      with
      | End_of_file ->
          close_in channel;
          List.rev acc
    in
    read_lines []
  with
  | Sys_error err ->
      prerr_endline ("Error: " ^ err);
      []

let rec str_contains str sub =
  let len_str = String.length str in
  let len_sub = String.length sub in
  if (len_sub > len_str) then
    false
  else if (len_sub = 0) then
    true
  else if (String.sub str 0 len_sub = sub) then
    true
  else
    str_contains (String.sub str 1 (len_str - 1)) sub


let find_edges name edge_list =
  let filter_function edge =
    str_contains edge.name name
  in
  List.filter filter_function edge_list
