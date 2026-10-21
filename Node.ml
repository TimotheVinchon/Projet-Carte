exception EmptyNodeList
exception NodeNotFound
exception MultipleNodesFound

type node = {
  uid: int;
  longitude: float;
  latitude: float;
  nature: string              
} 
 
let get_uid node =
  node.uid

let get_longitude node=
  node.longitude

let get_latitude node=
  node.latitude

let get_nature node=
  node.nature

let read_node_file city =
  let filename = "city_maps/" ^city^  "_nodes.txt" in
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

let node_of_line line =
  Scanf.sscanf line  "{ %d, %f, %f, %s }" (fun uid longitude latitude nature -> {uid ; longitude ; latitude ; nature}) 

 
let print_node node =
  let uid = get_uid node in
  let longitude = get_longitude node in
  let latitude = get_latitude node in
  let nature = get_nature node in
  Printf.printf "#%d = longitude : %f, latitude : %f, nature : %s \n" uid longitude latitude nature

let rec print_node_list nodes =
  match nodes with
  | [] -> ()
  | n :: rest ->
      print_node n;
      print_node_list rest


let rec load_node_list city =
  let file_name = "city_maps/" ^ city ^ "_nodes.txt" in
  try
    let channel = open_in file_name in
    let rec read_lines acc =
      try
        let line = input_line channel in
        let new_node = node_of_line line in
        read_lines (new_node :: acc)
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

let get_min_longitude node_list =
  match node_list with
  | [] -> raise EmptyNodeList
  | first_node :: rest ->
      let rec find_min_lon min_lon nodes =
        match nodes with
        | [] -> min_lon
        | node :: remaining ->
            let lon = get_longitude node in
            let new_min_lon = if lon < min_lon
              then lon 
              else min_lon in
            find_min_lon new_min_lon remaining
      in
      find_min_lon (get_longitude first_node) rest

let get_max_longitude node_list =
  match node_list with
  | [] -> raise EmptyNodeList
  | first_node :: rest ->
      let rec find_max_lon max_lon nodes =
        match nodes with
        | [] -> max_lon
        | node :: remaining ->
            let lon = get_longitude node in
            let new_max_lon = if lon > max_lon
              then lon 
              else max_lon in
            find_max_lon new_max_lon remaining
      in
      find_max_lon (get_longitude first_node) rest

let get_min_latitude node_list =
  match node_list with
  | [] -> raise EmptyNodeList
  | first_node :: rest ->
      let rec find_min_lat min_lat nodes =
        match nodes with
        | [] -> min_lat
        | node :: remaining ->
            let lat = get_latitude node in
            let new_min_lat = if lat < min_lat
              then lat
              else min_lat in
            find_min_lat new_min_lat remaining
      in
      find_min_lat (get_latitude first_node) rest

let get_max_latitude node_list =
  match node_list with
  | [] -> raise EmptyNodeList
  | first_node :: rest ->
      let rec find_max_lat max_lat nodes =
        match nodes with
        | [] -> max_lat
        | node :: remaining ->
            let lat = get_latitude node in
            let new_max_lat = if lat > max_lat
              then lat
              else max_lat in
            find_max_lat new_max_lat remaining
      in
      find_max_lat (get_latitude first_node) rest

let find_node node_uid node_list =
  let rec find_single_node uid nodes =
    match nodes with
    | [] -> raise NodeNotFound
    | n :: rest ->
        if get_uid n = uid then (
          if List.exists (fun x -> get_uid x = uid) rest then
            raise MultipleNodesFound
          else
            n
        )
        else
          find_single_node uid rest
  in
  find_single_node node_uid node_list
