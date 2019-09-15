let read threshold x =
  let chan = open_in x in
  let res =
    Asak.Lambda_utils.hash_all threshold @@
      Marshal.from_channel chan in
  close_in chan;
  res

let get_args () =
  match Array.to_list Sys.argv with
  | _::nb::xs -> (nb,xs)
  | _ -> failwith "specify a threshold"

let main () =
  let (threshold,xs) = get_args () in
  let read = read (int_of_string threshold) in
  let all_hashs =
    List.fold_left
      (fun acc x -> read x @ acc) [] xs in
  let all_cluster = Asak.Clustering.cluster all_hashs in
  Asak.Clustering.print_cluster (fun x -> x) all_cluster

let () = main ()
