let rec read_all_pkg desc =
  try
    let str = Unix.readdir desc in
    if str = ".." || str = "."
    then read_all_pkg desc
    else str::read_all_pkg desc
  with
  | End_of_file -> []

let test_problem x res =
  if not (res = 0)
  then failwith ("PROBLEM WITH: " ^ x)

let install_pkg x =
  let res = Sys.command ("opam install -y --switch=/home/nobrakal/prog/ocaml/ " ^ x) in
  test_problem x res;
  let res = Sys.command ("PKG_NAME="^x^" opam reinstall -y --switch=/home/nobrakal/prog/ocaml/ " ^ x) in
  test_problem x res

let main dir =
  let desc = Unix.opendir dir in
  let pkg_list = read_all_pkg desc in
  Unix.closedir desc;
  List.iter install_pkg pkg_list

let () = main Sys.argv.(1)
