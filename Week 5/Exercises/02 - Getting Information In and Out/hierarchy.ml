(* The given prelude... *)

type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list;;

(* Given a path, represented by an ordered list of strings,
   returns a pretty-printed path. *)
let rec print_path (path : string list) : unit =
  match path with
  | [] -> ()
  | hd :: [] -> print_string hd
  | hd :: tl -> print_string (hd ^ "/"); print_path tl;;

(* Given a integer for the depth of a file and the name of a
   file, returns a pretty-printed file string. *)
let rec print_file (lvl : int) (name : string) : unit =
  match lvl with
  | 0 -> print_string name
  | n -> print_string "| "; print_file (n - 1) name;;

(* Give an integer for the depth of a symlink, the name of
   the symlink, and the relative path to the file, pretty-
   prints a symlink string. *)
let print_symlink (lvl : int) (name : string) (path : string list) : unit =
  print_file lvl name;
  print_string " -> ";
  print_path path;;

(* Given a integer for the depth of a directory and the name of a
   directory, returns a pretty-printed directory string. *)
let rec print_dir (lvl : int) (name : string) : unit =
  match lvl with
  | 0 -> print_string ("/" ^ name)
  | n -> print_string "| "; print_dir (n - 1) name;;

(* Given a filesystem, will pretty-print the entire contents of
   the file system. *)
let print_filesystem (root : filesystem) : unit =
  let rec print_filesystem (lvl : int) (items : filesystem) : unit =
    match items with
    | [] -> ()
    | (name, File) :: tl ->
      print_file lvl name;
      print_newline ();
      print_filesystem lvl tl;
    | (name, Symlink (path)) :: tl ->
      print_symlink lvl name path;
      print_newline ();
      print_filesystem lvl tl;
    | (name, Dir (contents)) :: tl ->
      print_dir lvl name;
      print_newline ();
      print_filesystem (lvl + 1) contents;
      print_filesystem lvl tl;
  in
  print_filesystem 0 root;;

(* Given the full path from the root to a symlink as 'sym' and
   the relative path for the symlink, returns the full path from
   the root to the target of the symlink. *)
let rec resolve (sym : string list) (path : string list) : string list =
  let rec resolve (acc : string list) (path : string list) : string list =
    match path with
    | [] -> List.rev acc
    | hd :: [] -> resolve (hd :: acc) []
    | hd :: tl ->
      match hd with
      | ".." ->
        if acc <> [] then
          resolve (List.tl acc) tl
        else
          resolve acc tl
      | dir -> resolve (dir :: acc) tl
  in
  resolve (List.tl (List.rev sym)) path;;

(* Given a filesystem, root, and the absolute path to a File, path,
   as a string list, traverses the filesystem to locate the File.
   Dirs and Symlinks are not valid targets of file_exists. *)
let rec file_exists (root : filesystem) (path : string list) : bool =
  match root with
  | [] -> false
  | (name, File) :: fs ->
    if List.hd path = name then
      true
    else
      file_exists fs path
  | (name, Symlink (rpath)) :: fs -> file_exists fs path
  | (name, Dir (subdirs)) :: fs ->
    if List.hd path = name && (List.tl path) <> [] then
      file_exists subdirs (List.tl path)
    else
      file_exists fs path;;

(* Given a filesystem, will pretty-print the entire contents of
   the file system. Updated to replace the relative path of a
   Symlink with "INVALID" if it cannot be resolved to a File. *)
let print_filesystem_returns (root : filesystem) : unit =
  let rec print_filesystem_returns
      (lvl : int)
      (items : filesystem)
      (path : string list)
    : unit =
    match items with
    | [] -> ()
    | (name, File) :: tl ->
      print_file lvl name;  print_newline ();
      print_filesystem_returns lvl tl path;
    | (name, Symlink (rpath)) :: tl ->
      if file_exists root (resolve (List.rev (name :: path)) rpath) then
        print_symlink lvl name rpath
      else
        print_symlink lvl name ["INVALID"];
      print_newline ();
      print_filesystem_returns lvl tl path;
    | (name, Dir (contents)) :: tl ->
      print_dir lvl name; print_newline ();
      print_filesystem_returns (lvl + 1) contents (name :: path);
      if path <> [] then
        print_filesystem_returns lvl tl path
      else
        print_filesystem_returns lvl tl [];
  in
  print_filesystem_returns 0 root [];;
