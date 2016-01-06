let a_fs =
[ "photos", Dir
    [ "march", Dir
        [ "photo_1.bmp", File ;
          "photo_2.bmp", File ;
          "photo_3.bmp", File ;
          "index.html", File ] ;
      "april", Dir
        [ "photo_1.bmp", File ;
          "photo_2.bmp", File ;
          "index.html", File ] ] ;
  "videos", Dir
    [ "video1.avi", File ;
      "video2.avi", File ;
      "video3.avi", File ;
      "video4.avi", File ;
      "best.avi", Symlink [ "video4.avi" ] ;
      "index.html", File ] ;
  "indexes", Dir
    [ "videos.html",
      Symlink [ ".." ; "videos" ; "index.html" ] ;
      "photos_march.html",
      Symlink [ ".." ; "photos" ; "march" ; "index.html" ] ;
      "photos_april.html",
      Symlink [ ".." ; "photos" ; "april" ; "index.html" ] ;
      "photos_may.html",
      Symlink [ ".." ; "photos" ; "may" ; "index.html" ] ] ];;

let another_fs =
  [("funny.doc", File);
   ("htaccess.doc", Symlink ["opt"; "local"; "config.html"]);
   ("htaccess.wav", File);
   ("opt",
    Dir
     [(".htaccess.html", Symlink ["report.doc"]);
      ("Users",
       Dir
        [(".config.ppt", File); (".funny.html", File);
         (".funny.wav", Symlink [".funny.html"]); ("id_rsa.doc", File);
         ("id_rsa.ppt", File);
         ("passwd.txt", Symlink [".."; ".."; "htaccess.wav"])]);
      ("local",
       Dir
        [(".htaccess.wav", File); ("config.html", File);
         ("config.ppt", File); ("funny.txt", Symlink [".."; "report.doc"]);
         ("passwd.doc", File)]);
      ("report.doc", File)]);
   ("passwd.html", File);
   ("report.ppt", Symlink ["opt"; "Users"; "id_rsa.ppt"])]

let print_filesystem (root : filesystem) : unit =
  let rec print_filesystem
      (lvl : int)
      (items : filesystem)
      (path : string list)
    : unit =
    match items with
    | [] -> ()
    | (name, File) :: tl ->
      print_file lvl name;
      print_newline ();
      print_filesystem lvl tl path;
    | (name, Symlink (rpath)) :: tl ->
      if file_exists root (resolve (List.rev (name :: path)) rpath) then
        print_symlink lvl name rpath
      else
        print_symlink lvl name ["INVALID"];
      print_newline ();
      print_filesystem lvl tl path;
    | (name, Dir (contents)) :: tl ->
      print_dir lvl name;
      print_newline ();
      print_filesystem (lvl + 1) contents (name :: path);
      if path <> [] then
        print_filesystem lvl tl path
      else
        print_filesystem lvl tl [];
  in
  print_filesystem 0 root [];;
