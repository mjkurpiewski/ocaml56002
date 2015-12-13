module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end;;

module CharHashedType =
struct
  type t = char
  let equal i j = i = j
  let hash i = Char.code i
end

module CharHashtbl = Hashtbl.Make (CharHashedType);;

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () =
    Trie (None, CharHashtbl.t)

  let lookup trie w =
    "Replace this string with your implementation." ;;

  let insert trie w v =
    "Replace this string with your implementation." ;;

end
