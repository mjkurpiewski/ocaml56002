Random.self_init

Random.float 1;;

let mandog = words "I am a man and my dog is a good dog and a good dog makes a good man";;

et print_table table = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table [];;
(fun h -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []) dogtbl;;

let next_in_ltable (table : ltable) (word : string) : string =
  if not (List.mem_assoc word table) then
    raise Not_found
  else
    let candidates = List.assoc word table in
    List.nth candidates (Random.int (List.length candidates));;



  List.fold_left
    (fun acc x ->
       let (name, value) = x in
       (name, (float_of_int value /. total_entries)) :: acc)
    []
    choices;;


Computing
next_in_htable

(let table = Hashtbl.create 16 in
 Hashtbl.add table "a"
   {total = 7;
    amounts =
      [("man", 1); ("land", 1); ("kitchen", 1); ("house", 2); ("fridge", 1);
       ("beer", 1)]} ;
 Hashtbl.add table "has" {total = 1; amounts = [("a", 1)]} ;
 Hashtbl.add table "is" {total = 3; amounts = [("no", 1); ("a", 2)]} ;
 Hashtbl.add table "there" {total = 3; amounts = [("is", 3)]} ;
 Hashtbl.add table "man" {total = 1; amounts = [("who", 1)]} ;
 Hashtbl.add table "where" {total = 2; amounts = [("there", 2)]} ;
 Hashtbl.add table "beer" {total = 2; amounts = [("in", 2)]} ;
 Hashtbl.add table "kitchen"
   {total = 2; amounts = [("in", 1); ("STOP", 1)]} ;
 Hashtbl.add table "no" {total = 1; amounts = [("beer", 1)]} ;
 Hashtbl.add table "land" {total = 1; amounts = [("where", 1)]} ;
 Hashtbl.add table "who" {total = 1; amounts = [("has", 1)]} ;
 Hashtbl.add table "fridge" {total = 1; amounts = [("in", 1)]} ;
 Hashtbl.add table "the" {total = 1; amounts = [("kitchen", 1)]} ;
 Hashtbl.add table "in" {total = 5; amounts = [("the", 1); ("a", 4)]} ;
 Hashtbl.add table "START" {total = 1; amounts = [("there", 1)]} ;
 Hashtbl.add table "house"
   {total = 2; amounts = [("where", 1); ("in", 1)]} ;
 table)

  let total : float = float_of_int word_distribtuion.total in
  let freqs : frequencies = word_distribtuion.amounts in
  let random_float : float = Random.float 1.0 in

  let rec aux
      (freqs : frequencies)
      (rand : float) =
    match freqs with
    | [] -> ""
    | (name, prob) :: [] -> name
    | (name, prob) :: tl ->
      if 0.0 < random_float && random_float <= (float_of_int prob /. total) then
        (print_float random_float; name)
      else
        aux tl rand
  in
  aux freqs random_float;;

(* Testing on "a b a c a c a" *)
(* Expected distribution for "c" *)
(* "c" -> "a" 100% *)
(* 0 pt Unexpected distribution for "a" *)
(* "a" -> "STOP" 0% *)
(* "a" -> "b" 25% *)
(* "a" -> "c" 74% *)
(* Expected distribution for "START" *)
(* "START" -> "a" 100% *)
(* Expected distribution for "b" *)
(*     "b" -> "a" 100% *)


             let rec aux
      (freqs : frequencies)
      (counters : int * int)
      (rand : int) =
    match freqs with
    | [] -> ""
    | (name, prob) :: [] -> name
    | (name, prob) :: tl ->
      let (c1, loc_prob) = counters in
      print_string (string_of_int rand); print_newline ();
      print_string ((string_of_int c1) ^ " " ^ (string_of_int loc_prob));
      print_newline ();
      print_string name;
      print_newline ();
      if c1 = rand then
        name
      else if prob > 0 then
        aux choices (c1 + 1, loc_prob - 1) rand
      else
        aux tl (c1 + 1, 0) rand
  in
  aux choices (1, 1) rng;;


  let total_entries = word_distribtuion.total in
let choices = word_distribtuion.amounts in

int rouletteSelect(double[] weight) {
  // calculate the total weight
  double weight_sum = 0;
  for(int i=0; i<weight.length; i++) {
    weight_sum += weight[i];
  }
  // get a random value
  double value = randUniformPositive() * weight_sum;
  // locate the random value based on the weights
  for(int i=0; i<weight.length; i++) {
    value -= weight[i];
    if(value <= 0) return i;
  }
  // only when rounding errors occur
  return weight.length - 1;
}
