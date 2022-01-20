type block =  {hash0 : int ; tx : int  ; hash1 : int};;
	
type t == int vect;;

type 'a blockchain = 
	|Nil
	|Block of ( block * 'a blockchain);;
	

let test_block = {hash0 = 0; tx = 12345; hash1 = 12344};;
let test = (Nil,{hash0 = 0; tx = 12345; hash1 = 12344});;
	
		
(* operation sur la blockchain *)

(* hashage *)
let h = fun x -> x;;
let merkle t = _;; (*Renvoi tx, la racine de l'arbre de merkle des données*)


let add_hash a b = (* hash + tx*)
	let pui10 = ref 1 in
	while b> !pui10 do
		pui10 := !pui10*10
		 done;
	(b + !pui10 * a);;
	
(* Verification de la blockchain *)
(* Critère :
	le hash1 du block n égale au hash0 du block n+1
	fonction de hashage bien calculer 
		verifie si l'antecedent est bien une concatenation de tx et hash0
	*)
	
let verif_last = (* b0 b1 : type block*)
	
	let hash_verif b = match b.hash0 with
		 |0 -> true
		 |_ -> h (add_hash b.hash0 b.tx) = b.hash1
		 in
		
	let aux = function
		|Nil  -> true
		|Block( _ , Nil)  -> true
		|Block(b1,Block(b0,_))  -> b1.hash0 = b0.hash1 
									&& hash_verif b1
	in aux;;

let rec verif_all = function
	|Nil -> true
	|Block(Nil,_) -> true
	|Block(Block(b_chain),b) -> (verif_last b) && verif_all b_chain;;


let rec verif p= function(*Verifie les p derniers blocs*)
	|Nil -> true
	|Block(b,Nil) -> b1.hash0 = b0.hash1
	|b when p =0 -> true 
	|Block(b1,Block(b0,b_chain)) -> b1.hash0 = b0.hash1 && verif (p-1) b_chain;;


let verif = 
	let aux h0 p = function
		|
		|Block(b,bc) when p = 0 ->  


let add tx_ t_ h = function
	|Nil -> Block(hash0 = 0; tx = tx_; t=t_; hash1 = h tx_ )
	|Block{hash0 = _; tx = _; t = _; hash1 = h0} -> 
		Block(hash0 = h0; tx = tx_; t=t_; hash1 = h (tx_ + h0) );;

			 
