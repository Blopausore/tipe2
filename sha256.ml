let k = [|
[| 1116352408; 1899447441; 3049323471; 3921009573; 961987163; 1508970993; 2453635748; 2870763221;
 3624381080; 310598401; 607225278; 1426881987; 1925078388; 2162078206; 2614888103; 3248222580;
  3835390401; 4022224774; 264347078; 604807628; 770255983; 1249150122; 1555081692; 1996064986;
   2554220882; 2821834349; 2952996808; 3210313671; 3336571891; 3584528711; 113926993; 338241895;
    666307205; 773529912; 1294757372; 1396182291; 1695183700; 1986661051; 2177026350; 2456956037;
     2730485921; 2820302411; 3259730800; 3345764771; 3516065817; 3600352804; 4094571909; 275423344;
      430227734; 506948616; 659060556; 883997877; 958139571; 1322822218; 1537002063; 1747873779;
 1955562222; 2024104815; 2227730452; 2361852424; 2428436474; 2756734187; 3204031479; 3329325298|];;


let h0 = [| 1779033703; 3144134277; 1013904242; 2773480762; 1359893119; 2600822924; 528734635; 1541459225|];;

let p32 = 4294967296 ;; (* = 2**32  *)

let t = (0b1111 lsr 2) lsl 2;;



let int_to_str n =
	let k = ref 1 in
	while n >  !k do k := !k*2 done;
	 
	let rec aux k = function
		|0 -> "0"
	 	|1 -> "1"
	 	|n when n>=k -> "1" ^ aux ( k / 2) (n - k)
	 	|n -> "0" ^ aux (k / 2)  n;
	 in aux !k n;;

let rotr n x  = ( (x lsr n) lor (x lsl (32 - n) ) );;
let 


let sha256 m = (* On sait que l'entree sera de au maximum 512 bits*)
	
	let  p= ref 1 
	and l = ref 0 in
	while m >  !p do 
		p := !p*2;
		incr !l;
	done;
	
	let k = 448 - ( !l +1 ) mod 512 in	
	let add_0 = m lsl !l
	let add_1= m lsl 1 in
	let add_k0 = add_1 lsl k
	let b = ref ( (add_k0 lsl 64) + !l) in
	
	let n = (!l + k + 448)/512 in
	
	let w = Array.make_matrix 64 n 0 in
	
	for i = 0 to (n-1) do
		let bi =ref ( !b lsr (n-1-i)*512) in 
		b := !b - ( (!b lsr (n-1-i)*512) lsl (n-1-i)*512 )  
		let 
		let aux j = function
			|0 <- if j-8 < 64 
		
		
		
	let hash = Array.make_matrix (n+1) 8 0 in
	for i = 0 to 7 do hash.(i-1).(0).(i) <- h0.(i) done;
	
	for i = 1 to n do
		for t= 16 to 63 do
			let s0 = (rotr 7 w.(t-15)) lxor (rotr 13 w.(t-15)) lxor (w.(t-15) lsr 3) 
			and s1 = (rotr 17 w.(t-2)) lxor (rotr 19 w.(t-2)) lxor (w.(t-2) lsr 10) 
			in
			w.(t) <- (w.(t-16) + s0 + w.(t-7) + s1) mod p32;
		let  a = ref hash.(i-1).(0) and b = ref hash.(i-1).(1) and c= ref hash.(i-1).(2) and d= ref hash.(i-1).(3) and 
		and e= ref hash.(i-1).(4) and f = ref hash.(i-1).(5) and g= ref hash.(i-1).(6) and h = ref hash.(i-1).(7) in
		
		for i=0 to 63 do
			let s1 = (rotr 6 !e) lxor (rotr 11 !e) lxor (rotr 25 !e)
			and ch = (!e land !f) lxor ((lnot !e) land !g) in
			let temp1 = (!h + s1 + ch + k.(t) + w.(t)) mod p32
			and s0 = (rotr 2 !a) lxor (rotr 13 !a) lxor (rotr 22 !a) in
			let temp2 = ( s0 + s1 ) mod p32 in
			h := !g ;
			g := !f;
			f := !e;
			e := !d + temp1;
			d := !c;
			c := !b;
			b := !a;
			a := temp1 + temp2;
		done;
		
		hash.(i).(0) <- (!a + hash.(i-1).(0)) mod p32;
		hash.(i).(1) <- (!b + hash.(i-1).(1)) mod p32;
		hash.(i).(2) <- (!c + hash.(i-1).(2)) mod p32;
		hash.(i).(3) <- (!d + hash.(i-1).(3)) mod p32;
		hash.(i).(4) <- (!e + hash.(i-1).(4)) mod p32;
		hash.(i).(5) <-  (!f + hash.(i-1).(5)) mod p32;
		hash.(i).(6) <- (!g + hash.(i-1).(6)) mod p32; 
		hash.(i).(7) <- (!h + hash.(i-1).(7) )mod p32;
	let r = ref 0 in
	for i = 0 to 7 do 
		r := !r + hash.(n).(0);
		for j = (t+1) to 7 do
			hash.(n).(j) <- hash.(n).(j) lsl 32
		done
	done;
	r;;
	 
			
	
	
	
	
	
