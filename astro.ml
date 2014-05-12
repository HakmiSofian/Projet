open Directions;;
module Images = Images50;;



let kenny = Images.kenny;;
let girl = Images.cartman;;

let tour = ref 0;;
let scoreKenny,scoreGirl = ref 0,ref 0;;

(* Graphics.close_graph ();; *)
Graphics.open_graph" 700x700";;
Graphics.set_color (Graphics.rgb 100 40 50);;
Graphics.fill_rect 0 0 700 700;;
Graphics.set_color (Graphics.rgb 100 250 250);;
let f i =
  Graphics.draw_segments [|
      (50*i,0,50*i,700);
      (0,50*i,700,50*i)
     |];
in
List.iter f [1;2;3;4;5;6;7;8;9;10;11;12;13];;

Graphics.remember_mode false;;

let afficher (img,x,y) (i,j) =
  Dessiner.dessiner_image img (!i * 50 + x) (650 - !j * 50 +y);;


let plainBlock = [(ref 4,ref 4);(ref 5,ref 4);(ref 6,ref 4);(ref 7,ref 4);(ref 8,ref 4);(ref 8,ref 5);(ref 4,ref 5);(ref 4, ref 6); (ref 4,ref 7);(ref 4,ref 8);(ref 8,ref 6);(ref 8,ref 7);(ref 8,ref 8);(ref 5,ref 8);(ref 6,ref 8);(ref 7,ref 8)];;
let rampWest = [(ref 3, ref 4);(ref 3, ref 5);(ref 3, ref 6);(ref 3, ref 7);(ref 3, ref 8)];;
let rampNord = [(ref 4, ref 3);(ref 5, ref 3);(ref 6, ref 3);(ref 7, ref 3);
(ref 8, ref 3)];;
let rampEst = [(ref 9, ref 4);(ref 9, ref 5);(ref 9, ref 6);(ref 9, ref 7);
(ref 9, ref 8)];;
let rampSud = [(ref 4,ref 9);(ref 5,ref 9);(ref 6,ref 9);(ref 7,ref 9);
	       (ref 8,ref 9)];;
let grassblock =[(ref 5,ref 5);(ref 6,ref 5);(ref 7,ref 5);(ref 5,ref 6);(ref 5,ref 7);(ref 7,ref 6);(ref 7,ref 7);(ref 6,ref 7)];;

let water = [(ref 6,ref 6)];;


let etoiles =  [(ref 1,ref 1);(ref 10,ref 10);(ref 9,ref 8);(ref 5,ref 4);(ref 7,ref 8)];;
let lives = [];;(*[(ref 9,ref 8);(ref 5,ref 4);(ref 7,ref 8)];;*)
let rocher =[(ref 10,ref 2);(ref 9, ref 3);(ref 3, ref 3);(ref 9, ref 3);(ref 9, ref 9);(ref 3, ref 9);(ref 2, ref 7)];;           
let piege =[(ref 7,ref 6)];;



let kenny_i , kenny_j = ref 0, ref 0;;
let girl_i , girl_j = ref 13,ref 13;;




let augmenter_score score =
  incr score;;


let rec presence (l,e)=
  if (l=[]) then false
  else if (List.hd (l) = e) then true
  else presence (List.tl (l),e);;


let surelever_ou_pas img (ri,rj) =
  if( presence(rampWest,(ri,rj)) || presence(rampNord,(ri,rj))|| presence(rampEst,(ri,rj))|| presence(rampSud,(ri,rj)) ) 
  then ( afficher (img,0,20) (ri,rj)) 
  else if ( presence(plainBlock,(ri,rj))|| presence(grassblock,(ri,rj)) ||presence(water,(ri,rj)))
  then ( afficher (img,0,35) (ri,rj))
  else
  afficher (img,0,0) (ri,rj);;



let afficher_decor () =  
  Graphics.synchronize();;
let afficher_perso() =

  surelever_ou_pas kenny (kenny_i,kenny_j);
  surelever_ou_pas girl (girl_i, girl_j);;
  

let afficher_mobiles () =

  List.iter (surelever_ou_pas Images.star) etoiles;
  List.iter (surelever_ou_pas Images.heart) lives;

  afficher_perso ();;

let afficher_score() =
  print_string " Score \n\n Kenny :"; print_int !scoreKenny;
  print_string "\n\n Girl :";print_int !scoreGirl;
  print_string "\n\n\n";;

let gagner () =
  if ((!scoreKenny + !scoreGirl) =(List.length etoiles))  then (
    if( !scoreKenny > !scoreGirl ) then(
      print_string "\n\n\n\tKENNY A GAGNER !!!!!\n\n\n\t"
    )
    else if ( !scoreKenny = !scoreGirl ) then
      print_string "\n\n\n\tEGALITER !!!!!\n\n\n\t"
    else 
      print_string "\n\n\n\tGIRL A GAGNER !!!!!\n\n\n\t"

  )
;;

Graphics.remember_mode true;;
 List.iter (afficher (Images.rampNord50,0,0)) rampNord;;
 List.iter (afficher (Images.plainBlock,0,0)) plainBlock;;
 List.iter (afficher (Images.grassblock,0,0)) grassblock;;
 List.iter (afficher (Images.water,0,0)) water;;
 List.iter (afficher (Images.rampWest50,0,0)) rampWest;;
 List.iter (afficher (Images.rampEst,0,0)) rampEst;;
 List.iter (afficher (Images.rampSud,0,0)) rampSud;;
 List.iter (afficher (Images.rock,0,0)) rocher;;
Graphics.remember_mode false;;



let deplacer direction =
  let () = 
      if(!tour=0)then(
	match direction with
	  
	| E -> if !kenny_i < 13 then ( kenny_i := !kenny_i + 1)  else (kenny_i := 0)
	| O -> if !kenny_i > 0 then ( kenny_i := !kenny_i - 1) else (kenny_i := 13) 
	| S -> if !kenny_j < 13 then ( kenny_j := !kenny_j + 1) else (kenny_j := 0)
	| N -> if !kenny_j > 0 then (kenny_j := !kenny_j - 1) else (kenny_j := 13)
      )
      else(
	match direction with
	  
	| E -> if !girl_i < 13 then ( girl_i := !girl_i + 1)  else (girl_i := 0)
	| O -> if !girl_i > 0 then ( girl_i := !girl_i - 1) else (girl_i := 13) 
	| S -> if !girl_j < 13 then ( girl_j := !girl_j + 1) else ( girl_j := 0)
	| N -> if !girl_j > 0 then (girl_j := !girl_j - 1) else (girl_j := 13)

     )
	
  in
    
  let ramasser_etoile (ri,rj) =

    if (((!ri,!rj) = (!kenny_i,!kenny_j)) || ((!ri,!rj) = (!girl_i,!girl_j)) ) then(
      begin
        ri := -1;
        rj := 9;
	if (!tour=0) then augmenter_score scoreKenny
	else augmenter_score scoreGirl
      end
    )
  in
  List.iter ramasser_etoile etoiles;

  let impossibiliter (ai,aj) = 
    if(!tour=0)then
      (
	if( ( (!ai,!aj) = (!kenny_i, !kenny_j)) || ((!girl_i,!girl_j) = (!kenny_i,!kenny_j)))
	then(	  
	  match direction with	    

	  | E -> if !kenny_i = 0  then (kenny_i := 13) else (kenny_i := !kenny_i - 1)
	  | O -> if !kenny_i = 13 then (kenny_i := 0)  else (kenny_i := !kenny_i + 1)  
	  | S -> if !kenny_j = 0  then (kenny_j := 13)  else (kenny_j := !kenny_j - 1)
	  | N -> if(presence(rampWest,(kenny_i, kenny_j))) then (kenny_j := !kenny_j +1) else 
	      if !kenny_j = 13 then (kenny_j := 0) else (kenny_j := !kenny_j + 1) 
	)
	
      )
    else(
      if( ( (!ai,!aj) = (!girl_i, !girl_j)) || ((!girl_i,!girl_j) = (!kenny_i,!kenny_j)))
      then(
	
	match direction with

	| E -> if !girl_i = 0  then (girl_i := 13) else (girl_i := !girl_i - 1)
	| O -> if !girl_i = 13 then (girl_i := 0)  else (girl_i := !girl_i + 1)  
	| S -> if(presence(rampWest,(girl_i, girl_j))) then (girl_j := !girl_j -1) else
	    if !girl_j = 0  then (girl_j := 13)  else (girl_j := !girl_j - 1)
	| N -> if (presence(rampWest,(girl_i, girl_j))) then (girl_j := !girl_j +1) else 
	    if !girl_j = 13 then (girl_j := 0) else (girl_j := !girl_j + 1)

      )
    )
      
  in
  List.iter impossibiliter rocher;
  

  
  let ramasser_lives (ri,rj) =
	if (((!ri,!rj) = (!kenny_i,!kenny_j)) || ((!ri,!rj) = (!girl_i,!girl_j)) ) then(
	  begin
            ri := -1;
            rj := 9;
	  end
      )
  in
  List.iter ramasser_lives lives;






  let changer_tour letour =
    if (!letour=0) then ( incr letour) else (decr letour)
  in
  changer_tour tour;

  gagner();
  afficher_decor ();
  afficher_mobiles ();;

let jouer p = Interprete.run p deplacer;;

let score ()  =  
 afficher_score();;

let quitter () =
Graphics.close_graph();;
  
  

List.length rocher;;


print_string "          *** космонавт ***\n";;
print_string "       Salut jeune astronaute\n";;
print_string "\n";;
print_string " ******   Tahar et Sofian   ******  ";;
print_string " Crédit image : lostgarden.com et SouthPark\n";;



afficher_mobiles ();;
