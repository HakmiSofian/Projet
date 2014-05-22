open Directions;;
module Images = Images50;;



let kenny = Images.kenny;;
let cartman = Images.cartman;;



let tour = ref 0;;
let bouger = ref 0;;
let scoreKenny,scorecartman = ref 0,ref 0;;
let compteurCoeurKenny,compteurCoeurCartman = ref 0, ref 0;;


(* Graphics.close_graph ();; *)
Graphics.open_graph" 700x700";;
Graphics.set_color (Graphics.rgb 0 170 0);;
Graphics.fill_rect 0 0 700 700;;
Graphics.set_color (Graphics.rgb 100 250 250);;
(*let f i =
  Graphics.draw_segments [|
      (50*i,0,50*i,700);
      (0,50*i,700,50*i)
     |];
in

List.iter f [1;2;3;4;5;6;7;8;9;10;11;12;13];;
*)
Graphics.remember_mode false;;

let afficher (img,x,y) (i,j) =
  Dessiner.dessiner_image img (!i * 50 + x) (650 - !j * 50 +y);;


let plainBlock = [(ref 4,ref 4);(ref 5,ref 4);(ref 6,ref 4);(ref 7,ref 4);(ref 8,ref 4);(ref 8,ref 5);(ref 4,ref 5);(ref 4, ref 6); (ref 4,ref 7);(ref 4,ref 8);(ref 8,ref 6);(ref 8,ref 7);(ref 8,ref 8);(ref 5,ref 8);(ref 6,ref 8);(ref 7,ref 8)
;(ref 3,ref 1);
(ref 9,ref 10)];;
let rampWest = [(ref 3, ref 4);(ref 3, ref 5);(ref 3, ref 6);(ref 3, ref 7);(ref 3, ref 8);
(ref 2,ref 1);
(ref 8,ref 10)];;
let rampNord = [(ref 4, ref 3);(ref 5, ref 3);(ref 6, ref 3);(ref 7, ref 3);
(ref 8,ref 3)];;
let rampEst = [(ref 9, ref 4);(ref 9, ref 5);(ref 9, ref 6);(ref 9, ref 7);
(ref 9, ref 8);(ref 4,ref 1);(ref 10,ref 10)];;
let rampSud = [(ref 4,ref 9);(ref 5,ref 9);(ref 6,ref 9);(ref 7,ref 9);
	       (ref 8,ref 9)];;
let grassblock =[(ref 5,ref 5);(ref 6,ref 5);(ref 7,ref 5);(ref 5,ref 6);(ref 5,ref 7);(ref 7,ref 6);(ref 7,ref 7);(ref 6,ref 7)];;

let water = [(ref 6,ref 6)];;



let etoiles =  [(ref 3,ref 1);(ref 3,ref 4);(ref 9,ref 10);(ref 9,ref 8);(ref 5,ref 4);(ref 7,ref 8);(ref 5,ref 7);
(ref 10,ref 1);(ref 11,ref 1);(ref 10,ref 2);(ref 11,ref 2);(ref 12,ref 2);
(ref 1,ref 11);(ref 2,ref 11);(ref 3,ref 11);
(ref 2,ref 12);(ref 3,ref 12);
];;
let lives = [];;(*[(ref 5,ref 7)];;*)
let rocher =[(ref 1,ref 0);(ref 13,ref 0);(ref 12, ref 0);(ref 11, ref 0);(ref 10, ref 0);(ref 9,ref 0);(ref 9, ref 1);(ref 9, ref 2);(ref 9, ref 3);(ref 13, ref 1);(ref 13, ref 2);(ref 10, ref 3);(ref 11, ref 3);(ref 12, ref 3);(ref 13, ref 3);
(ref 0,ref 10);(ref 0,ref 11);(ref 0,ref 12);(ref 0,ref 13);
(ref 1,ref 13);(ref 2,ref 13);(ref 3,ref 13);(ref 4,ref 13);(ref 4,ref 13);
(ref 4,ref 11);(ref 4,ref 10);
(ref 2,ref 10);(ref 3,ref 10);];;           
let selector =[(ref 12,ref 1)];;



let kenny_i , kenny_j = ref 5, ref 4;;
let cartman_i , cartman_j = ref 9,ref 9;;



let augmenter_score score =
  incr score;;



let rec presence (l,e)= match l with 
  | [] -> false
  | x::xs when (x=e) -> true
  | x::xs -> presence (xs,e)
;;


let sur_le_sol (ri,rj) =
  if( presence(plainBlock,(ri,rj))) then false
  else  if( presence(rampNord,(ri,rj))) then false
  else  if( presence(grassblock,(ri,rj))) then false
  else  if( presence(water,(ri,rj))) then false
  else  if( presence(rampWest,(ri,rj))) then false
  else  if( presence(rampEst,(ri,rj))) then false
  else  if( presence(rampSud,(ri,rj))) then false
  else true
;;




let surelever_ou_pas img (ri,rj) =
  if( presence(rampWest,(ri,rj)) || presence(rampNord,(ri,rj))|| presence(rampEst,(ri,rj))|| presence(rampSud,(ri,rj)) ) 
  then ( afficher (img,0,20) (ri,rj)) 
  else if ( presence(plainBlock,(ri,rj))|| presence(grassblock,(ri,rj)) ||presence(water,(ri,rj)))
  then ( afficher (img,0,35) (ri,rj))
  else
  afficher (img,0,0) (ri,rj)
;;



let afficher_decor () =  
  Graphics.synchronize();;
let afficher_perso() =

  surelever_ou_pas kenny (kenny_i,kenny_j);
  surelever_ou_pas cartman (cartman_i, cartman_j);;
  

let afficher_mobiles () =

  List.iter (surelever_ou_pas Images.star) etoiles;
  List.iter (surelever_ou_pas Images.heart) lives;

  afficher_perso ();;

let afficher_score() =
  print_string " Score \n\n Kenny :"; print_int !scoreKenny;
  print_string "\n\n Cartman :";print_int !scorecartman;
  print_string "\n\n\n"
;;

let gagner () =
  if ((!scoreKenny + !scorecartman) =(List.length etoiles))  then (
    if( !scoreKenny > !scorecartman ) then(
      print_string "\n\n\n\tKENNY A GAGNE !!!!!\n\n\n\t"
    )
    else if ( !scoreKenny = !scorecartman ) then
      print_string "\n\n\n\tEGALITER !!!!!\n\n\n\t"
    else 
      print_string "\n\n\n\tCartman A GAGNER !!!!!\n\n\n\t"

  )
;;



let rec recup_ligne x liste = match liste with
  | [] -> []
  | (a,b)::xs when (!b = x) ->  (a,b)::(recup_ligne x xs); 
  | (a,b)::xs when (!b != x)->  recup_ligne x xs
  | _::_ -> liste
;;
  


Graphics.remember_mode true;;

for i=0 to 13 do 

 List.iter (afficher (Images.selector,0,0)) (recup_ligne i selector);
 List.iter (afficher (Images.rampNord50,0,0)) (recup_ligne i rampNord);
 List.iter (afficher (Images.plainBlock,0,0)) (recup_ligne i plainBlock);
 List.iter (afficher (Images.grassblock,0,0)) (recup_ligne i grassblock);
 List.iter (afficher (Images.water,0,0)) (recup_ligne i water);
 List.iter (afficher (Images.rampWest50,0,0)) (recup_ligne i rampWest);
 List.iter (afficher (Images.rampEst,0,0)) (recup_ligne i rampEst);
 List.iter (afficher (Images.rampSud,0,0)) (recup_ligne i rampSud);
 List.iter (afficher (Images.rock,0,0)) (recup_ligne i rocher);


done;;
Graphics.remember_mode false;;




let deplacer direction =

  let verifier (mtni,mtnj) (prei,prej) =
    if ( presence(rocher,(prei,prej)) ) then (	print_string "\nImpossible ! Il y a un rocher.\n\n\t";false)
    else if (((!kenny_i,!kenny_j) = (!prei,!prej)) || ((!cartman_i,!cartman_j)=(!prei,!prej))) then (print_string "\nImpossible ! Il y a un autre joueur.\n\n\t";false)
    else if((presence(rampWest,(prei,prej))))then(
                       if(  
			   (((!prei,!prej) = (!mtni+1,!mtnj)) && (sur_le_sol (mtni,mtnj)))               ||
                           ( ( (!prei,!prej) = (!mtni-1,!mtnj) ) && (presence(plainBlock,(mtni,mtnj))) ) ||
                           ( ( (!prei,!prej) = (!mtni-1,!mtnj) ) && (presence(grassblock,(mtni,mtnj))) ) ||
                           ( ( (!prei,!prej) = (!mtni-1,!mtnj) ) && (presence(water,(mtni,mtnj))))       ||
                           ( ( ((!prei,!prej) = (!mtni,!mtnj-1)) || ((!prei,!prej) = (!mtni,!mtnj+1)) ) && (presence(rampWest,(mtni,mtnj))))    ||
                           ( ( (!prei,!prej) = (!mtni-1,!mtnj) ) && (presence(rampEst,(mtni,mtnj))))
                           
		       )
                       then(  true)
                       else (print_string "\nTu ne peux pas passer par là ! cherche un autre chemin.\n\n\t";false))
    else if((presence(rampNord,(prei,prej))))then(
                       if(  ( ((!prei,!prej) = (!mtni,!mtnj+1)) && (sur_le_sol (mtni,mtnj)))  ||
                            ( ( (!prei,!prej) = (!mtni,!mtnj-1) ) && (presence(plainBlock,(mtni,mtnj))))  ||
                            ( ( (!prei,!prej) = (!mtni,!mtnj-1) ) && (presence(grassblock,(mtni,mtnj))))  ||
                            ( ( (!prei,!prej) = (!mtni,!mtnj-1) ) && (presence(water,(mtni,mtnj))))            ||
                            ( ( ((!prei,!prej) = (!mtni+1,!mtnj)) || ((!prei,!prej) = (!mtni-1,!mtnj)) ) && (presence(rampNord,(mtni,mtnj))))     ||
                            ( ( (!prei,!prej) = (!mtni,!mtnj-1) ) && (presence(rampSud,(mtni,mtnj)))) 
                                                                
		       )
                                                   then(  true)
                                                   else false)
    else if((presence(rampEst,(prei,prej))))then(
                       if(  ( ((!prei,!prej) = (!mtni-1,!mtnj)) && (sur_le_sol (mtni,mtnj)))  ||
                            ( ( (!prei,!prej) = (!mtni+1,!mtnj) ) && (presence(plainBlock,(mtni,mtnj)))) ||
                            ( ( (!prei,!prej) = (!mtni+1,!mtnj) ) && (presence(grassblock,(mtni,mtnj)))) ||
                            ( ( (!prei,!prej) = (!mtni+1,!mtnj) ) && (presence(water,(mtni,mtnj)))) ||
                            ( ( ((!prei,!prej) = (!mtni,!mtnj-1)) || ((!prei,!prej) = (!mtni,!mtnj+1)) ) && (presence(rampEst,(mtni,mtnj)))) ||
                            ( ( (!prei,!prej) = (!mtni+1,!mtnj) ) && (presence(rampWest,(mtni,mtnj))))
                                                                
		       )
                                                   then(  true)
                                                   else (print_string "\nTu ne peux pas passer par là ! cherche un autre chemin.\n\n\t";false))
    else if((presence(rampSud,(prei,prej))))then(
                       if(  ( ((!prei,!prej) = (!mtni,!mtnj-1)) && (sur_le_sol (mtni,mtnj)))  ||
                            ( ((!prei,!prej) = (!mtni,!mtnj+1)) && (presence(plainBlock,(mtni,mtnj)))) ||
                            ( ((!prei,!prej) = (!mtni,!mtnj+1)) && (presence(grassblock,(mtni,mtnj)))) ||
                            ( ((!prei,!prej) = (!mtni,!mtnj+1)) && (presence(water,(mtni,mtnj)))) ||
                            ( ( ((!prei,!prej) = (!mtni+1,!mtnj)) || ((!prei,!prej) = (!mtni-1,!mtnj)) ) && (presence(rampSud,(mtni,mtnj)))) ||
                            ( ((!prei,!prej) = (!mtni,!mtnj+1)) && (presence(rampNord,(mtni,mtnj)))) 
                                                                
		       )
                                                   then(  true)
                                                   else (print_string "\nTu ne peux pas passer par là ! cherche un autre chemin.\n\n\t";false))
    
    else if( presence(plainBlock,(prei,prej)) || presence(grassblock,(prei,prej)) || presence(water,(prei,prej)) )
         then( 
	   if(    
	           ( ((!prei,!prej) = (!mtni,!mtnj-1)) &&(presence(rampSud,(mtni,mtnj))))  ||
                   ( ((!prei,!prej) = (!mtni,!mtnj+1)) &&(presence(rampNord,(mtni,mtnj)))) || 
                   ( ((!prei,!prej) = (!mtni+1,!mtnj)) && (presence(rampWest,(mtni,mtnj)))) ||
                   ( ((!prei,!prej) = (!mtni-1,!mtnj)) && (presence(rampEst,(mtni,mtnj))))  ||
                   (presence(plainBlock,(mtni,mtnj))) ||
                   (presence(grassblock,(mtni,mtnj))) ||
                   (presence(water,(mtni,mtnj)))
           )
           then(  true)
           else (print_string "\nTu ne peux pas passer par là ! cherche un autre chemin.\n\n\t";false)
	 )

    else if ( sur_le_sol(prei,prej)) then ( 
              if( 
	         ((sur_le_sol (mtni,mtnj))) ||
                 (( (!prei,!prej) = (!mtni,!mtnj-1)) && presence(rampNord,(mtni,mtnj))) ||
		 (( (!prei,!prej) = (!mtni-1,!mtnj)) && presence(rampWest,(mtni,mtnj))) ||
                 (( (!prei,!prej) = (!mtni,!mtnj+1)) && presence(rampSud,(mtni,mtnj))) ||
		 (( (!prei,!prej) = (!mtni+1,!mtnj)) && presence(rampEst,(mtni,mtnj)))
	      )
	      then true
	      else (print_string "\nTu ne peux pas passer par là ! cherche un autre chemin.\n\n\t";false)
         )
      

    else true
  in

  let exp = ref 0
  
  in
  
  let avancer (ri,rj)  =
  	match direction with
	  
	| E -> if (!ri < 13) then (exp := !ri+1 ;
				   if ( verifier (ri,rj) (exp , rj) ) 
				   then ((ri := !ri + 1); incr bouger)) 
	  else ( exp := 0 ;
		 if (verifier (ri,rj) (exp , rj)) then ((ri := 0;incr bouger))
	  )
	| O -> if !ri > 0 then (exp := !ri-1 ;
				if ( verifier (ri,rj) (exp , rj) ) 
				then ((ri := !ri - 1);incr bouger))
	  else ( exp := 13 ;
		 if (verifier (ri,rj) (exp , rj)) then (ri := 13;incr bouger)
	  )
	| S -> if !rj < 13 then (exp := !rj+1 ;
				 if ( verifier (ri,rj) (ri , exp) ) 
				 then ((rj := !rj + 1);incr bouger)) 
	  else ( exp := 0 ;
		 if (verifier (ri,rj) (ri , exp)) then (rj := 0;incr bouger)
	  )
	| N -> if !rj > 0 then (exp := !rj - 1 ;
				if ( verifier (ri,rj) (ri , exp) )
				then( (rj := !rj - 1); incr bouger) )
	  else ( exp := 13 ;
		 if (verifier (ri,rj) (ri , exp)) then (rj := 13;incr bouger)
	  )
	    
  in

  
  let () = 
      if(!tour=0)then(
	avancer (kenny_i,kenny_j) 
      )
      else(
	avancer (cartman_i,cartman_j)

     )
	
  in
    
  let ramasser_etoile (ri,rj) =

    if (((!ri,!rj) = (!kenny_i,!kenny_j)) || ((!ri,!rj) = (!cartman_i,!cartman_j)) ) then(
      begin
	print_string "\nVous venez de manger une étoile !\n\nPour voir le score tapez score();; .\n\n\t";
        ri := -1;
        rj := 9;
	if (!tour=0) then augmenter_score scoreKenny
	else augmenter_score scorecartman
      end
    )
  in
  List.iter ramasser_etoile etoiles;

  
  let ramasser_lives (ri,rj) =
	if (((!ri,!rj) = (!kenny_i,!kenny_j)) || ((!ri,!rj) = (!cartman_i,!cartman_j)) ) then(
	  begin
	    print_string "\n\n\n\tLE COEUR ETAIT UN INDICE POUR LE PASSAGE SECRET !!!";
            ri := -1;
            rj := 9;
	  end
	)
  in
  List.iter ramasser_lives lives;


  let tomber_dans_water (ri,rj)=
    if( presence(water,(ri,rj))) 
    then (
      begin
	if(!tour=0)then(
	  if( presence(selector,(cartman_i,cartman_j))) then(
	    print_string "\n\nCartman est sur le téléporteur !\n te voilà à la case départ.\n\n\t";
	    ri := 0;
	    rj := 0;
	   
	  )
	  else(
	    print_string "\n\nBravo !!!\n\nVous avez trouvé le passage secret.\n\n\t";
	    ri := 12;
	    rj := 1;
	 
	  )
	)
	else (
	   if(  presence(selector,(kenny_i,kenny_j))) then(
	    print_string "\n\nCartman est sur le téléporteur !\n te voilà à la case départ.\n\n\t";
	    ri := 12;
	    rj := 12;
	  )
	  else(
	    print_string "\n\nBravo !!!\n\nVous avez trouvé le passage secret.\n\n\t";
	    ri := 12;
	    rj := 1;
	   
	  )
	)

      end
    )
  in
  tomber_dans_water (kenny_i,kenny_j);
  tomber_dans_water (cartman_i,cartman_j);

  let changer_tour letour = 
    if(!bouger =1) then (
      if (!letour =1)then(
	print_string "\n\n\n Le tour de Kenny.\n\n";
	decr bouger;
	decr letour ;
      )
      else (
	print_string "\n\n\n Le tour de Cartman.\n\n";
	decr bouger;
	incr letour;
      )
    )
    else  (
      if (!letour =1)then(
	print_string "\n\n\n C'est toujours le tour de Cartman.\n\n";
      )
      else (
	print_string "\n\n\n C'est toujours le tour de Kenny.\n\n";
      )
    )
      
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
print_string " ******   Tahar et Sofian   ******  \n";;
print_string " Crédit image : lostgarden.com et SouthPark\n";;
print_string "\n\n\n Kenny joue en premier.\n\n";;


afficher_mobiles ();;


