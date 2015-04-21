Hakmi Sofian, Tahar Si Ahmed

Le programme ce trouve dans astro.ml


Notre jeu, est un jeu, qui permet à deux joueurs de disputer une partie commune,
chacun doit ramasser un nombre maximal d'etoiles, pour gagner la partie.

Au total, les deux joueurs, doivent disputer les 11 etoiles présentes sur le terrain, 
réparties, d'une maniere  équilibrée et qui ne favorise pas un des deux joueurs.

Les deux joueurs sont kenny et cartman, chacun joue a tour de role, s'il essaye d'effectuer
un mouvement impossible, il garde son tour jusqu'a qu'il reussisse a se deplacer.

Apart l'imppossibilte de se deplacer, tout les mouvements alternent le tours.

Dans le jeu, les deplacements sont reglés de sorte que, quand le joueur X est sur une case, 
le joueur Y ne peux pas y etre au meme temps.

Un joueur ne peut faire que des déplacement logique. exemple être sur un bloc et vouloir 
descendre sans passer par une ramp ceci n'est pas logique.

Sur le terrain on a pu mettre une carré fermé par des rochers, et qui ne permettent pas 
aux joueurs de les traverser, et qui contient des etoiles  , donc le joueur reussisssant 
a y parvenir,aura 5 etoiles d'avance, voir gagner la partie.

pour acceder au carré precedent, il exite un passage secret, et unique, que le joueur 
doit emprunter grace a un teleporteur qui aura le bout du tunnel dans le carré en question.

quand un joueur est sur le téléporteur l'autre joueur se téléporte à sa case départ respective.
s'il reussi a emprunter le passage secret, dans ce cas, il peut jouir du privilège des 
étoiles secrète. 

le joueur ne peut passer du sol a un block plein que en empruntant une rampe ascendante

le joueur ne peut aborder une rampe par les cote, elle est accessible que dans un sens.

une fois, sur un block plein ou sur une rampe, on ne peut descendre qu'en passant par 
une rampe descendante.










voilà la ligne de commande pour compiller le jeu:

ocamlmktop directions.cmo interprete.cmo graphics.cma images50.cmo directions.ml interprete.ml images50.ml dessiner.ml astro.ml -o FlappyCandyAstroCrush

puis :

open Astro;;
open Interprete;;


et pour jouer :
deplacement :
jouer Est / Ouest / Sud / Nord;;
voir le score du jeu :
score();;

