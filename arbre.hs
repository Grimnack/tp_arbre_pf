-- TP Arbre Matthieu Caron et Arnaud Cojez

import Test.QuickCheck

data Arbre coul val = Vide | Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show

mapArbre _  Vide                         = Vide
mapArbre f (Noeud coul val gauche droit) = Noeud (fst (f coul val)) (snd (f coul val)) (mapArbre f gauche) (mapArbre f droit)

foldArbre :: (couleur -> a -> b -> b -> b) -> b -> Arbre couleur a -> b
foldArbre _ v  Vide                         = v
foldArbre f v (Noeud coul val gauche droit) = f coul val (foldArbre f v gauche) (foldArbre f v droit) 

--Petit Arbre de test 
test = Noeud "rouge" 1 (Noeud "rouge" 2 (Noeud "rouge" 4 Vide Vide) Vide) (Noeud "rouge" 3 Vide Vide)
test'= Noeud "rouge" 1 (Noeud "rouge" 2 Vide Vide) (Noeud "rouge" 3 Vide Vide)


--Version non terminale de taille 
taille  Vide                    = 0
taille (Noeud _ _ gauche droit) = 1 + taille gauche + taille droit
	 
--Version non terminale de hauteur
hauteur  Vide                    = 0
hauteur (Noeud _ _ gauche droit) = max ((hauteur gauche)+1,(hauteur droit)+1)
										where max (x,y) = if x < y then y else x
										
										
--maintenant avec fold/map plus tard :3	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--taille' = foldlArbre (\_->(+) 1) 0 


peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche []              = Vide
peigneGauche ((coul,val):xs) = Noeud coul val (peigneGauche xs) Vide

--Question 5

prop_hauteurPeign xs = length xs == hauteur (peigneGauche xs)

--Question 6 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


--Question 7
estComplet :: Arbre c a -> Bool
estComplet  Vide                    = True
estComplet (Noeud _ _ gauche droit) = if taille gauche == taille droit then
								      	(estComplet gauche) && 
									  	(estComplet droit)   
									  else 
									  	False

--Question 8
--Les peignes à gauche complet est l'abre de hauteur 1.

--Question 9
 
complet' :: Int -> [(c, a)] -> (Arbre c a,[(c, a)]) 
complet' 0 xs  = (Vide, xs)
complet' n xs  = (Noeud c v gauche droite , zs)
                  where (gauche, r1) = complet' (n-1) xs
                        (c,v)        = head r1
                        (droite, zs) = complet' (n-1) (tail r1)   
 
complet :: Int -> [(c, a)] -> Arbre c a
complet n xs = fst (complet' n xs)


--Question 10
infini :: a -> [a]
infini x = iterate id x
 
--Question 11
creeListeCouple :: [((), Char)]
creeListeCouple = map (\x -> ((),x)) ['a'..]
 
--Question 12
aplatit :: Arbre c a -> [(c, a)]
aplatit Vide = []
aplatit (Noeud c a gauche droite) = aplatit gauche ++ [(c,a)] ++ aplatit droite
 
--Question 13
element :: Eq a => a -> Arbre c a -> Bool
element _ Vide = False
element a (Noeud _ b gauche droite)= if a == b then
                                       True
                                     else
                                       (element a gauche) || (element a droite)

--Question 14
noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud fcol fval (c,v) = fval v ++ fcol c 


noeuds :: (c -> String) -> (a -> String) -> Arbre c a -> String
noeuds _ _ Vide = ""
noeuds fcol fval (Noeud c a gauche droite) = (noeud fcol fval (c,a))++"\n"++
                                             (noeuds fcol fval gauche)++
                                             (noeuds fcol fval droite)


--Question 15
arcs :: Arbre c a -> [(a,a)]
arcs  Vide                     = []
arcs (Noeud _ a Vide Vide)     = []
arcs (Noeud _ a gauche Vide)   = [(a,b)] ++ arcs gauche 
                                 where (Noeud _ b _ _ ) = gauche
arcs (Noeud _ a Vide droite)   = [(a,b)] ++ arcs droite 
                                 where (Noeud _ b _ _ ) = droite
arcs (Noeud _ a gauche droite) = [(a,b)] ++ [(a,c)] ++ arcs gauche 
                                 where (Noeud _ b _ _ ) = gauche
                                       (Noeud _ c _ _ ) = droite
                                     
                                     
arc :: (a -> String) -> (a,a) -> String
arc farc (a,b) = farc a ++ " -> " ++ farc b ++ "\n"

showArc ::  (a -> String) -> [(a,a)] -> String
showArc _ [] = ""
showArc farc ((a,b):xs) = (arc farc (a,b)) ++ (showArc farc xs)

dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise nomArbre fcol fval arbre = "digraph \""++nomArbre++"\" { \n   node [fontname=\"DejaVu-Sans\", shape=circle]\n" ++(noeuds fcol fval arbre)++ "\n" ++ (showArc fval (arcs arbre)) ++ "}"

























 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

