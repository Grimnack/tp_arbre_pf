-- TP Arbre Matthieu Caron et Arnaud Cojez

import Test.QuickCheck

data Arbre coul val = Vide | Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show

mapArbre _ Vide = Vide
mapArbre f (Noeud coul val gauche droit) = Noeud (fst (f coul val)) (snd (f coul val)) (mapArbre f gauche) (mapArbre f droit)

foldArbre :: (couleur -> a -> b -> b -> b) -> b -> Arbre couleur a -> b
foldArbre _ v Vide = v
foldArbre f v (Noeud coul val gauche droit) = f coul val (foldArbre f v gauche) (foldArbre f v droit) 

--Petit Arbre de test 
test = Noeud "rouge" 1 (Noeud "rouge" 2 (Noeud "rouge" 4 Vide Vide) Vide) (Noeud "rouge" 3 Vide Vide)
test'= Noeud "rouge" 1 (Noeud "rouge" 2 Vide Vide) (Noeud "rouge" 3 Vide Vide)


--Version non terminale de taille 
taille Vide = 0
taille (Noeud _ _ gauche droit) = 1 + taille gauche + taille droit
	 
--Version non terminale de hauteur
hauteur Vide = 0
hauteur (Noeud _ _ gauche droit) = max ((hauteur gauche)+1,(hauteur droit)+1)
										where max (x,y) = if x < y then y else x
										
										
--maintenant avec fold/map plus tard :3	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--taille' = foldlArbre (\_->(+) 1) 0 


peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Vide
peigneGauche ((coul,val):xs) = Noeud coul val (peigneGauche xs) Vide

--Question 5

prop_hauteurPeign xs = length xs == hauteur (peigneGauche xs)

--Question 6 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


--Question 7
-- FAUX !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estComplet :: Arbre c a -> Bool
estComplet Vide = True
estComplet (Noeud _ _ Vide Vide) = True
estComplet (Noeud _ _ Vide _) = False
estComplet (Noeud _ _ _ Vide) = False
estComplet (Noeud _ _ gauche droit) = (estComplet gauche) && (estComplet droit)




--Question 8
--Les peignes à gauche complet est l'abre de hauteur 1.

--fonction intermediaire prenant une liste et une hauteur est renvoie la premiere moitié 
-- de la liste - 1 elmt



premiere :: Int -> [(a,b)] -> [(a,b)]
premiere h xs =  take ((2^(h-1))-1) xs

seconde :: Int  -> [(a,b)] -> [(a,b)]
seconde h xs = reverse (take ((2^(h-1))-1) (reverse xs))

milieu :: Int -> [(a,b)] -> (a,b)
milieu h xs = (!!) xs ((2^(h-1))-1)


complet :: Int -> [(c, a)] -> Arbre c a
complet 0 _ = Vide
complet n xs = Noeud (fst (milieu n xs)) (snd (milieu n xs)) (complet (premiere n xs)) (complet (seconde n xs)) 
 





























































