-- TP Arbre Matthieu Caron et Arnaud Cojez

import Test.QuickCheck
import Control.Concurrent (threadDelay)

main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"



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

--Q18 
elementR x Vide = False
elementR x (Noeud _ v ag ad)
  | x < v = elementR x ag
  | x > v = elementR x ad 
  | otherwise = True

--Q19
data Couleur = N | R
 
--Q20
é = equilibre
equilibre :: Arbre Couleur a -> Arbre Couleur a
equilibre Vide = Vide
equilibre (Noeud N vz (Noeud R vy (Noeud R vx a b) c) d) =
  Noeud R vy (Noeud N vx (equilibre a) (equilibre b)) (Noeud N vz (equilibre c) (equilibre d))
equilibre (Noeud N vz (Noeud R vx a (Noeud R vy b c)) d) =
  Noeud R vy (Noeud N vx (equilibre a) (equilibre b)) (Noeud N vz (equilibre c) (equilibre d))
equilibre (Noeud N vx a (Noeud R vz (Noeud R vy b c) d)) =
  Noeud R vy (Noeud N vx (equilibre a) (equilibre b)) (Noeud N vz (equilibre c) (equilibre d))
equilibre (Noeud N vx a (Noeud R vy b (Noeud R vz c d))) =
  Noeud R vy (Noeud N vx (equilibre a) (equilibre b)) (Noeud N vz (equilibre c) (equilibre d))
equilibre (Noeud c v g d) = Noeud c v (equilibre g) (equilibre d)

racineNoire (Noeud _ v g d) = Noeud N v g d
{-
insertion a x = racineNoire (insertion' a x)

insertion' :: (Ord a) => Arbre Couleur a -> a -> Arbre Couleur a
insertion' Vide a = Noeud R a Vide Vide
insertion' arbre@(Noeud c a gauche droite) x 
  | elementR x arbre = arbre
  | x < a            = equilibre (Noeud c a (insertion' gauche x) droite)
  | otherwise        = equilibre (Noeud c a gauche (insertion' droite x))
           -}      
insertion :: (Ord val) => val -> Arbre Couleur val -> Arbre Couleur val
insertion e a = racineNoire res
           where res = insertion' e a
                    where insertion' :: (Ord val) => val -> Arbre Couleur val -> Arbre Couleur val
                          insertion' elem Vide             = Noeud R elem Vide Vide
                          insertion' elem all@(Noeud c v g d) =   if elementR elem all
                                                                  then all
                                                                  else
                                                                    if elem <= v 
                                                                    then é (Noeud c v (insertion' elem g) d)
                                                                    else é (Noeud c v g (insertion' elem d))
      
arbresDot' :: (Ord a) => [a] -> Arbre Couleur a -> [Arbre Couleur a]
arbresDot' [] _ = []
arbresDot' (x:xs) arbre = ins : (arbresDot' xs ins)
                          where ins = (insertion x arbre)

couleur2String :: Couleur -> String
couleur2String    = \c -> case c of   N -> "[color=black, fontcolor=black]"
                                      R -> "[color=red, fontcolor=red]"
char2String :: Char -> String
char2String  = (: [])

arbresDot :: [Char] -> [String]
arbresDot xs = map (dotise "arbre" couleur2String char2String) (arbresDot' xs Vide) 









 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

