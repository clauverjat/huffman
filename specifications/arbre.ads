-- Spécification du type Arbre

package Arbre is

    type T_Arbre is private;

    Nil : constant T_Arbre;

    -- Créer un nouvel arbre sur un noeud avec un caractère et une fréquence donnée
    function Nouveau(Caractere: Character; Frequence: Integer; Gauche: T_Arbre; Droite: T_Arbre) return T_Arbre;

    -- Une nouvelle feuille
    -- Paramètres :
    --   Caractere : le caractere de la feuille
    --   Frequence : la fréquence du caractère
    -- Retourne : une nouvelle feuille de Huffman
    function Feuille(Caractere: Character; Frequence: Integer) return T_Arbre;

    -- L'arbre est-il vide?
    function Est_Vide(Arbre : in T_Arbre) return Boolean;

    -- Retourne vrai si Arbre possède un sous-arbre gauche
    function Existe_Arbre_Gauche(Arbre: in T_Arbre) return Boolean;

    -- Retourne vrai si Arbre possède un sous-arbre droit
    function Existe_Arbre_Droit(Arbre: in T_Arbre) return Boolean;

    -- Crée un sous-arbre gauche et le renvoie
    -- Attention fonction avec effet de bord !
    function Creer_Gauche(Arbre: in T_Arbre) return T_Arbre
    with Pre => not Est_Vide(Arbre);

    -- Crée un sous-arbre droit et le renvoie
    -- Attention fonction avec effet de bord!
    function Creer_Droite(Arbre: in T_Arbre) return T_Arbre
    with Pre => not Est_Vide(Arbre);

    -- Sous arbre gauche de Arbre
    function Sous_Arbre_Gauche(Arbre: in T_Arbre) return T_Arbre
    with Pre => not Est_Vide(Arbre);

    -- Sous arbre droite de Arbre
    function Sous_Arbre_Droit(Arbre: in T_Arbre) return T_Arbre
    with Pre => not Est_Vide(Arbre);

    -- Retourne la fréquence du noeud ou de la feuille pointée par Arbre
    -- NB : La fréquence de l'arbre vide est 0
    function Frequence(Arbre: in T_Arbre) return Integer;

    -- Caractère contenue par la feuille concernée
    function Caractere(Arbre: in T_Arbre) return Character
    with Pre => Est_Feuille(Arbre);

    -- Remplacer le caractère de la feuille
    procedure Modifier_Caractere(Arbre: in T_Arbre; c: Character)
    with Pre => not Est_Vide(Arbre) and then Est_Feuille(Arbre);

    -- Mettre à jour le noeud considéré
    procedure Modifier_Frequence(Arbre: in T_Arbre; Frequence : Integer)
    with Pre => not Est_Vide(Arbre);

    -- L'arbre est-il réduit à une feuille ?
    function Est_Feuille(Arbre : in T_Arbre) return Boolean
    with Pre => not Est_Vide(Arbre);

    -- Taille de l'arbre (nombre de feuilles)
    -- Assure : Taille(Arbre) >= 0
    function Taille(Arbre : T_Arbre) return Integer
    with Post => Taille'Result >=0;

    -- Egalité sur les pointeurs
    function Builtin_Equal (Left, Right : T_Arbre) return Boolean renames "=";

    -- Surcharge sémantique de la relation d'égalité entre deux arbres
    function "="(Arbre1 : in T_Arbre; Arbre2 : in T_Arbre) return Boolean;

    -- Regrouper les deux arbres dans un arbre de fréquence somme.
    -- Pré-condition :
    --     Les arbres ne sont pas vides
    --     et la fréquence de l'arbre gauche est <= à la fréquence de l'arbre droit
    function Fusion(Arbre1: in T_Arbre ; Arbre2: in T_Arbre) return T_Arbre
    with Pre => ( not Est_Vide(Arbre1) and not Est_Vide(Arbre2) )
	    and then Frequence(Arbre1) <= Frequence(Arbre2);
    -- Afficher l'arbre de Huffman
    -- Exemple d'affichage :
    --   (13)
    --     \−−0−− (5)
    --              \−−0−− (2)
    --                       \−−0−− (1) ’n’
    --                       \−−1−− (1) ’t’
    --              \−−1−− (3) ’e ’
    --     \−−1−− (8)
    --              \−−0−− (4)
    --                       \−−0−− (2) ’r’
    --                       \−−1−− (4) ’o’
    --              \−−1−− (4)
    --                       \−−0−− (2) ’m’
    --                       \−−1−− (2)
    --                                \−−0−− (1) ’j’
    --                                \−−1−− (1) ’ ’
    procedure Afficher(Arbre : in T_Arbre);

    -- Detruire l'arbre
    procedure Detruire(Arbre : in out T_Arbre);

private
    type T_Noeud is record
	Caractere: Character;
	Frequence: Integer;
	Gauche: T_Arbre;
	Droite: T_Arbre;
	-- Invariant :
	--     { Frequence >= 0 }
    end record;

    type T_Arbre is access T_Noeud;

    Nil : constant T_Arbre := Null;
end Arbre;
