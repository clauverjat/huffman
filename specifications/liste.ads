-- Ce module définit un type T_Liste

generic
    type T_Element is private;
package Liste is
    -- Le type est public
    type T_Cellule;
    type T_Liste is access T_Cellule;
    type T_Cellule is
	record
	    Element : T_Element;
	    Suivant : T_Liste;
	end record;

    function Builtin_Equal (Left, Right : T_Liste) return Boolean renames "=";

    Liste_Vide_Error : exception;

    -- Initialiser une liste
    procedure Initialiser (Liste : out T_Liste) with
	    Post => Est_Vide (Liste);

    -- Détruire la liste
    procedure Detruire (Liste: in out T_Liste);

    -- Est-ce que la liste est vide ?
    function Est_Vide (Liste : in T_Liste) return Boolean;

    -- Récupérer la tête de la liste
    -- Exception :
    --   Liste_Vide_Error : levée si la liste est vide
    function Tete (Liste : in T_Liste) return T_Element;

    -- Ajouter un élément en tête à la liste
    procedure Ajouter(Liste : in out T_Liste; Element : in T_Element) with
	    Post => Tete(Liste) = Element;

    -- Enlever la tête de la liste
    -- Exception :
    --   Liste_Vide_Error : lever si la liste est vide
    procedure Enlever_Tete(Liste : in out T_Liste);

    -- Une copie de la liste renversée
    function Liste_Renverse(Liste : in T_Liste) return T_Liste;

    generic
	with procedure Put(Element : T_Element);
    -- Afficher une liste
    procedure Afficher(Liste : in T_Liste);

    -- Relation d'égalité entre deux listes
    function "="(Liste1 : in T_Liste; Liste2 : in T_Liste) return Boolean;

end Liste;
