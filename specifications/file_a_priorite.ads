-- Spécification du module générique File_A_Priorite.

generic
    -- Nombre maximal d'éléments que la file peut contenir
    Capacite : Integer; -- Capacite >= 0
    -- Type des éléments de la file
    type T_Element is private;
    -- Relation d'ordre stricte pour comparer les éléments
    with function "<"(a : in T_Element; b : in T_Element) return Boolean;

package File_A_Priorite is
    type T_File_A_Priorite is limited private;

    -- Exceptions :
    -- 	File_Vide_Error : la file est vide
    --  File_Pleine_Error : la file est pleine

    File_Vide_Error, File_Pleine_Error : exception;

    -- Initialiser la file
    -- Paramètres :
    --   File: T_File_A_Priorite
    -- Assure : Taille(File) = 0
    -- Complexité en temps garantie : O(1)
    procedure Initialiser (File : out T_File_A_Priorite) with
	    Post => Taille(File) = 0;

    -- Récupèrer le plus petit élément
    -- Paramètres :
    --   File: T_File_A_Priorite
    -- Exception :
    -- 	File_Vide_Error : la file est vide
    -- Complexité en temps garantie : O(1)
    function Sommet (File: in T_File_A_Priorite) return T_Element;

    -- Ajouter un élément dans la file
    -- Paramètres :
    --   File: T_File_A_Priorite
    --   Element : T_Element : l'élément à enfiler
    -- Exception :
    --  File_Pleine_Error : lorsque la file est pleine,
    --                      (l'élément ne peut pas être enfilé)
    -- Complexité en temps garantie : O(log Taille)
    procedure Enfiler (File : in out T_File_A_Priorite; Element: in T_Element) with
	    Post => Taille(File)'Old + 1 = Taille(File);

    -- Retirer le plus petit élément
    -- Paramètres :
    --    File : T_File_A_Priorite
    -- Assure : la taille de la file est décrémentée
    -- Exceptions :
    --  File_Vide_Error : la file est vide
    -- Complexité en temps garantie : O(log Taille)
    procedure Extraire(File : in out T_File_A_Priorite) with
	    Post => Taille(File)'Old - 1 = Taille(File);

    -- Le nombre d'élements dans la file
    -- Paramètres :
    --    File : T_File_A_Priorite
    -- Complexité en temps garantie : O(1)
    function Taille(File : in T_File_A_Priorite) return Integer with
	    Post => Taille'Result >= 0;

private
    type T_Tableau is array(1..Capacite) of T_Element;
    type T_File_A_Priorite is
        record
            Elements : T_Tableau;   -- Les éléments de la file
	    Taille: Integer;        -- Nombre d'éléments dans la file
	    -- Invariant :
            --   0 <= Taille <= Capacite
        end record;
end File_A_Priorite;
