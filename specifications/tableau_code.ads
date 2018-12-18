with Byte;
use Byte;

package Tableau_Code is
    type T_Tableau_Binaire is array(Character) of Liste_Byte.T_Liste;

    -- Detruire la table en libérant la mémoire occupée par les listes
    procedure Detruire(Table : in out T_Tableau_Binaire);

    -- Afficher une table de Huffman
    -- Exemple d'affichage:
    -- ’␣’ −> 1111
    -- ’e’ −> 01
    -- ’j’ −> 1110
    -- ’m’ −> 110
    -- ’n’ -> 000
    -- ’o’ −> 101
    -- ’r’ -> 100
    -- ’t’ −> 001
    --
    -- Paramètre :
    --    Table_Huffman : la table de Huffman à afficher
    procedure Afficher(Table_Huffman : T_Tableau_Binaire);
end Tableau_Code;
