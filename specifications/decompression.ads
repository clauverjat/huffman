with Arbre; use Arbre;
with Ada.Sequential_IO;
with Byte; use Byte;

package Decompression is 
    package Bin_IO is new Ada.Sequential_IO(t_byte);

    NB_CARACTERES_POSSIBLES: Integer := 256;
    type Tab_Caracteres is array (1..NB_CARACTERES_POSSIBLES) of t_byte;

    -- Lire les métadonnées contenues dans le fichier et créer l'arbre associé
    -- Arguments:
    --   Fichier: le fichier binaire qui contient les données
    --   Taille_Decompresse: la taille du texte à créer
    --   Arbre_Huffman: l'arbre généré à partir des métadonnées stockées dans le fichier
    procedure Lire_Metadonnees(Fichier: in Bin_IO.File_Type; Taille_Decompresse: out Integer; Arbre_Huffman: out T_Arbre);

    -- Créer la structure de l'arbre et y placer les caractères récursivement
    -- Arguments:
    --   Arbre_Huffman: l'arbre généré
    --   Fichier: le fichier binaire qui contient les données
    --   Caracteres_Utilises: tableau des caractères à ajouter dans l'arbre (nous ne sommes intéressés que par les valeurs comprises entre 1 et Nombre_Caracteres_Utilises)
    --   Nombre_Caracteres_Utilises: le nombre de caractères à ajouter dans l'arbre
    --   Position_Caractere: la position du caractère courant dans le tableau
    procedure Construire_Structure(Arbre_Huffman: in T_Arbre; Fichier: in Bin_IO.File_Type; Caracteres_Utilises: in Tab_Caracteres; Nombre_Caracteres_Utilises: in Integer; Position_Caractere: in out Integer);

    -- Décompresser le fichier dans Fichier_Sortie
    -- Arguments:
    --   Fichier_Sortie: le fichier dans lequel les données décompressées vont être écrites
    --   Fichier: le fichier qui contient les données binaires, positionné au début des données à décompresser
    --   Taille_Decompresse: le nombre de caractères à extraire
    --   Arbre_Huffman: l'arbre qui permet la décompression
    procedure Extraction(Fichier_Sortie: in out Bin_IO.File_Type; Fichier: in Bin_IO.File_Type; Taille_Decompresse: in Integer; Arbre_Huffman: in T_Arbre);
end Decompression;
