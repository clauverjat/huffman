-- Ce module permet de compresser des fichiers

with Ada.Sequential_IO;

with Byte; use Byte;use Byte.Liste_Byte;
with Tableau_Code; use Tableau_Code;
with Arbre; use Arbre;

package Compression is
    package Bin_IO is new Ada.Sequential_IO(T_Byte);
    package Char_IO is new Ada.Sequential_IO (Character); use Char_IO;
    type T_Tableau_Entier is array(Character) of Integer;

    -- Compresser un fichier
    -- Paramètres :
    --   Fichier : Descripteur de fichier du fichier à compresser
    --   Sortie  : Descripteur de fichier de la sortie
    --   Verbose (facultatif) : Affiche l'arbre de Huffman et la table de Huffman
    procedure Compresser (Fichier : in out Char_IO.File_Type; Sortie: in out Bin_IO.File_Type ; Verbose : in Boolean := False);

    -- Donner le nombre d'occurences des caractères dans le fichier à compresser
    -- Paramètre :
    --   Fichier : Descripteur de fichier du fichier à compresser
    -- Précondition : Le curseur est placé sur le début du fichier
    -- Post-condition : Le curseur est placé sur le début du fichier
    function Frequences_Caracteres(Fichier : in out Char_IO.File_Type) return T_Tableau_Entier;

    -- L'arbre de Huffman
    -- Paramètre :
    --   Fichier : Tableau des fréquences
    -- Retourne : un arbre de Huffman
    function Arbre_Huffman(Frequence : in T_Tableau_Entier) return T_Arbre;

    --  Table de Huffman
    -- Paramètre :
    --    Arbre : T_Arbre : un arbre de Huffman du fichier à compresser
    -- Retourne :
    --     T_Tableau_Binaire le code de chaque caractère
    function Table_Huffman(Arbre : in T_Arbre) return Tableau_Code.T_Tableau_Binaire;

    -- Coder le fichier
    -- Paramètres :
    --   Fichier : Descripteur de fichier du fichier à compresser
    --   Sortie  : Descripteur de fichier du fichier de sortie
    --   Table   : Table de Huffman

    procedure Coder(Fichier : in out Char_IO.File_Type;
		    Sortie : in out Bin_IO.File_Type;
		    Arbre : in T_Arbre ;
		    Table: in Tableau_Code.T_Tableau_Binaire);

    -- Ecrire l'entête
    -- Paramètres :
    --    Sortie  : Descripteur de fichier du fichier de sortie
    --    Arbre : T_Arbre : un arbre de Huffman du fichier à compresser
    --    Table : Table de Huffman
    procedure Ecrire_Entete(Sortie : in out Bin_IO.File_Type;
			    Arbre : in T_Arbre);

    -- Coder le message
    -- Paramètres :
    --   Fichier : Descripteur de fichier du fichier à compresser
    --   Sortie  : Descripteur de fichier du fichier de sortie
    --   Table   : Les codes de chaque caractère
    procedure Coder_Message(Fichier : in out Char_IO.File_Type;
			    Sortie : in out Bin_IO.File_Type;
			    Table: in Tableau_Code.T_Tableau_Binaire);

    private

end Compression;
