with Ada.Command_line; use Ada.Command_line;
with Decompression; use Decompression;
with Arbre; use Arbre;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;

procedure Decompress is
    Fichier: Bin_IO.File_Type;
    Taille_Decompresse: Integer;
    Arbre_Huffman: T_Arbre;
    Fichier_Sortie: Bin_IO.File_Type;
begin
    If Argument_Count < 1 then
        Put_Line("Nombre d'arguments invalide !");
    else
        -- Ouvre le fichier à compresser
        Bin_IO.Open(Fichier, Bin_IO.In_File, Argument(1));
        -- Ouvre le fichier privé de l'extension ".hf"
        Bin_IO.Create(Fichier_Sortie, Bin_IO.Out_File, Argument(1)(1..Argument(1)'Length-3));
        Lire_Metadonnees(Fichier, Taille_Decompresse, Arbre_Huffman);
        Extraction(Fichier_Sortie, Fichier, Taille_Decompresse, Arbre_Huffman);
        Detruire(Arbre_Huffman);
        Bin_IO.Close(Fichier_Sortie);
        Bin_IO.Close(Fichier);
    end if;

    exception
        when Ada.IO_Exceptions.Name_Error =>
            Put_Line("Le fichier n'existe pas ! Abandon...");
        when Ada.IO_Exceptions.End_Error =>
            Put_Line("Fin prématurée du fichier, l'opération a échouée");
end Decompress;
