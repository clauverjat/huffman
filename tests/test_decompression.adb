with Decompression; use Decompression;
with Arbre; use Arbre;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;  use Ada.Directories;
with byte; use byte;

procedure Test_Decompression is
    Fichier: Bin_IO.File_Type;
    Taille_Decompresse: Integer;
    Arbre_Huffman: T_Arbre;
    Fichier_Sortie: Bin_IO.File_Type;

    procedure Comparer_Fichers is
        Fichier_Sortie_Comparaison: Bin_IO.File_Type;
        Fichier_Comparaison: Bin_IO.File_Type;
        char1: T_Byte;
        char2: T_Byte;
    begin
        Bin_IO.Open(Fichier_Sortie_Comparaison, Bin_IO.In_File, "tests/wikipedia.txt");
        Bin_IO.Open(Fichier_Comparaison, Bin_IO.In_File, "tests/wikipedia-ref.txt");
        for i in 1..File_Size'Max(Size("tests/wikipedia.txt"), Size("tests/wikipedia-ref.txt")) loop
            Bin_IO.Read(Fichier_Comparaison, char1);
            Bin_IO.Read(Fichier_Sortie_Comparaison, char2);
            pragma assert(char1 = char2);
        end loop;
        Bin_IO.Close(Fichier_Comparaison);
        Bin_IO.Close(Fichier_Sortie_Comparaison);
    end Comparer_Fichers;

begin
    -- Extrait wikipedia.txt.hf
    Bin_IO.Open(Fichier, Bin_IO.In_File, "tests/wikipedia.txt.hf");
    Bin_IO.Create(Fichier_Sortie, Bin_IO.Out_File, "tests/wikipedia.txt");
    Lire_Metadonnees(Fichier, Taille_Decompresse, Arbre_Huffman);
    Extraction(Fichier_Sortie, Fichier, Taille_Decompresse, Arbre_Huffman);
    Bin_IO.Close(Fichier_Sortie);
    Detruire(Arbre_Huffman);
    Bin_IO.Close(Fichier);

    -- Vérifie que le fichier extrait est correct
    Comparer_Fichers;
end Test_Decompression;
