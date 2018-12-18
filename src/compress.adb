with Compression;
use Compression;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_line; use Ada.Command_line;

-- Programme de compression.
procedure Compress is
    Fichier : Char_IO.File_Type;
    Sortie : Bin_IO.File_Type;
begin
    -- Affichage des arguments de la ligne de commande
    New_Line;
    Put("---- Arguments de la ligne de commande : ");
    Put(Argument_Count);
    Put_Line(" arguments.");
    for i in 1 .. Argument_Count loop
	Put("Argument");
	Put(i);
	Put(" : ");
	Put_Line(Argument(i));
    end loop;
    New_Line(2);
    if Argument_Count >=1 then
	-- Nom du fichier donne dans le premier argument. Argument(1);
	begin
	    -- Ouverture du fichier à compresser en lecture seule
	    Char_IO.Open(Fichier, Char_IO.In_File, Argument(1));
	    -- Création du fichier
	    Bin_IO.Create(Sortie, Bin_IO.Out_File, Argument(1)&".hf");

	    -- Affichage du texte contenu dans le fichier en argument 1.
	    Put_Line("---- Compression du fichier : "&Argument(1));
	    Compresser(Fichier, Sortie);

	    -- Fermer les fichiers ouverts
	    Char_IO.Close(Fichier);
	    Bin_IO.Close(Sortie);
	    Put_Line("---- Fichier compressé : "&Argument(1)&".hf");
	exception
	    when ADA.IO_EXCEPTIONS.NAME_ERROR =>
		put_line("Fichier non present - donner le nom d'un fichier en argument");
	end;
    end if;
end Compress;
