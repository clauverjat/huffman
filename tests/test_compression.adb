with Compression; use Compression;
use Compression.Bin_IO;
with Arbre; use Arbre;
with Byte; use Byte; use Byte.Liste_Byte;
with Tableau_Code; use Tableau_Code;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Assertions;
use Ada.Assertions;

procedure Test_Compression is

    Fichier : Char_IO.File_Type;
    Arbre_Reference : T_Arbre;

    procedure Comparer_Fichers(Reference : in out Bin_IO.File_Type ; Sortie : in out Bin_IO.File_Type)
    is
	Octet_Ref, Octet_Sortie : T_Byte;
    begin
	while not End_Of_File(Reference) and not End_Of_File(Sortie) loop
	    Bin_IO.read(Reference, Octet_Ref);
	    Bin_IO.read(Sortie, Octet_Sortie);
	    pragma Assert( Octet_Ref = Octet_Sortie);
	end loop;
	pragma Assert(End_Of_File(Reference) and End_Of_File(Sortie));
	Reset(Reference);
	Reset(Sortie);
    end Comparer_Fichers;

    procedure Tester_Frequences_Caracteres is
	Frequences : T_Tableau_Entier;
	Frequences_Reference : T_Tableau_Entier;
    begin
	Frequences_Reference := (' ' => 1,
			  'n' => 1,
			  'e' => 3,
			  'o' => 2,
			  't' => 1,
			  'j' => 1,
			  'm' => 2,
			  'r' => 2,
			  others => 0);
	Frequences := Frequences_Caracteres(Fichier);
	pragma Assert(Frequences = Frequences_Reference);
    end Tester_Frequences_Caracteres;

    -- Longueur pondéré du code préfixe
    -- Calcule L(Code) à partir de l'arbre de Huffman
    -- L(Code) = Somme (sur les caractères) des Frequences(Caractere) * Nb_Bits(Caractere)
    function Longueur_Code(Arbre : in T_Arbre; Frequences : in T_Tableau_Entier) return Integer is
	function Longueur_Code_Recursif(Arbre : in T_Arbre; Profondeur : in Integer) return Integer is
	begin
	    if Est_Feuille(Arbre) then
		return Frequences(Caractere(Arbre)) * Profondeur;
	    else
		return  Longueur_Code_Recursif(Sous_Arbre_Gauche(Arbre), Profondeur + 1)
			+ Longueur_Code_Recursif(Sous_Arbre_Droit(Arbre), Profondeur + 1);
	    end if;
	end Longueur_Code_Recursif;

    begin
	return Longueur_Code_Recursif(Arbre, 0);
    end Longueur_Code;

    procedure Tester_Arbre_Huffman is
	Arbre_Resultat : T_Arbre;
	Frequences : T_Tableau_Entier;
    begin
	Frequences := Frequences_Caracteres(Fichier);
	Arbre_Resultat := Arbre_Huffman(Frequences);
	-- Assurer de l'optimalité du code généré
        -- Invariant pour tous les arbres de Huffman valides du texte donné
	pragma Assert(Longueur_Code(Arbre_Resultat, Frequences) = 38);
	Put_Line("Arbre optimal :  OK");
	-- Sauf modification du code l'arbre résultat devrait être égal
	-- à l'arbre de référence.
	-- Toutefois il existe plusieurs arbres de Huffman pour un même texte
	-- car il n'y a pas unicité.
	pragma Assert(Arbre_Reference = Arbre_Resultat);
	Put_Line("Arbre généré identique à la référence : OK");
	-- Detruire les arbres construits
	Detruire(Arbre_Resultat);

	-- Assurer que l'arbre de Huffman lorsqu'il n'y a aucun caractère est bien vide.
	pragma Assert(Arbre_Huffman((others =>0)) = Nil);
    end Tester_Arbre_Huffman;

    procedure Tester_Table_Huffman is
	Table_Resultat, Table_Reference : T_Tableau_Binaire;

	-- Convertir une chaine de caractère en code binaire
	function Liste(Chaine : String) return T_Liste is
	    Code : T_Liste;
	begin
	    Initialiser (Code);
	    for i in reverse 1..Chaine'Length loop
		case Chaine(i) is
		when '0' => Ajouter(Code, 0);
		when '1' => Ajouter(Code, 1);
		when others => raise Assertion_Error;
		end case;
	    end loop;
	    return Code;
	end Liste;

    begin

	Table_Resultat := Table_Huffman(Arbre_Reference);
	Table_Reference := (
		     ' ' => Liste("000"),
		     'n' => Liste("001"),
		     'e' => Liste("01"),
		     'o' => Liste("100"),
		     't' => Liste("1010"),
		     'j' => Liste("1011"),
		     'm' => Liste("110"),
		     'r' => Liste("111"),
		     others => Null);

	for c in Table_Reference'Range loop
	    pragma Assert (Table_Resultat(c) = Table_Reference(c));
	end loop;
	Detruire(Table_Resultat);
	Detruire(Table_Reference);
    end Tester_Table_Huffman;

    procedure Tester_Compresser(Nom_Fichier: String) is
	Entree : Char_IO.File_Type;
	Sortie : Bin_IO.File_Type;
	Reference: Bin_IO.File_Type;
    begin
	-- Création du fichier compressé
	Char_IO.Open(Entree, Char_IO.In_File, "tests/"&Nom_Fichier&".txt");
	Bin_IO.Create(Sortie, Bin_IO. Out_File, "tests/"&Nom_Fichier&".txt.hf");

	Compresser(Entree,Sortie,True);
	Close(Sortie);

	Bin_IO.Open(Sortie, Bin_IO.In_File, "tests/"&Nom_Fichier&".txt.hf");
	Bin_IO.Open(Reference, Bin_IO.In_File, "tests/"&Nom_Fichier&"_reference.txt.hf");
	Comparer_Fichers(Reference,Sortie);
	Bin_IO.Close(Reference);
	Bin_IO.Close(Sortie);
    exception
    when ADA.IO_EXCEPTIONS.NAME_ERROR =>
	Put_Line("Fichier de reference non present - fin du test!");
    end Tester_Compresser;


begin
    -- Tests unitaires :
    -- Ouverture du fichier à compresser en lecture seule
    Char_IO.Open(Fichier, Char_IO.In_File, "tests/test_compression.txt");

    Put_Line("Tester Frequences_Caracteres ");
    Tester_Frequences_Caracteres;

    Arbre_Reference :=
	    Nouveau('#',13,
	     Gauche => Nouveau('#', 5,
		     Gauche => Nouveau('#',2,
			     Gauche => Feuille(' ',1),
			     Droite => Feuille('n',1)),
		     Droite => Feuille('e',3)),
	     Droite => Nouveau('#',8,
		     Gauche => Nouveau('#',4,
			     Gauche => Feuille('o',2),
			     Droite => Nouveau('#',2,
				     Gauche => Feuille('t',1),
				     Droite => Feuille('j',1))),

		     Droite => Nouveau('#',4,
			     Gauche => Feuille('m',2),
			     Droite => Feuille('r',2))));
    Put_Line("Tester Arbre_Huffman ");
    Tester_Arbre_Huffman;

    Put_Line("Tester Table_Huffman ");
    Tester_Table_Huffman;

    Detruire(Arbre_Reference);
    -- Fermeture du fichier
    Char_IO.Close(Fichier);

    -- Tests fonctionnels sur des cas particuliers :

    Put_Line("Tester la compression sur 'jerome ermont' ");
    Tester_Compresser("test_compression");

    Put_Line("Tester la compression sur 'oooooooo' ");
    Tester_Compresser("test_compression_mono");

    Put_Line("Tester la compression sur le fichier vide");
    Tester_Compresser("test_compression_vide");

exception
    when ADA.IO_EXCEPTIONS.NAME_ERROR =>
	Put_Line("Fichier source non present - impossible d'executer le test !");
end Test_Compression;
