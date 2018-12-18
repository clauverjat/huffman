with File_A_Priorite;
-- with Byte;

-- Ce module permet la compression.

package body Compression is
    TAILLE_ALPHABET : constant Integer := 256;

    function Ordre(Arbre1: T_Arbre; Arbre2: T_Arbre) return Boolean is
    begin
	return (Frequence(Arbre1) < Frequence(Arbre2));
    end Ordre;

    package File_Arbre is
	    new File_A_Priorite(Capacite => TAILLE_ALPHABET,
			  T_Element => T_Arbre,
			  "<" => Ordre);
    use File_Arbre;

    procedure Compresser (Fichier : in out Char_IO.File_Type; Sortie: in out Bin_IO.File_Type; Verbose : Boolean := False) is
	Frequences : T_Tableau_Entier;
	Arbre_Huff : T_Arbre;
	Table : T_Tableau_Binaire;
    begin
	-- Génération de la table
	Frequences :=  Frequences_Caracteres(Fichier);

	Arbre_Huff := Arbre_Huffman(Frequences);
	if Verbose then
	    Arbre.Afficher(Arbre_Huff);
	end if;

	Table := Table_Huffman(Arbre_Huff);
	if Verbose then
	    Tableau_Code.Afficher(Table);
	end if;

	-- Codage du fichier
	Coder(Fichier, Sortie, Arbre_Huff, Table);
	-- Détruire la table de Huffman
	Detruire(Table);
	Detruire(Arbre_Huff);
    end Compresser;

    function Frequences_Caracteres(Fichier : in out Char_IO.File_Type) return T_Tableau_Entier is
	Frequences : T_Tableau_Entier := (others => 0);
	Caractere : Character;
    begin
	while not End_Of_File(Fichier) loop
	    Char_IO.Read(Fichier, Caractere);
	    Frequences(Caractere) := Frequences(Caractere) + 1;
	end loop;
	-- Remettre le curseur au début du fichier
	Char_IO.Reset(Fichier);
	return Frequences;
    end Frequences_Caracteres;

    function Arbre_Huffman(Frequence : T_Tableau_Entier) return T_Arbre is
	Foret : T_File_A_Priorite;
	Arbre_Min, Arbre_Min2 : T_Arbre;

	-- Ajouter les feuilles
	-- Construire la feuille de chaque caractère et l'ajouter à la file
	-- Paramètres :
	--   Foret : T_File_A_Priorite : la file à priorité listant les arbres
	--   Frequence : T_Tableau_Entier     : tableau des fréquences des caractères
	procedure Ajouter_Feuilles(Foret: in out T_File_A_Priorite; Frequence : T_Tableau_Entier) is
	begin
	    for c in Frequence'Range loop
		if Frequence(c) /= 0 then
		    Enfiler( Foret, Feuille(Caractere => c,
			                    Frequence => Frequence(c)) );
		end if;
		end loop;
	end Ajouter_Feuilles;
    begin
	Initialiser(Foret);
	Ajouter_Feuilles(Foret, Frequence);
	if Taille(Foret) = 0 then
	    return Nil; -- Arbre vide
	else
	    while Taille(Foret) >= 2 loop
		Arbre_Min := Sommet(Foret);
		Extraire(Foret);
		Arbre_Min2 := Sommet(Foret);
		Extraire(Foret);
		Enfiler(Foret, Fusion(Arbre_Min, Arbre_Min2));
	    end loop;
	    return Sommet(Foret);
        end if;
    end Arbre_Huffman;


    function Table_Huffman(Arbre : T_Arbre) return T_Tableau_Binaire is
	Code : T_Tableau_Binaire;
	Prefixe : T_Liste;

	-- Coder avec le préfixe passé en paramètre le noeud
	-- Le code est écrit lorsque le noeud est une feuille
	procedure Coder_Avec_Prefixe(Arbre : in T_Arbre; Prefixe : in out T_Liste) is
	begin
	    if Est_Vide(Arbre) then
		Null;
	    elsif Est_Feuille(Arbre) then
		-- Stocker le code dans la table de Huffman
		Code(Caractere(Arbre)) := Liste_Renverse(Prefixe);
	    else
		-- Sous arbre gauche
		Ajouter(Prefixe, 0); -- Ajouter 0 au code
		Coder_Avec_Prefixe(Sous_Arbre_Gauche(Arbre), Prefixe);
		Enlever_Tete(Prefixe); -- Retirer 0
		-- Sous arbre droite
		Ajouter(Prefixe, 1); -- Ajouter 1 au code
		Coder_Avec_Prefixe(Sous_Arbre_Droit(Arbre), Prefixe);
		Enlever_Tete(Prefixe); -- Retirer 1
	    end if;
	end;
    begin
	Initialiser(Prefixe);
	Coder_Avec_Prefixe(Arbre, Prefixe);
	return Code;
    end Table_Huffman;


    procedure Coder(Fichier : in out Char_IO.File_Type;
		    Sortie : in out Bin_IO.File_Type;
		    Arbre : in T_Arbre ;
		    Table: in T_Tableau_Binaire) is
    begin
	Ecrire_Entete(Sortie, Arbre);
	Coder_Message(Fichier, Sortie, Table);
    end Coder;

    procedure Ecrire_Entete (Sortie : in out Bin_IO.File_Type;
			     Arbre : in T_Arbre) is
	-- Ecrire dans le fichier les caractères de l'arbre dans l'ordre infixe
	procedure Afficher_Caractere_Infixe(Arbre : in T_Arbre) is
	begin
	    if Est_Vide(Arbre) then
		Null;
	    elsif Est_Feuille(Arbre) then
		-- On écrit le caractère sous forme d'octet dans la sortie
		Bin_IO.Write(Sortie,
	                     T_Byte(Character'Pos(Caractere(Arbre))));
	    else
		Afficher_Caractere_Infixe(Sous_Arbre_Gauche(Arbre));
		Afficher_Caractere_Infixe(Sous_Arbre_Droit(Arbre));
	    end if;
	end Afficher_Caractere_Infixe;

	-- Description de l'arbre
	procedure Decrire_Arbre(Arbre : T_Arbre; Droite : Boolean) is
	begin
        if not Est_Feuille(Arbre) then
            Bin_IO.Write(Sortie,0);
            Decrire_Arbre(Sous_Arbre_Gauche(Arbre), False);
            Bin_IO.Write(Sortie,1);
            Bin_IO.Write(Sortie,0);
            Decrire_Arbre(Sous_Arbre_Droit(Arbre), Droite);
            if not Droite then
                Bin_IO.Write(Sortie,1);
	    end if;
	    end if;
	end Decrire_Arbre;

	-- Ecrire un entier sur un certain nombre d'octets
	-- Ce code suppose une architecture/convention "big endian"
	procedure Ecrire_Entier(Fichier :in out Bin_IO.File_Type; Entier : Integer; Nb_Octets : Integer) is
	begin
	    for i in 1..Nb_Octets loop
            Bin_IO.Write(Fichier, T_Byte(Entier/256**(Nb_Octets-i) mod 256));
	    end loop;
	end;

    begin
	-- Ecrire sur 4 octets la taille du texte
	Ecrire_Entier(Fichier => Sortie,
	              Entier => Frequence(Arbre),
	              Nb_Octets => 4);
	-- Ecrire sur 1 octet la taille de l'arbre
	Ecrire_Entier(Fichier => Sortie,
	              Entier => Taille(Arbre),
	              Nb_Octets => 1);
	-- Ecrire un parcours infixe de l'arbre
	Afficher_Caractere_Infixe(Arbre);
	-- Ecrire la description du parcours infixe de l'arbre
	if not Est_Vide(Arbre) then
	    Decrire_Arbre(Arbre => Arbre, Droite => True);
	    Bin_IO.Write(Sortie,1);
	end if;
    end Ecrire_Entete;



    procedure Coder_Message(Fichier : in out Char_IO.File_Type;
			    Sortie : in out Bin_IO.File_Type;
			    Table: in T_Tableau_Binaire) is
	Caractere : Character;
	-- Ecrire un code binaire dans un fichier
        -- Paramètres :
        --      Sortie : fichier ouvert en écriture
        --      Code : le code binaire à écrire
	procedure Ecrire(Sortie : Bin_IO.File_Type; Code : in T_Liste) is
	begin
	    if not Est_Vide(Code) then
		Bin_IO.Write(Sortie, Tete(Code));
		Ecrire(Sortie, Code.all.suivant);
	    end if;
	end Ecrire;
    begin
	while not End_Of_File(Fichier) loop
	    Char_IO.Read(Fichier, Caractere);
	    Ecrire(Sortie,Table(Caractere));
	end loop;
    end;

end Compression;
