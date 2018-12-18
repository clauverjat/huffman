-- Module File_A_Priorite
-- Implantation d'une file à priorité avec un tas.

--  L'arbre binaire tas est représenté par
--  un tableau d'éléments structuré ainsi :
--     * l'indice de la racine vaut 1
--     * l'indice du fils gauche du noeud i est 2*i
--     * l'indice du fils  droit du noeud i est 2*i + 1
--  Avec cette structure le père d'un noeud i en i/2.

-- with Ada.Text_IO; use Ada.Text_IO;
package body File_A_Priorite is

    --Quelques fonctions utiles sur le tas binaire :
    -- L'indice du sommet correspond-t-il à un noeud de l'arbre?
    function Est_Noeud(File : T_File_A_Priorite ; Sommet : Integer) return Boolean is
    begin
	return  ( 1 <= Sommet and Sommet <= File.Taille );
    end Est_Noeud;

    -- L'indice du fils gauche d'un sommet
    -- Attention : le fils peut ne pas exister
    function Fils_Gauche(Sommet : Integer) return Integer is
    begin
	return 2*Sommet;
    end Fils_Gauche;

    -- L'indice du fils droit d'un sommet
    -- Attention : le fils peut ne pas exister
    function Fils_Droit(Sommet : Integer) return Integer is
    begin
	return 2*Sommet + 1;
    end Fils_Droit;

    -- Est ce que l'indice du sommet correspond à une feuille de l'arbre ?
    function Est_Feuille(File : T_File_A_Priorite; Sommet : Integer) return Boolean is
    begin
	return not Est_Noeud(File, Fils_Gauche(Sommet));
    end Est_Feuille;

    -- L'indice du père d'un sommet
    -- Attention : La racine n'a pas de père donc par convention
    --             Pere renvoie 0 (qui n'est pas un noeud de l'arbre!)
    function Pere(Sommet : Integer) return Integer is
    begin
	return Sommet/2;
    end;

    -- Percoler le tas depuis un sommet
    -- La percolation fait remonter un nœud dans l’arbre par échange successif
    -- avec le père jusqu’à ce que l’arbre vérifie la propriété de tas.
    procedure Percoler(Tas: in out T_File_A_Priorite ; Sommet : in Integer) is
	Temp : T_Element;
    begin
	if Est_Noeud(Tas,Pere(Sommet)) and then Tas.Elements(Sommet) < Tas.Elements(Pere(Sommet)) then
	    -- Echanger Tas(Pere(Sommet)) et Tas(Sommet)
	    Temp := Tas.Elements(Pere(Sommet));
	    Tas.Elements(Pere(Sommet)) := Tas.Elements(Sommet);
	    Tas.Elements(Sommet) := Temp;

	    Percoler(Tas, Pere(Sommet));
	else
	    Null;
	end if;
    end Percoler;

    -- Tamiser un arbre à partir d'un noeud
    -- Tamiser fait descendre le nœud dans les sous-arbres par échange
    -- successif avec un de ses fils jusqu’à ce que l’arbre vérifie la propriété
    -- de tas.
    procedure Tamiser(Tas : in out T_File_A_Priorite;Sommet : in Integer) is
	Plus_Petit_Fils : Integer;
	Temp : T_Element;
    begin
	if not Est_Feuille(Tas, Sommet) then
	    if  Est_Noeud(Tas,Fils_Droit(Sommet)) and then
	        Tas.Elements(Fils_Droit(Sommet)) < Tas.Elements(Fils_Gauche(Sommet)) then
		Plus_Petit_Fils := Fils_Droit(Sommet);
	    else
		Plus_Petit_Fils := Fils_Gauche(Sommet);
	    end if;
	    if Tas.Elements(Plus_Petit_Fils) < Tas.Elements(Sommet) then
		-- Echanger Tas.Elements(Plus_Petit_Fils) et Tas.Elements(Sommet)
		Temp := Tas.Elements(Sommet);
		Tas.Elements(Sommet) := Tas.Elements(Plus_Petit_Fils);
		Tas.Elements(Plus_Petit_Fils) := Temp;

		Tamiser(Tas, Plus_Petit_Fils);
	    end if;
	end if;
    end;

    --
    -- Fonctions principales :
    --

    procedure Initialiser (File : out T_File_A_Priorite) is
    begin
	File.Taille := 0;
    end;

    function Taille(File : in T_File_A_Priorite) return Integer is
    begin
	return File.Taille;
    end Taille;

    function Sommet (File: in T_File_A_Priorite) return T_Element is
    begin
	if File.Taille > 0 then
	    return File.Elements(1);
	else
	    raise File_Vide_Error;
	end if;
    end;

    procedure Enfiler (File : in out T_File_A_Priorite; Element: in T_Element) is
	Indice_Dernier_Element : Integer;
    begin
	if File.Taille < Capacite then
	    File.Taille := File.Taille + 1;
	    Indice_Dernier_Element := File.Taille;
	    File.Elements(Indice_Dernier_Element) := Element;
	    Percoler(File, Indice_Dernier_Element);
	else
	    raise File_Pleine_Error;
	end if;
    end;

    procedure Extraire(File : in out T_File_A_Priorite) is
    begin
	if File.Taille > 0 then
	    File.Elements(1) := File.Elements(File.Taille);
	    File.Taille := File.Taille - 1;
	    Tamiser(File, 1);
	else
	    raise File_Vide_Error;
	end if;
    end;

--    procedure Afficher_File(File : in T_File_A_Priorite) is
--	procedure Afficher_Depuis(Sommet : in Integer; Indentation : in Integer; Forme : in Character) is
--	begin
--	    if Est_Noeud(File,Sommet)  then
--		for i in 1..Indentation loop
--		    Put("---");
--		end loop;
--		Put(Forme&" ");
--		Afficher_Elmt(File.Elements(Sommet));
--		New_Line;
--		Afficher_Depuis(Fils_Gauche(Sommet), Indentation + 1, '>');
--		Afficher_Depuis(Fils_Droit(Sommet),  Indentation + 1, '<');
--	    end if;
--
--	end Afficher_Depuis;
--    begin
--	Afficher_Depuis(1,0,'/');
--	Put_Line("___________");
--    end Afficher_File;

end File_A_Priorite;
