-- Implantation du module Arbre.

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Arbre is

    procedure Free is
        new Ada.Unchecked_Deallocation (T_Noeud, T_Arbre);

    function Nouveau(Caractere: Character; Frequence: Integer; Gauche: T_Arbre; Droite: T_Arbre) return T_Arbre is
        Arbre: T_Arbre;
    begin
        Arbre := new T_Noeud'(Frequence => Frequence,
                         Caractere => Caractere,
                         Gauche => Gauche,
                         Droite => Droite);
        return Arbre;
    end Nouveau;

    function Feuille(Caractere: Character; Frequence: Integer) return T_Arbre is
    begin
        return Nouveau(Caractere => Caractere,
            Frequence => Frequence,
            Gauche => Null,
            Droite => Null);
    end Feuille;

    function Existe_Arbre_Gauche(Arbre: in T_Arbre) return Boolean is
    begin
        return Arbre /= Null and then Arbre.all.Gauche /= Null;
    end Existe_Arbre_Gauche;

    function Existe_Arbre_Droit(Arbre: in T_Arbre) return Boolean is
    begin
        return Arbre /= Null and then Arbre.all.Droite /= Null;
    end Existe_Arbre_Droit;

    function Creer_Gauche(Arbre: in T_Arbre) return T_Arbre is
    begin
        Arbre.all.Gauche := Feuille('0', 0);
        return Sous_Arbre_Gauche(Arbre);
    end Creer_Gauche;

    function Creer_Droite(Arbre: in T_Arbre) return T_Arbre is
    begin
        Arbre.all.Droite := Feuille('0', 0);
        return Sous_Arbre_Droit(Arbre);
    end Creer_Droite;

    function Sous_Arbre_Gauche(Arbre: in T_Arbre) return T_Arbre is
    begin
		return Arbre.all.Gauche;
    end Sous_Arbre_Gauche;

    function Sous_Arbre_Droit(Arbre: in T_Arbre) return T_Arbre is
    begin
		return Arbre.all.Droite;
    end Sous_Arbre_Droit;

    function Frequence(Arbre: in T_Arbre) return Integer is
    begin
	if Arbre = Null then
	    return 0;
	else
	    return Arbre.all.Frequence;
	end if;
    end Frequence;

    function Caractere(Arbre: in T_Arbre) return Character is
    begin
		return Arbre.all.Caractere;
    end Caractere;

    procedure Modifier_Caractere(Arbre: in T_Arbre; c: Character) is
	begin
		Arbre.all.Caractere := c;
	end Modifier_Caractere;

    procedure Modifier_Frequence(Arbre: in T_Arbre; Frequence : Integer) is
    begin
        Arbre.all.Frequence := Frequence;
    end Modifier_Frequence;

    function Est_Vide(Arbre : in T_Arbre) return Boolean is
    begin
    return Builtin_Equal(Arbre, Null);
    end Est_Vide;


    function Taille(Arbre : T_Arbre) return Integer is
    begin
    if Arbre = Null then
        return 0;
    elsif Est_Feuille(Arbre) then
        return  1;
    else
        return Taille(Arbre.all.Gauche) + Taille(Arbre.all.Droite);
    end if;
    end;

    function Est_Feuille(Arbre : T_Arbre) return Boolean is
    begin
        return Est_Vide(Arbre.all.Gauche) and then Est_Vide(Arbre.all.Droite);
    end Est_Feuille;

    function Fusion(Arbre1 : in T_Arbre; Arbre2: in T_Arbre) return T_Arbre is
    begin
	return new T_Noeud'(Caractere => '#',
		     Frequence => Arbre1.all.Frequence + Arbre2.all.Frequence,
		     Gauche => Arbre1,
		     Droite => Arbre2);
    end Fusion;

    procedure Detruire(Arbre : in out T_Arbre) is
    begin
    if Arbre /= Null then
        Detruire(Arbre.all.Gauche);
        Detruire(Arbre.all.Droite);
        Free(Arbre);
    end if;
    end Detruire;

    -- Relation d'égalité sémantique entre deux arbres
    function "="(Arbre1: in T_Arbre; Arbre2: in T_Arbre) return Boolean is
    begin
	-- /!\ On teste ici l'égalité des pointeurs
	-- (condition suffisante d'égalité) pour une question d'efficacité
	if Builtin_Equal(Arbre1, Arbre2) then
	    return True;
	end if;

	if not Est_Vide(Arbre1) and not Est_Vide(Arbre2) then
	    if Est_Feuille(Arbre1) and Est_Feuille(Arbre2) then
		return Caractere(Arbre1) = Caractere(Arbre2);
	    else
		return Sous_Arbre_Gauche(Arbre1) = Sous_Arbre_Gauche(Arbre2)
			and then Sous_Arbre_Droit(Arbre1) = Sous_Arbre_Droit(Arbre2);
	    end if;
	else
	    return False;

	end if;
    end "=";

    procedure Afficher(Arbre : in T_Arbre) is

        procedure Afficher_Indentation(Arbre : in T_Arbre; Indentation : in Integer; Forme : in Character) is
        begin
            if not Est_Vide(Arbre) then
                for i in 1..Indentation loop
                    Put("        ");
                end loop;
                if Forme = '>' then
                    Put("\--0-- ");
                elsif Forme = '<' then
                    Put("\--1-- ");
                end if;
                Put("(");
                Put(Arbre.all.Frequence, 0);
                Put(")");
                if Est_Feuille(Arbre) then
                    Put(" '" & Arbre.all.Caractere & "'");
                end if;
                New_Line;
                Afficher_Indentation(Arbre.all.Gauche, Indentation + 1, '>');
                Afficher_Indentation(Arbre.all.Droite,  Indentation + 1, '<');
            end if;
        end Afficher_Indentation;

    begin

    Afficher_Indentation(Arbre, -1, '/');
    end Afficher;

end Arbre;
