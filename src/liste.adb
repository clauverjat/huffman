with Ada.Unchecked_Deallocation;

package body Liste is

    procedure Free is
	    new Ada.Unchecked_Deallocation (T_Cellule, T_Liste);

    procedure Initialiser (Liste : out T_Liste) is
    begin
	Liste := Null;
    end Initialiser;

    procedure Detruire (Liste: in out T_Liste) is
    begin
	if Liste /= Null then
	    Detruire(Liste.Suivant);
	    Free(Liste);
	end if;
    end Detruire;


    function Est_Vide (Liste : in T_Liste) return Boolean is
    begin
	return Liste = Null;
    end Est_Vide;


    function Tete (Liste : in T_Liste) return T_Element is
    begin
	if Est_Vide(Liste) then
	    raise Liste_Vide_Error;
	else
	    return Liste.all.Element;
	end if;
    end Tete;


    procedure Ajouter(Liste : in out T_Liste ; Element : in T_Element) is
    begin
	Liste := new T_Cellule'(Element => Element, Suivant => Liste);
    end Ajouter;

    procedure Enlever_Tete(Liste : in out T_Liste) is
	Liste_Obselete : T_Liste;
    begin
	if Est_Vide(Liste) then
	    raise Liste_Vide_Error;
	else
	    Liste_Obselete := Liste;
	    Liste := Liste.all.Suivant;
	    Free(Liste_Obselete);
	end if;
    end Enlever_Tete;

    function Liste_Renverse(Liste : in T_Liste) return T_Liste is
	Resultat : T_Liste;
	Curseur : T_Liste;
    begin
	Initialiser(Resultat);
	Curseur := Liste;
	while Curseur /= Null loop
	    Ajouter(Resultat, Curseur.all.Element);
	    Curseur := Curseur.all.Suivant;
	end loop;
	return Resultat;
    end Liste_Renverse;

    procedure Afficher(Liste : in T_Liste) is
	Curseur : T_Liste;
    begin
	Curseur := Liste;
	while Curseur /= Null loop
	    Put(Curseur.all.Element);
	    Curseur := Curseur.all.Suivant;
	end loop;
    end Afficher;


    function "=" (Liste1, Liste2 : in T_Liste) return Boolean is
    begin
	if (Builtin_Equal(Liste1, Null) and Builtin_Equal(Liste2, Null) ) then
	    return True;
	elsif ( not Builtin_Equal(Liste1,Null) and  not Builtin_Equal(Liste2, Null) ) then
	    return (Liste1.all.Element = Liste2.all.Element
	     and then Liste1.all.Suivant = Liste2.all.Suivant);
	else
	    return False;
	end if;
    end "=";

end Liste;
