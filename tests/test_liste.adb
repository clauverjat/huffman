with Liste;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Liste is

    package Liste_Entier is
	    new Liste (T_Element => Integer);
    use Liste_Entier;

    procedure Initialiser_Test (Liste1, Liste2, Liste3  : out T_Liste) is
    begin
	-- Liste1
	Initialiser (Liste1);
	Ajouter(Liste1, 1);

	-- Liste2
	Initialiser (Liste2);
	Ajouter (Liste2, 2);
	Ajouter (Liste2, 4);
	Ajouter (Liste2, 8);

	-- Liste3
	Initialiser(Liste3);
    end;

    procedure Detruire_Test (Liste1, Liste2, Liste3  : in out T_Liste) is
    begin
	Detruire (Liste1);
	Detruire (Liste2);
	Detruire(Liste3);
    end;


    procedure Tester_Tete is
	Liste1, Liste2, Liste3: T_Liste;
	Inutile : Integer;
	pragma Unreferenced (Inutile);
    begin
	Initialiser_Test(Liste1, Liste2, Liste3);
	pragma Assert( Tete(Liste1) = 1);
	pragma Assert( Tete(Liste2) = 8);
	-- Tester la levée de l'exception Liste_Vide
	begin
	    Inutile := Tete(Liste3);
	    pragma Assert (false);
	exception
	    when Liste_Vide_Error => Null; -- OK
	end;
	Detruire_Test(Liste1, Liste2, Liste3);
    end Tester_Tete;


    procedure Tester_Enlever_Tete is
	Liste1, Liste2, Liste3: T_Liste;
    begin
	Initialiser_Test(Liste1, Liste2, Liste3);
	Enlever_Tete(Liste1);
	Enlever_Tete(Liste2);
	pragma Assert( Tete(Liste2) = 4);
	-- Tester la levée de l'exception Liste_Vide
	begin
	    Enlever_Tete(Liste3);
	    pragma Assert (false);
	exception
	    when Liste_Vide_Error => Null; -- OK
	end;
	Detruire_Test(Liste1, Liste2, Liste3);
    end Tester_Enlever_Tete;

    procedure Tester_Liste_Renverse is
	Liste1, Liste2, Liste3 : T_Liste;
	Renversee1, Renversee2, Renversee3 : T_Liste;
    begin
	Initialiser_Test(Liste1, Liste2, Liste3);

	Renversee1 := Liste_Renverse(Liste1);
	pragma Assert(Tete(Renversee1) = 1);
	Detruire(Renversee1);

	Renversee2 := Liste_Renverse(Liste2);
	pragma Assert(Tete(Renversee2) = 2);
	Detruire(Renversee2);

	Renversee3 := Liste_Renverse(Liste3);
	pragma Assert(Est_Vide(Renversee3));
	Detruire(Renversee3);

	Detruire_Test(Liste1, Liste2, Liste3);
    end Tester_Liste_Renverse;


begin
    Put_Line("Tester Tete");
    Tester_Tete;
    Put_Line("Tester Enlever Tete");
    Tester_Enlever_Tete;
    Put_Line("Tester Liste Renverse");
    Tester_Liste_Renverse;
end Test_Liste;
