with File_A_Priorite;
with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_File_A_Priorite is

    package File_Entier is
	    new File_A_Priorite (Capacite => 9,
			  T_Element => Integer,"<" => "<");

    use File_Entier;

    File : T_File_A_Priorite;
begin
    Initialiser (File);
    Put_Line("Enfiler 8");
    Enfiler(File, 8);
    Put_Line("Enfiler -3");
    Enfiler(File, -3);
    pragma Assert(Sommet(File) = -3);
    Put_Line("Extraire sommet");
    Extraire(File);
    Put_Line("Enfiler 8");
    Enfiler(File, 8);
    Put_Line("Enfiler 9");
    Enfiler(File, 9);
    pragma Assert(Sommet(File) = 8);
    Put_Line("Extraire sommet");
    Extraire(File);
    pragma Assert(Sommet(File) = 8);
    Put_Line("Extraire sommet");
    Extraire(File);
    pragma Assert(Sommet(File) = 9);
    Put_Line("Extraire sommet");
    Extraire(File);
    pragma Assert(Taille(File) = 0);
    Enfiler(File, 2);
    pragma Assert(Taille(File) = 1);
    Enfiler(File, 3);
    pragma Assert(Taille(File) = 2);
end Test_File_A_Priorite;
