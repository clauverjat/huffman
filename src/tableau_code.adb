with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Tableau_Code is

    procedure Detruire(Table : in out T_Tableau_Binaire)  is
    begin
	for c in Table'Range loop
	    Liste_Byte.Detruire(Table(c));
	end loop;
    end Detruire;


    procedure Afficher(Table_Huffman : T_Tableau_Binaire) is
	procedure Afficher_Byte(Byte : T_Byte) is
	begin
	    Put(Integer(Byte),0);
	end Afficher_Byte;

	procedure Afficher_Liste is new Liste_Byte.Afficher(Put => Afficher_Byte);

    begin
	for c in Table_Huffman'Range loop
	    if not Liste_Byte.Est_Vide(Table_Huffman(c)) then
		Put("'"&c&"' -> ");
		Afficher_Liste(Table_Huffman(c));
		New_Line;
	    end if;
	end loop;
    end Afficher;

end Tableau_Code;
