package body Decompression is
    procedure Lire_Metadonnees(Fichier: in Bin_IO.File_Type; Taille_Decompresse: out Integer; Arbre_Huffman: out T_Arbre) is
        Nombre_Caracteres: Integer;
        Caracteres_Utilises: Tab_Caracteres;
        tmp_byte: t_byte;
        Position_Caractere: Integer := 0;
    begin
        Caracteres_Utilises := (others => 0);
        -- Lit le nombre de caractères à extraire
        Taille_Decompresse := 0;
        for i in 1..4 loop
            Bin_IO.read(Fichier, tmp_byte);
            Taille_Decompresse := Taille_Decompresse * 256 + Integer(tmp_byte);
        end loop;

        Bin_IO.read(Fichier, tmp_byte);
        Nombre_Caracteres := Integer(tmp_byte);
        -- Si le fichier contient 256 caractères différents, il faut prendre en compte l'arrondi dû au cast en t_byte
        if Taille_Decompresse > 0 and then Nombre_Caracteres = 0 then
            Nombre_Caracteres := 256;
        end if;

        -- Lecture des caractères utilisés pour décoder le fichier
        for i in 1..Nombre_Caracteres loop
            Bin_IO.read(Fichier, tmp_byte);
            Caracteres_Utilises(i) := tmp_byte;
        end loop;

        Arbre_Huffman := Feuille('0', 0);
        -- Génération de l'arbre de huffman
        Construire_Structure(Arbre_Huffman, Fichier, Caracteres_Utilises, Nombre_Caracteres, Position_Caractere);
    end Lire_Metadonnees;

    procedure Construire_Structure(Arbre_Huffman: in T_Arbre; Fichier: in Bin_IO.File_Type; Caracteres_Utilises: in Tab_Caracteres; Nombre_Caracteres_Utilises: in Integer; Position_Caractere: in out Integer) is
        tmp_byte: t_byte;
    begin
        -- Tant que tous les caractères n'ont pas été scannés, on continue
        if Position_Caractere /= Nombre_Caracteres_Utilises then
            loop
                Bin_IO.read(Fichier, tmp_byte);
                if tmp_byte = 1 then
                    -- on remonte dans l'arbre
                    -- on ajoute un caractère uniquement si on est sur un noeud
                    if not Existe_Arbre_Gauche(Arbre_Huffman) then
                        Position_Caractere := Position_Caractere + 1;
                        Modifier_Caractere(Arbre_Huffman, Character'Val(Caracteres_Utilises(Position_Caractere)));
                    end if;
                else
                    -- on descend dans l'arbre
                    if Existe_Arbre_Gauche(Arbre_Huffman) then
                        Construire_Structure(Creer_Droite(Arbre_Huffman), Fichier, Caracteres_Utilises, Nombre_Caracteres_Utilises, Position_Caractere);
                    else
                        Construire_Structure(Creer_Gauche(Arbre_Huffman), Fichier, Caracteres_Utilises, Nombre_Caracteres_Utilises, Position_Caractere);
                    end if;
                end if;

                exit when tmp_byte = 1 or else Position_Caractere = Nombre_Caracteres_Utilises;
            end loop;
        end if;
    end Construire_Structure;

    procedure Extraction(Fichier_Sortie: in out Bin_IO.File_Type; Fichier: in Bin_IO.File_Type; Taille_Decompresse: in Integer; Arbre_Huffman: in T_Arbre) is
        Arbre_Temporaire: T_Arbre;
        byte: t_byte;
    begin
        for pos in 1..Taille_Decompresse loop
            Arbre_Temporaire := Arbre_Huffman;
            while Existe_Arbre_Gauche(Arbre_Temporaire) loop
                Bin_IO.Read(Fichier, byte);
                -- Descente dans l'arbre
                if byte = 0 then
                    Arbre_Temporaire := Sous_Arbre_Gauche(Arbre_Temporaire);
                else
                    Arbre_Temporaire := Sous_Arbre_Droit(Arbre_Temporaire);
                end if;
            end loop;
            -- Écriture dans le fichier de sortie
            Bin_IO.Write(Fichier_Sortie, t_byte(Character'Pos(Caractere(Arbre_Temporaire))));
        end loop;
    end Extraction;
end Decompression;
