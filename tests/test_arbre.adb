with Arbre;
use Arbre;

procedure Test_Arbre is

    procedure Initialiser_Arbre(Arbre : out T_Arbre) is
    begin
        --                          +---------------+
        --                          | Caractere : # |
        --                          | Frequence : 8 |
        --                          +---------------+
        --                                |   |
        --                      +---------+   +---------+
        --                      |                       |
        --                      v                       v
        --              +---------------+       +---------------+
        --              | Caractere : # |       | Caractere : c |
        --              | Frequence : 3 |       | Frequence : 5 |
        --              +---------------+       +---------------+
        --                     | |
        --           +---------+ +---------
        --           |                    |
        --           v                    v
        --   +---------------+     +---------------+
        --   | Caractere : a |     | Caractere : b |
        --   | Frequence : 1 |     | Frequence : 2 |
        --   +---------------+     +---------------+
        --
        Arbre := Nouveau('#', 8,
            Gauche => Nouveau('#', 3,
                Gauche => Feuille('a', 1),
                Droite => Feuille('b', 2)
            ),
            Droite => Feuille('c', 5)
        );

    end Initialiser_Arbre;

    procedure Initialiser_Feuille(Arbre : out T_Arbre) is
    begin
        --                          +---------------+
        --                          | Caractere : # |
        --                          | Frequence : 1 |
        --                          +---------------+
        --
        Arbre := Feuille('#', 1);
    end Initialiser_Feuille;

    procedure Tester_Feuille is
        Feuille : T_Arbre;
    begin
        Initialiser_Feuille(Feuille);
        pragma assert(not Existe_Arbre_Gauche(Feuille));
        pragma assert(not Existe_Arbre_Droit(Feuille));
        Detruire(Feuille);
    end Tester_Feuille;

    procedure Tester_Frequence is
        Arbre : T_Arbre;
    begin
        Initialiser_Arbre(Arbre);
        pragma assert(Frequence(Sous_Arbre_Gauche(Sous_Arbre_Gauche(Arbre))) = 1 and then Caractere(Sous_Arbre_Droit(Sous_Arbre_Gauche(Arbre))) = 'b');
        Detruire(Arbre);
    end Tester_Frequence;

	procedure Tester_Arbre_Gauche is
        Arbre: T_Arbre;
        Sous_Arbre: T_Arbre;
    begin
        Initialiser_Feuille(Arbre);
        Sous_Arbre := Creer_Gauche(Arbre);
        pragma assert(Existe_Arbre_Gauche(Arbre));
        pragma assert(Sous_Arbre_Gauche(Arbre) = Sous_Arbre);
        Detruire(Arbre);
    end Tester_Arbre_Gauche;

    procedure Tester_Arbre_Droit is
        Arbre: T_Arbre;
        Sous_Arbre: T_Arbre;
    begin
        Arbre := Feuille(Character'Val(0), 0);
        Sous_Arbre := Creer_Droite(Arbre);
        pragma assert(Existe_Arbre_Droit(Arbre));
        pragma assert(Sous_Arbre_Droit(Arbre) = Sous_Arbre);
        Detruire(Arbre);
    end Tester_Arbre_Droit;

    procedure Tester_Modifier_Noeud is
        Arbre: T_Arbre;
    begin
        Initialiser_Arbre(Arbre);
        Modifier_Frequence(Arbre, 17);
        pragma assert(Frequence(Arbre) = 17);
        Modifier_Caractere(Sous_Arbre_Gauche(Sous_Arbre_Gauche(Arbre)), '@');
        pragma Assert(Caractere(Sous_Arbre_Gauche(Sous_Arbre_Gauche(Arbre))) = '@');
        Detruire(Arbre);
    end Tester_Modifier_Noeud;

    procedure Tester_Est_Feuille is
        Arbre1 : T_Arbre;
        Arbre2 : T_Arbre;
    begin
        Initialiser_Arbre(Arbre1);
        pragma Assert (not Est_Feuille(Arbre1));
        pragma Assert (not Est_Feuille(Sous_Arbre_Gauche(Arbre1)));
        pragma Assert (Est_Feuille(Sous_Arbre_Gauche(Sous_Arbre_Gauche(Arbre1))));
        pragma Assert (Est_Feuille(Sous_Arbre_Gauche(Sous_Arbre_Gauche(Arbre1))));
        pragma Assert (Est_Feuille(Sous_Arbre_Droit(Arbre1)));
        Detruire(Arbre1);

        Initialiser_Feuille(Arbre2);
        pragma Assert(Est_Feuille (Arbre2));
        Detruire(Arbre2);
    end Tester_Est_Feuille;


    procedure Tester_Fusion is
        Arbre2 : T_Arbre;
        Arbre1 : T_Arbre;
        Arbre_Fusion : T_Arbre;
    begin
        Initialiser_Feuille(Arbre2);
        Initialiser_Arbre(Arbre1);
        Arbre_Fusion := Fusion(Arbre2, Arbre1);
        pragma Assert (Frequence(Arbre_Fusion) = 9);
        Detruire(Arbre_Fusion);
    end Tester_Fusion;

    procedure Tester_Afficher is
        Arbre2 : T_Arbre;
        Arbre1 : T_Arbre;
    begin

        Initialiser_Arbre(Arbre1);
        Afficher(Arbre1);
        Detruire(Arbre1);
        Initialiser_Feuille(Arbre2);
        Afficher(Arbre2);
        Detruire(Arbre2);
    end Tester_Afficher;


begin
    Tester_Feuille;
    Tester_Frequence;
    Tester_Arbre_Gauche;
    Tester_Arbre_Droit;
    Tester_Modifier_Noeud;
    Tester_Est_Feuille;
    Tester_Fusion;
    Tester_Afficher;
end Test_Arbre;
