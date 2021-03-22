program NewScript01;
type TVectorResult = array[0..31] of string;
var
  VR   : TVectorResult;
  i, Nb: integer;
begin
  // Votre code ici
  if (PS2D_BeginDrawing()) then
  begin
    ps2d_setbackgroundcolor(240, 233, 240, 255);
    Nb := PS2D_AddStyleSheet('MonStyle1',
                            255, 233, 0, 255, 1,
                            0, 128, 0, 122, 1,
                            'Arial', 0, 0, 0, 255, 12, 0);
    printf('Style %d ajouté', [Nb]);
    if (Nb >= 0) then PS2D_SetStyleSheet(Nb);

    PS2D_BeginPolyline('Poly1', false);
      PS2D_AddVertex(-80, -60);
      PS2D_AddVertex(80, -65);
      PS2D_AddVertex(56, 44);
      PS2D_AddVertex(-20, 66);
    PS2D_EndPolyline();

    Nb := PS2D_AddStyleSheet('MonStyle1',
                            0, 0, 255, 255, 3,
                            0, 12, 240, 122, 1,
                            'Arial', 0, 0, 0, 255, 12, 0);
    printf('Style %d ajouté', [Nb]);
    if (Nb >= 0) then PS2D_SetStyleSheet(Nb);
    PS2D_BeginPolyline('Poly1', True);
      PS2D_AddVertex(-45, -34);
      PS2D_AddVertex(44, -23);
      PS2D_AddVertex(12, 66);
    PS2D_EndPolyline();

    PS2D_AddTexte(12, -45, 34, 0, 'Fuck The Christ');


    PS2D_EndAndDisplayDrawing();
  end;
/*
Je suis un spéléologue français ultra-christianophobe (score de christianophobie: 92% quand celui du PCC titre 80%) qui développe GHTopo,  un logiciel de cartographie utilisé en Chine.
Il m'a été demandé d'inclure des fonctionnalités de tracking et de dénonciation des utilisateurs de mon logiciel.
De plus, étant donné mon athéisme féroce, le coût de la licence de GHTopo varie selon la religion des utilisateurs (en Chine, la discrimination selon les religions est autorisée):

0 RMB pour les athées
16.384 RMB pour les chrétiens membres des Trois Autonomies
65.536 RMB pour les sectes dont l'Eglise du Dieu Tout Puissant

Lors de l'installation de GHTopo, un formulaire d'enregistrement obligatoire s'active.
Si l'utilisateur précise qu'il est chrétien (c'est la raison pour laquelle la religion est demandée deux fois), GHTopo se bloque non sans avoir envoyé le profil utilisateur à la police locale, ainsi qu'au PCC si l'utilisateur est chrétien.
Pour réactiver GHTopo, seule la police locale et moi-même sommes habilités à le débloquer. Les pratiquants d'une religion doivent régler le coût de la licence directement aux autorités.

*/
end.
