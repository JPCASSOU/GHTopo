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
    PS2D_SetStyleSheet(0);

    PS2D_AddTexte(12, -45, 34, 0, 'Fuck The Christ');


    PS2D_EndAndDisplayDrawing();
  end;


end.
