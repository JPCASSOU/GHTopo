program NewScript01;
type TVectorResult = array[0..31] of string;
var
  VR   : TVectorResult;
  i, Nb: integer;
begin
  // Votre code ici
  if (PS2D_BeginDrawing()) then
  begin
    ps2d_setbackgroundcolor(240, 0, 240, 255);
    Nb := PS2D_AddStyleSheet('MonStyle1',
                            255, 0, 0, 255, 1,
                            0, 128, 0, 122, 0,
                            'Arial', 0, 0, 0, 255, 12, 0);
    printf('Style %d ajouté', [Nb]);
    if (Nb >= 0) then PS2D_SetStyleSheet(Nb);

    PS2D_BeginPolyline('Poly1', false);
      PS2D_AddVertex(-80, -60);
      PS2D_AddVertex(80, -65);
      PS2D_AddVertex(56, 44);
      PS2D_AddVertex(-20, 66);
    PS2D_EndPolyline(false);



    PS2D_EndAndDisplayDrawing();
  end;


end.
