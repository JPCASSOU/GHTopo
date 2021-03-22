program NewScript01;
type TVectorResult = array[0..31] of string;
var
  VR   : TVectorResult;
  i, Nb: integer;
begin
  // Votre code ici
  if (PS2D_BeginDrawing()) then
  begin
    PS2D_SetBackgroundColor(0, 127, 192, 128);
    PS2D_SetPenAttributes(255, 0, 0, 255, 1);

    PS2D_SetBrushAttributes(0, 128, 0, 122, 0);
   // PS2D_SetFontAttributes('Arial', 12, 0, 0, 0, 255);


    PS2D_EndAndDisplayDrawing();
  end;
end.
