program NewScript01;
type TVectorResult = array[0..31] of string;
var
  VR   : TVectorResult;
  i, Nb: integer;
begin
  // Votre code ici
  if (PS2D_BeginDrawing()) then
  begin
    PS2D_SetBackgroundColor(0, 0, 255, 128);
    PS2D_EndAndDisplayDrawing();
  end;
end.
