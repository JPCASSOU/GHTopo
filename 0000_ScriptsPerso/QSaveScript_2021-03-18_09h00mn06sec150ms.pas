program NewScript01;
type TVectorResult = array[0..31] of string;
var
  VR   : TVectorResult;
  i, Nb: integer;
begin
  dt_removeseriebynumserie(503);
  dt_removeseriebyidx(2);
end.
