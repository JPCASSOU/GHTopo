program FTPTest;
type TVectorResult = array[0..31] of string;
var
  VR   : TVectorResult;
  i, Nb: integer;
begin
  // Votre code ici
  if (FTP_BeginConnexion()) then
  begin
    FTP_EndConnexion();
  end;
end.
