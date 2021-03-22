program NewScript01;

var
  VR   : array  of const;
  i, Nb: integer;
begin
  // Votre code ici
  SetLength(VR, 10);
  DT_ExtractDataFromVisee(1, 12, VR);
end.
