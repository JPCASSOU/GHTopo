// exemple de script utilisant PascalScript
program Toto;
var
  i: integer;
  EWE: integer;
begin
  EWE := 26;
  AfficherMessage(GetGHTopoDirectory());
  EWE := SelectionCouleurToporobot(EWE);
  showmessage(Format('EWE = %d', [EWE]));
  i := 0;
  while True do
  begin
    AfficherMessage(Format('Passe %d', [i]));
    i := i + 1;
  end;
end.                                         
