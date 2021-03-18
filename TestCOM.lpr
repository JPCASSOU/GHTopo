program TestCOM;

{$APPTYPE CONSOLE}

uses
synaser,
sysutils,
UnitMinimalCOM
;

var
MyMesureVisee: TMesureViseeDistoX;
MyDistoX2Connexion: TDistoX2Connexion;
begin
  MyDistoX2Connexion := TDistoX2Connexion.Create;
  try
    if (MyDistoX2Connexion.Initialiser('COM10', 5)) then
    begin
      WriteLn(MyDistoX2Connexion.GetDescriptionDevice());
      while not (MyDistoX2Connexion.GetLastError() <> 0) do
      begin
        if (0 <> MyDistoX2Connexion.GetLastError()) then break;
        if (MyDistoX2Connexion.LireEtDecoderBuffer8Bytes(MyMesureVisee)) then
        begin
          WriteLn(Format('L = %.3f m; Az = %.2f; P = %.2f - EWE = %s',
                                         [myMesureVisee.Longueur,
                                          myMesureVisee.Azimut,
                                          myMesureVisee.Pente,
                                          myMesureVisee.HexaData
                                          ]));

        end;
        // quitter avec une visée courte et verticale
        if ((MyMesureVisee.Longueur < 0.20) and (Abs(MyMesureVisee.Pente) > 85)) then
        begin
          WriteLn('Visee de signalisation d''arret');
          break;
        end;
      end;

    end;
    MyDistoX2Connexion.Finaliser();
  finally
    FreeAndNil(MyDistoX2Connexion);
  end;
end.

