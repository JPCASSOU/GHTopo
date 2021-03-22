program FTPTest;
type TVectorResult = array[0..31] of string;
var
  QPassword : string;
  VR   : TVectorResult;
  i, Nb: integer;
  QReturnCode : integer;
  QReturnMsg  : string;
begin
  // Votre code ici
  QReturnCode := -1;
  QReturnMsg  := '';
  //if (not InputQuery('Connexion au serveur FTP', 'Mot de passe', QPassWord)) then exit;
  //'function  InputQuery(const ACaption, APrompt: string; var QValeur: string): boolean;');
  if (FTP_BeginConnexion('ftp4.phpnet.org', 21, 'synclinal65', 'G40+g41-f840', QReturnCode, QReturnMsg)) then
  begin
    printf('Connexion OK: %d - %s', [QReturnCode, QReturnMsg]);
    FTP_EndConnexion();
  end
  else
  begin
    printf('Echec de connexion: %d - %s', [QReturnCode, QReturnMsg]);
  end;
end.
