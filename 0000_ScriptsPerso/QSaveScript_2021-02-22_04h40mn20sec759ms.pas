program FTPTest;
type TVectorResult = array[0..31] of string;
var
  QPassword : string;
  VR   : TVectorResult;
  i, Nb: integer;
begin
  // Votre code ici
  if (not InputQuery('Connexion au serveur FTP', 'Mot de passe', QPassWord)) then exit;
  //'function  InputQuery(const ACaption, APrompt: string; var QValeur: string): boolean;');
  if (FTP_BeginConnexion('ftp4.phpnet.org', 21, 'synclinal65', QPassword)) then
  begin
    print('Connexion OK');
    FTP_EndConnexion();
  end;
end.
