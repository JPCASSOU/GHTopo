program FTPTest;
type TVectorResult = array[0..31] of string;
var
  QPassword : string;
  VR   : TVectorResult;
  i, Nb: integer;
  QReturnCode : integer;
  QReturnMsg  : string;
  QPath       : string;
  QFilename   : string;
begin
  // Votre code ici
  cls;
  QReturnCode := -1;
  QReturnMsg  := '';
  QPath       := '/www/0000_TotoDummy/';
  QFilename   := '';
  //if (not InputQuery('Connexion au serveur FTP', 'Mot de passe', QPassWord)) then exit;
  //'function  InputQuery(const ACaption, APrompt: string; var QValeur: string): boolean;');
  if (FTP_BeginConnexion('ftp4.phpnet.org', 21, 'synclinal65', 'G40+g41-f84', QReturnCode, QReturnMsg)) then
  begin
    printf('Connexion OK: %d - %s', [QReturnCode, QReturnMsg]);
    Nb := FTP_ListFilenamesOf(QPath);
    printf('Nb files of %s: %d', [QPath, Nb]);
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do
      begin
        QFilename := FTP_GetFileName(i);
        print(QFilename);
      end;
    end;
    // essai de transfert de fichier
    FTP_SetRemoteDirectory(QPath);
    FTP_SendFile(GetGHTopoDirectory() + '00000000_GrapheJS_Bidon.htm');
    FTP_EndConnexion();
  end
  else
  begin
    printf('Echec de connexion: %d - %s', [QReturnCode, QReturnMsg]);
  end;
end.
/*
 function  FTP_GetFileName(const Idx: integer): String;
    function  FTP_GetNbFilesOfLast(): integer;
    function  FTP_ListFilenamesOf(const Path: string): integer;

*/
