unit UnitRemoteFunctions;
// Tranfert de fichiers et de dossiers via FTP vers le site synclinal
//********************************************************************
// Utilisable par GHTopo (notamment pour publication des graphes)


{$mode delphi}

interface

uses
  StructuresDonnees,
  Common,
  Classes, SysUtils,
  ftpsend
  ;
type

{ TRemoteServicesFTP }

 TRemoteServicesFTP = class(TFTPSend)
  private
    FCurrentRemoteDirectory : string;
    FDirList                : TStringList;

  public
    procedure SetCurrentRemoteDirectory(const PathFromRoot: TStringDirectoryFilename);
    function  GetCurrentRemoteDirectory(): TStringDirectoryFilename;

    function  SetParamsAndConnect(const QHost: string; const QPort: integer; const QUser, QPassword: string; out QReturnCode: integer; out QReturnMsg: string): boolean;
    procedure Finaliser();
    function  SendFile(const QFilename: TStringDirectoryFilename): boolean;

    function  ListFilenamesOf(const Path: string): integer;
    function  GetFileName(const Idx: integer): String;
    function  GetNbFilesOfLast(): integer;
    function  CreateDirectory(const PathFromRoot, QDirectory: TStringDirectoryFilename; const YAller: boolean): boolean;

end;


implementation



{ TRemoteServicesFTP }



procedure TRemoteServicesFTP.SetCurrentRemoteDirectory(const PathFromRoot: TStringDirectoryFilename);
begin
  AfficherMessageErreur(format('%s --> %s', [FCurrentRemoteDirectory, PathFromRoot]));
  FCurrentRemoteDirectory := PathFromRoot;
end;

function TRemoteServicesFTP.GetCurrentRemoteDirectory(): TStringDirectoryFilename;
begin
  result := FCurrentRemoteDirectory;
end;

function TRemoteServicesFTP.SetParamsAndConnect(const QHost: string; const QPort: integer; const QUser, QPassword: string; out QReturnCode: integer; out QReturnMsg: string): boolean;
begin
  result := false;
  self.BinaryMode   := True;
  self.TargetHost   := QHost;
  self.TargetPort   := IntToStr(QPort);
  self.UserName     := QUser;
  Self.Password     := QPassword;
  AfficherMessageErreur(Format('Trying connexion: Host: "%s", Port: %s, User: "%s"', [self.TargetHost, self.TargetPort, self.UserName]));
  Result := self.Login;
  QReturnCode := self.ResultCode;
  QreturnMsg  := self.ResultString;


  if (not result) then exit;
  //result := (350 = QReturnCode);
  AfficherMessageErreur(Format('Connected: %d - %s', [self.ResultCode, self.ResultString]));
  self.ChangeToRootDir;
  FCurrentRemoteDirectory := '/';
end;

function TRemoteServicesFTP.ListFilenamesOf(const Path: string): integer;
var
  EWE: Boolean;
begin
  Result := 0;
  FCurrentRemoteDirectory := Path;
  EWE := self.List(Path, false);
  if (not EWE) then exit(0);
  Result := FtpList.Count;
end;
function TRemoteServicesFTP.GetNbFilesOfLast(): integer;
begin
  result := FtpList.Count;
end;

function TRemoteServicesFTP.CreateDirectory(const PathFromRoot, QDirectory: TStringDirectoryFilename; const YAller: boolean): boolean;
var
  DD: String;
begin
  result := false;
  FCurrentRemoteDirectory := PathFromRoot;
  DD := FCurrentRemoteDirectory + '/' + QDirectory;
  AfficherMessageErreur('Création du dossier ' + DD);
  self.CreateDir(DD);
end;



function TRemoteServicesFTP.GetFileName(const Idx: integer): String;
var
  EWE: TFTPListRec;
begin
  if (0 = FtpList.Count) then exit('');
  EWE := FtpList.Items[Idx];
  Result := EWE.FileName;
end;

procedure TRemoteServicesFTP.Finaliser();
begin
  self.Logout;
  FtpList.Clear;
end;

function TRemoteServicesFTP.SendFile(const QFilename: TStringDirectoryFilename): boolean;
begin
  result := false;
  AfficherMessageErreur(Format('Envoi de %s vers %s', [QFilename, FCurrentRemoteDirectory]));
  //if (not FileExists(QFilename)) then exit;
  //try
    self.ChangeToRootDir;
    self.ChangeWorkingDir(FCurrentRemoteDirectory);

    self.DataStream.LoadFromFile(QFilename);
    Result := self.StoreFile(ExtractFileName(QFilename), True);
    AfficherMessageErreur(format('%s - %s', [ExtractFileName(QFilename), BoolToStr(Result, 'OK', 'KO')]));
  //except
    pass;
  //end;
end;



end.
////////////////////////////////////////////////////////////////////////////////

function FtpGetDir(const IP, Port, Path, User, Pass: string; DirList: TStringList): Boolean;
var
  i: Integer;
  s: string;
begin
  Result := False;
  with TFTPSend.Create do
  try
    Username := User;
    Password := Pass;
    TargetHost := IP;
    TargetPort := Port;
    if not Login then
      Exit;
    Result := List(Path, False);
    for i := 0 to FtpList.Count -1 do
    begin
      s := FTPList[i].FileName;
      DirList.Add(s);
    end;
    Logout;
  finally
    Free;
  end;
end;

