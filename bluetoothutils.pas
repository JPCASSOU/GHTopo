unit BlueToothUtils;

//{$INCLUDE CompilationParameters.inc}
{$MODE objfpc}

interface

uses
  Classes, SysUtils,
  Windows, winsock, WinSock2,
  BluetoothBase,
  Common;


type

{ TBlueToothConnexion }

 TBlueToothConnexion = class
  private
    FListeDevicesAddress: TStringList;
    function CheckVersion(VersionWord: Word): Boolean;

  public
    function Initialise: boolean;
    function Initialise2: boolean;
    procedure Finalise;
end;

implementation
{ TBlueToothConnexion }
function TBlueToothConnexion.CheckVersion(VersionWord: Word): Boolean;
Var
    wVersionRequested : Word;
    WsaData : TWsaData;
    Err : Integer;
begin
 CheckVersion := False;
{Socket version check:}
 WVersionRequested := VersionWord;      {Minimaal versie 2.2 van winsock nodig}
 Err := WSAStartup(wVersionRequested, WsaData);
 If WsaData.wVersion <> VersionWord then begin WsaCleanup; exit; end;
 If Err = 0 then CheckVersion := True;
End;

function TBlueToothConnexion.Initialise: boolean;
var
  FLG: Integer;
  Restrictions: LPWSAQuerySetA;
  Err: LongInt;
  HND: LPHANDLE;
  AF: TAFProtocols;
  PAF: ^TAFProtocols;
  WSA: TWSAVersion;
  PWSA : ^TWSAVersion ;
  function qCreateUID(): PGUID;
  var
    HR: HRESULT;
    UID: TGUID;
    P  : PGUID;
  begin
    HR := CreateGUID(UID);
    New(P);
    P^ := UID;
    Result := P;
  end;
begin
  Result := False;
  New(HND);
  AfficherMessage(Format('%s.Initialise', [ClassName]));
  if not Checkversion($0202) then
  begin
    AfficherMessage('Winsock2 version 2.2 NOT found!');
    Exit;
  end;
  AfficherMessage('Winsock2 version 2.2 found.');
  FLG := {LUP_DEEP AND LUP_NEAREST AND} LUP_RETURN_ALL;

  New(Restrictions);

  //FillChar(Restrictions, Sizeof(TWSAQuerySetW), 0);
  AfficherMessage(inttostr(Sizeof(TWSAQuerySetW)));
  //Restrictions^.;
  Restrictions^.dwSize                  := Sizeof(TWSAQuerySetW);
  AfficherMessage('001.3');
  Restrictions^.lpszServiceInstanceName := PChar('');
  AfficherMessage('001.3.1');

  Restrictions^.lpServiceClassId       := qCreateUID();
  AfficherMessage('001.4');

  WSA.dwVersion := 0;
  WSA.ecHow     := COMP_EQUAL;
  New(PWSA);
  PWSA^ := WSA;
  AfficherMessage('001.5');
  Restrictions^.lpVersion               := PWSA;
  AfficherMessage('001.6');
  Restrictions^.lpszComment             := PChar('');
  Restrictions^.dwNameSpace             := NS_ALL;
  Restrictions^.lpNSProviderId          := qCreateUID();
  AfficherMessage('001.7');
  Restrictions^.lpszContext             := PChar('');
  Restrictions^.dwNumberOfProtocols     := 0;
  AF.iAddressFamily := 0;
  AF.iProtocol      := 0;
  New(PAF);
  PAF^ := AF;
  Restrictions^.lpafpProtocols          := PAF;
  Restrictions^.lpszQueryString         := PChar('');
  Restrictions^.dwNumberOfCsAddrs       := 0;
  Restrictions^.lpcsaBuffer             := nil;
  Restrictions^.dwOutputFlags           := 0;
  Restrictions^.lpBlob                  := nil;







  AfficherMessage('002');
  Err := WSALookupServiceBegin(Restrictions, Flg, HND);
  AfficherMessage('003');

  if Err <> 0 then
  begin
     Err := WSAGetLastError;
     AfficherMessage('lookupservicebegin: '+inttostr(err));
     if Err = WSAEInval then AfficherMessage('invalid handle')
     else if Err = WSANO_Data then AfficherMessage('name found in db')
     else if Err = WSANOTINITIALISED then AfficherMessage('No proper initialization');
  end else
  begin
   AfficherMessage('Okidoki');
   Result := True;
  end;

  WsaCleanup;
end;

function TBlueToothConnexion.Initialise2: boolean;
begin
  Result := False;
  FListeDevicesAddress := TStringList.Create;
  FListeDevicesAddress.Clear;
  AfficherMessage(Format('%s.Initialise', [ClassName]));
  if not Checkversion($0202) then
  begin
    AfficherMessage('Winsock2 version 2.2 NOT found!');
    Exit;
  end;
  BluetoothDeviceDiscover(FListeDevicesAddress);
end;

procedure TBlueToothConnexion.Finalise;
begin
  AfficherMessage(Format('%s.Finalise', [ClassName]));
end;

end.

