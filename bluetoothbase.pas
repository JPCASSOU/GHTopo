{
Bluetooth aygıt tarama ünitesi (Bluetooth device discovery unit)
Ağustos 2005, Furkan Duman (coderlord)

LGPL lisansı altında dilediğinizi yapabilirsiniz.
Winsock2 unitesi Jedi Projesinden alınmış,
Bluetooth API kısmı için msdn.microsoft.com adresi kurcalanmıştır.

GNU LESSER GENERAL PUBLIC LICENSE
Version 2.1, February 1999

http://www.gnu.org/licenses/lgpl.txt
}
unit BluetoothBase;

interface

uses SysUtils, Windows, WinSock2, winsock, Classes;
type
{$IFDEF UNICODE}
  TWSAQuerySet  = TWSAQuerySetA;
  PWSAQuerySet  = PWSAQuerySetW;
  LPWSAQuerySet = PWSAQuerySetW;
{$ELSE}
  TWSAQuerySet  = TWSAQuerySetA;
  PWSAQuerySet  = PWSAQuerySetA;
  LPWSAQuerySet = PWSAQuerySetA;
{$ENDIF}

function BluetoothDeviceDiscover(ADeviceAddressList: TStrings): Boolean;

implementation
const  NS_BTH = 16;
type
  TBTH_ADDR = Int64;
  PSOCKADDR_BTH = ^TSOCKADDR_BTH;
  TSOCKADDR_BTH = packed record
    addressFamily: Word;
    btAddr: TBTH_ADDR;
    ServiceClassID: TGUID;
    Port: DWORD;
  end;




function BluetoothAddressToStr(AAddress: Int64): string;
type
  TBTAddr = record
  A1: Byte;
  A2: Byte;
  A3: Byte;
  A4: Byte;
  A5: Byte;
  A6: Byte;
  A7: Byte;
  A8: Byte;
end;
begin
  with TBTAddr(AAddress) do
    Result:= Format('%.2x:%.2x:%.2x:%.2x:%.2x:%.2x',
    [A6, A5, A4, A3, A2, A1]);
end;

function BluetoothDeviceDiscover(ADeviceAddressList: TStrings): Boolean;
var
  wsaqs: PWSAQuerySet;
  Buffer: array[0..4095] of Byte; // 4Kb lık tampon.
  pwsaResults: LPWSAQUERYSET;
  dwSize: DWORD;
  HLookup: PHANDLE;
begin
  ADeviceAddressList.Clear;
  Result:= False;

  FillChar(wsaqs, SizeOf(TWSAQuerySet), 0);
  wsaqs.dwSize:= SizeOf(TWSAQuerySet);
  wsaqs.dwNameSpace:= NS_BTH; // NS_BTH = 16

  // Bi umut varsa
  if WSALookupServiceBegin(wsaqs, LUP_CONTAINERS or LUP_FLUSHCACHE, HLookup) = 0 then
  begin
    // Bunun yerine pwsaResults array de olabilirmiş. Hilekar bir tavır izledik.
    pwsaResults:= @Buffer;
    FillChar(pwsaResults^, SizeOf(TWSAQuerySet), 0);
    pwsaResults^.dwSize:= SizeOf(TWSAQuerySet);
    pwsaResults^.dwNameSpace:= NS_BTH;
    dwSize:= SizeOf(Buffer);
    try
      // Artık bulamayana kadar
      while WSALookupServiceNext(HLookup^, LUP_RETURN_ADDR, // İsim de almak için LUP_RETURN_NAME or
            dwSize, pwsaResults) = 0 do
      begin
        // İsim de almak istersen. Bana ID si lazımdı.
        // ADeviceList.Append(pwsaResults^.lpszServiceInstanceName);
        ADeviceAddressList.Append(BluetoothAddressToStr( // Adresi aldık
        PSOCKADDR_BTH(pwsaResults^.lpcsaBuffer^.RemoteAddr.lpSockaddr).btAddr));
        Result:= True;
      end;
    finally
      // WSANext dedin ya. Sonra bunu demeden olmuyor işte.
      WSALookupServiceEnd(HLookup^);
    end;
  end;
end;

var
WSData: TWSAData; // 396 byte beleşten yer kaplayan
// WSAStartup için gerekli bişi. Dinamik allocate etsek
// 4 byte pointer tutacak + bi sürü kod. Boşveeer...
initialization
// Discoverın çalışması için açılışta çalışmak zorunda
WSAStartup($100, WSData);
finalization
// WSAStartup yapıldıysa WSACleanup da yapılmalı
WSACleanup;
end.
