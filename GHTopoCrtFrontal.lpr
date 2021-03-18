program GHTopoCrtFrontal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  CRT, CRTUtils, unitCRTObjects;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg, QText, WU, S: String;
  w, ch: Char;
  i, m, n, Q: Integer;
  EWE: byte;
  MenuItems : array of string;
  MyWnd: TCRTModalWindow;
  MyListBox: TCRTListBox;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  CLS();
  while(True) do
  begin
    CRTDrawFenetre(1,1, 75, 20, 7, 0, MOTIF_CADRE_WNDS, false, 'toto');
    for i := 1 to 15 do
    begin
      EWE := 180+i;
      LocatePrint(2, 2 + i, Format('%d %s', [EWE, chr(EWE)]));
    end;
    SetLength(MenuItems, 7);
    MenuItems[0] := 'Sous-menu';
    MenuItems[1] := 'Editeur de ligne';
    MenuItems[2] := 'Boite d''information';
    MenuItems[3] := 'Fenetre dynamique';
    MenuItems[4] := 'Liste défilante';
    MenuItems[5] := 'Petit formulaire';

    MenuItems[6] := 'Quitter';
    m := CRTMenu(3,5, 60, 15, 1, 'Menu principal', MenuItems);
    case m of
      0: begin
           n := CRTMenu(14, 9, 20, 15, 1, MenuItems[m], ['toto', 'tata', 'titi', 'lulu', 'mimi']);
         end;
      1: begin
           // éditeur de ligne
           QText := 'Long texte à éditer';
           CRTEditLine(5, 22, 50, 4, 15, QText);

           CRTMessageDlg([QText]);
         end;
      2: CRTMessageDlg(['Premiere ligne', 'Deuxieme ligne', 'Troisieme ligne', 'lfjlhgkmhflhkljfdhlsdfkj hgkjgjk']);

      3: begin
           MyWnd := TCRTModalWindow.Create;
           try
             MyWnd.WinInit(10,8,60,10, 6, 9, MOTIF_CADRE_WNDS, true, 'Fenetre dynamique');
             LocatePrint(10,24, InttoStr(MyWnd.ModalResult));
           finally
             MyWnd.Free;
           end;
         end;
      4: begin
           MyListBox := TCRTListBox.Create;
           try
             MyListBox.Initialise(7, 2, 60, 20, 1, 15, MOTIF_CADRE_WNDS, True, 'Ma liste');
               for i := 0 to 90 do MyListBox.AddItem(Format('Element de liste %d', [i]));
             MyListBox.ShowModal();
             Q := MyListBox.GetSelectedIndex();
             S := MyListBox.GetSelectedItem();
             CRTMessageDlg([Format('Item sélectionné: %d - %s', [Q, S])]);
             MyListBox.Finalise();

           finally
             MyListBox.Free;
           end;
           //*)
         end;
      5: // petit formulaire
         begin
           CRTDrawFenetre(6, 10, 50, 4, 4, 15, MOTIF_CADRE_WNDS, True, 'Petit formulaire');
           LocatePrint(8, 11, 'Nom');
           LocatePrint(8, 12, 'Prenom');
           LocatePrint(25, 11, 'CASSOUX');
           LocatePrint(25, 12, 'Jean-Kul');

           CRTEditLine(25, 11, 50, 4, 15, QText);
           repeat

             ch := ReadKey;
           until (ch = #27) or (ch = #13);
         end;
      6: begin
           break;
         end;
    else
      ;
    end;
  end;
  CRT.TextBackground(0);
  CRT.TextColor(15);
  CLS();
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.

