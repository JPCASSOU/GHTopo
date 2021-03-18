unit DGCUtilityDialogs;
// Dialogues utilitaires (eg: Editeur de feuilles de styles

{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCUtilityFunctions,
  Classes, SysUtils,
  Forms, Dialogs, Controls,
  frmDGCStyleSheetEditor;

function EditStyleSheet(var FS: TDGCStyleSheet): boolean;
implementation

function EditStyleSheet(var FS: TDGCStyleSheet): boolean;
var
  TD: TdlgDGCStyleSheetEditor;
begin
  result := false;
  TD := TdlgDGCStyleSheetEditor.Create(Application);
  try
    if (TD.Initialiser(FS)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        FS := TD.GetStyleSheet();
        Result := True;
      end;
    end;

  finally
    FreeAndNil(TD);
  end;
end;

end.

