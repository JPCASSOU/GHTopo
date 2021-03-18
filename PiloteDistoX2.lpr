program PiloteDistoX2;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  PiloteDistoX2MainFrm, printer4lazarus, pack_powerpdf, lazopenglcontext, tachartlazaruspkg, pascalscript

  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainFormPilote, MainFormPilote);
  Application.Run;
end.

