program GHTopoToolbox;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GHTopoToolboxMainFrm, laz_synapse, printer4lazarus, pack_powerpdf, rxnew, lazopenglcontext, datetimectrls, tachartaggpas, pascalscript, CadreTextures
  { you can add units after this }
  ;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

