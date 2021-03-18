program GHTopoRPI;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,    // A activer sur Raspberry
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  //, dlgSplashScreen
  { you can add units after this }
  , frmRPIMainWnd
  , TAChartLazarusPkg, datetimectrls         // Indispensable ici
  //, CadreRoseDiagramme, CadreDepthDiagramme  // A désactiver sur Raspberry
  ;
{$R *.res}
begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  try
    Application.CreateForm(TRPIMainWnd, RPIMainWnd);
  finally
    //frmSplashScreen.Free;
  end;
  Application.Run;
end.
