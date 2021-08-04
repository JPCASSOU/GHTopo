program GHTopoGM;

{$mode delphi}{$H+}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Interfaces // this includes the LCL widgetset
  , Forms
  , frmGHTopoMainBar
  { you can add units after this }
  , TAChartLazarusPkg; //, CadrePascalScript, frmPSDrawing2D, UnitRemoteFunctions, laz_synapse;
{$R *.res}
begin

  Application.Scaled := True;
  Application.Title:='GHTopoFPC';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  //frmSplashScreen := TfrmSplashScreen.Create(Application);
  try
    //ShowMessage('Vous devez apostasier pour pouvoir utiliser GHTopo. L''utilisation de ce logiciel est interdite aux chrétiens');
    //frmSplashScreen.Show;
    //frmSplashScreen.Update;
    // Create your application forms here
    Application.CreateForm(TGHTopoMainMenuBar, GHTopoMainMenuBar);
  finally
  end;
  Application.Run;
end.


