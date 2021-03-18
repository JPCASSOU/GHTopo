program GHCaveDrawViewer;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, rxnew,
  pack_powerpdf, printer4lazarus,
  ViewerMainFrm,
  cdrdessincanvasBGRA,
  {$IFDEF MSWINDOWS}
  frmJournal,
  {$ENDIF}
  {$IFDEF MAC_OS_X_VERSION_MIN_REQUIRED}
  frmJournal,
  {$ENDIF}
  GeneralFunctions, GHCD_Types, UnitGraphismesProvisoires, UnitDocDessin,
  SVGCanvasUnit, UnitMessages_fr, CallDialogs, UnitCenterlinesBasePoints,
  UnitDessinOOo, UnitExportToODG, frmEditScraps, frmExportPDF,
  GeometryPolygonesUtils, UnitFindObjects, CdrListeGroupes, CdrHierarchieObjets,
  frmHierarchieObjets, UnitTGHCaveDrawDrawingContext, UnitAtlasHTML,
frmTImageObject, frmParametrageAtlas, unitSectionsTransversales, frmEditeurTexte;
  //frmGroupes,
  //frmPrintingCenter, frmTextObject, frmEditStyleCourbe, frmEditstyleTexte;
  { you can add units after this }

{$R *.res}

begin
  RequireDerivedFormResource := false;
  Application.Initialize;
  Application.CreateForm(TMainWndViewer, MainWndViewer);
  {$IFDEF MSWINDOWS}
  Application.CreateForm(TdlgProcessing, dlgProcessing);
  {$ENDIF}
  {$IFDEF MAC_OS_X_VERSION_MIN_REQUIRED}
  Application.CreateForm(TdlgProcessing, dlgProcessing);
  {$ENDIF}
  //Application.CreateForm(TdlgHierarchieObjets, dlgHierarchieObjets);
  //Application.CreateForm(TdlgTImageObject, dlgTImageObject);
  //Application.CreateForm(TdlgParametrageAtlas, dlgParametrageAtlas);
  //Application.CreateForm(TdlgEditeurTexte, dlgEditeurTexte);
  //Application.CreateForm(TdlgEditScrap, dlgEditScrap);
  //Application.CreateForm(TdlgExportPDF, dlgExportPDF);
  //Application.CreateForm(TdlgEditStyleCourbe, dlgEditStyleCourbe);
  //Application.CreateForm(TdlgEditStyleTexte, dlgEditStyleTexte);
  //Application.CreateForm(TdlgGroupe, dlgGroupe);
  //Application.CreateForm(TdlgPrintingCenter, dlgPrintingCenter);
  //Application.CreateForm(TdlgTextObject, dlgTextObject);
  Application.Run;
end.

