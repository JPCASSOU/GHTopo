unit CadreTextures;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common, Graphics,
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, curredit,
  Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients,
  SynEdit, SynHighlighterPas, SynEditHighlighter;

type

  { TCdrTextures }

  TCdrTextures = class(TFrame)
    btnRedigerCodeTexture: TButton;
    btnCopierTextePascal: TButton;
    cmbPerlinFunc: TComboBox;
    cmbResampleFilter: TComboBox;
    editColorA: TCurrencyEdit;
    editColorB: TCurrencyEdit;
    editColorG: TCurrencyEdit;
    editColorR: TCurrencyEdit;
    editCouleurHTML: TEdit;
    editCouleurKML: TEdit;
    editCouleurRGBA: TEdit;
    editCouleurSVG: TEdit;
    editExponent: TCurrencyEdit;
    editHorizontalPeriod: TCurrencyEdit;
    editLightColorA: TCurrencyEdit;
    editLightColorB: TCurrencyEdit;
    editLightColorG: TCurrencyEdit;
    editLightColorR: TCurrencyEdit;
    editNatureTexture: TEdit;
    editSizeTextureX: TCurrencyEdit;
    editTileOverlap: TCurrencyEdit;
    editVerticalPeriod: TCurrencyEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    pnlColor: TPanel;
    sclColorA: TScrollBar;
    sclColorB: TScrollBar;
    sclColorG: TScrollBar;
    sclColorR: TScrollBar;
    sclExponent: TScrollBar;
    sclHorizontalPeriod: TScrollBar;
    sclLightColorA: TScrollBar;
    sclLightColorB: TScrollBar;
    sclLightColorG: TScrollBar;
    sclLightColorR: TScrollBar;
    sclVerticalPeriod: TScrollBar;
    editCodePascalTexture: TSynEdit;
    SynPasTextures: TSynPasSyn;
    procedure btnCopierTextePascalClick(Sender: TObject);
    procedure btnRedigerCodeTextureClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure sclColorAChange(Sender: TObject);
    procedure sclColorBChange(Sender: TObject);
    procedure sclColorGChange(Sender: TObject);
    procedure sclColorRChange(Sender: TObject);
    procedure sclExponentChange(Sender: TObject);
    procedure sclHorizontalPeriodChange(Sender: TObject);
    procedure sclLightColorAChange(Sender: TObject);
    procedure sclLightColorBChange(Sender: TObject);
    procedure sclLightColorGChange(Sender: TObject);
    procedure sclLightColorRChange(Sender: TObject);
    procedure sclVerticalPeriodChange(Sender: TObject);
  strict private
    FMaTextureDeTest: TBGRABitmap;
    procedure DefineColorFromScrollbars();
    procedure RedigerPascalCodeForTexture();
  private
    function CreateAnyTexture(tx, ty: integer; const DoEcho: boolean): TBGRABitmap;
    function CreateCustomTexture(AWidth, AHeight: integer; HorizontalPeriod: Single=1; VerticalPeriod: Single=1; Exponent: Double=1; ResampleFilter: TResampleFilter=rfCosine): TBGRABitmap;

  public
    function Initialiser(): boolean;

  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}
//******************************************************************************
function MakeTRect(const Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Top     := Top;
  Result.Left    := Left;
  Result.Bottom  := Bottom;
  Result.Right   := Right;
end;

// pour les textures
function Interp256(const value1,value2,position: integer): integer; inline; overload;
begin
  result := (value1*(256-position) + value2*position) shr 8;
end;
function Interp256(const color1,color2: TBGRAPixel; const position: integer): TBGRAPixel; inline; overload;
begin
  result.red   := Interp256(color1.red,color2.red, position);
  result.green := Interp256(color1.green,color2.green, position);
  result.blue  := Interp256(color1.blue,color2.blue, position);
  result.alpha := Interp256(color1.alpha,color2.alpha, position);
end;

function CreateSandTexture(tx, ty: integer): TBGRABitmap;
begin

end;

// texture de pierre
function CreateStoneTexture(tx,ty: integer): TBGRABitmap;
 var
   temp: TBGRABitmap;
   phong: TPhongShading;
   WU: TRECT;
 begin
   result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,0.6);
   // GetPart crée également l'objet Temp
   WU := MakeTRect(-2,-2,tx+2,ty+2);
   temp:= result.GetPart(WU) as TBGRABitmap;
   phong := TPhongShading.Create;
   try
     phong.LightSourceDistanceFactor := 0;
     phong.LightDestFactor := 0;
     phong.LightSourceIntensity := 100;
     phong.LightPositionZ := 100;
     phong.NegativeDiffusionFactor := 0.3;
     phong.AmbientFactor := 0.5;
     phong.Draw(result,temp,30,-2,-2,BGRA(170,170,170));
   finally
     FreeAndNil(phong);
     FreeAndNil(temp);
   end;
 end;


// texture eau
function CreateWaterTexture(tx,ty: integer): TBGRABitmap;
const blurSize = 5;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
  QRect: TRect;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,2.6);
  QRect := MakeTRect(-blurSize, -blurSize, tx + blurSize, ty + blurSize);
  temp   := result.GetPart(QRect) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
  try
    phong := TPhongShading.Create;
    phong.LightSourceDistanceFactor := 0;
    phong.LightDestFactor := 0;
    phong.LightSourceIntensity := 150;
    phong.LightPositionZ := 80;
    phong.LightColor := BGRA(105,233,240);
    phong.NegativeDiffusionFactor := 0.3;
    phong.SpecularIndex := 20;
    phong.AmbientFactor := 0.4;
    phong.Draw(result,temp,20,-blurSize,-blurSize,BGRA(28,139,166));
  finally
    FreeAndNil(phong);
    FreeAndNil(temp);
  end;
end;

{ TCdrTextures }

procedure TCdrTextures.btnCopierTextePascalClick(Sender: TObject);
begin
  editCodePascalTexture.CopyToClipboard;
end;

procedure TCdrTextures.btnRedigerCodeTextureClick(Sender: TObject);
begin
  RedigerPascalCodeForTexture();
end;

procedure TCdrTextures.PaintBox1Paint(Sender: TObject);
const MG = 20;
var
  QTmpBuffer : TBGRABitmap;
begin
  try
    QTmpBuffer:= TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height, BGRA(192, 192, 192, 255));

    QTmpBuffer.CanvasBGRA.Pen.Color := clBlue;
    QTmpBuffer.CanvasBGRA.Pen.Width := 2;
    QTmpBuffer.CanvasBGRA.Brush.Texture := FMaTextureDeTest;

    QTmpBuffer.CanvasBGRA.Rectangle(MG,MG, PaintBox1.Width - MG, PaintBox1.Height - MG);





    QTmpBuffer.Draw(PaintBox1.Canvas, 0, 0, True);

  finally
    FreeAndNil(QTmpBuffer);//TmpBuffer.Free;
    // ne pas mettre cette ligne avant la libération du sémaphore
    Application.ProcessMessages;
  end;
end;





//******************************************************************************
procedure TCdrTextures.sclHorizontalPeriodChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;




procedure TCdrTextures.sclVerticalPeriodChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;
procedure TCdrTextures.sclExponentChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;


procedure TCdrTextures.sclColorAChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
  DefineColorFromScrollbars();
end;

procedure TCdrTextures.sclColorBChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
  DefineColorFromScrollbars();
end;

procedure TCdrTextures.sclColorGChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
  DefineColorFromScrollbars();
end;

procedure TCdrTextures.sclColorRChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
  DefineColorFromScrollbars();
end;



procedure TCdrTextures.sclLightColorAChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;

end;

procedure TCdrTextures.sclLightColorBChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TCdrTextures.sclLightColorGChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;



procedure TCdrTextures.sclLightColorRChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;


//******************************************************************************
function TCdrTextures.CreateCustomTexture(AWidth, AHeight: integer;
                                            HorizontalPeriod: Single = 1;
                                            VerticalPeriod: Single = 1;
                                            Exponent: Double = 1;
                                            ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;
 var
   colorOscillation: integer;
   p: PBGRAPixel;
   i: Integer;
   S1, S2: TBGRAPixel;
 begin
   result := CreateCyclicPerlinNoiseMap(AWidth,AHeight,
                                        HorizontalPeriod,
                                        VerticalPeriod,
                                        Exponent,
                                        ResampleFilter);


   S1 := BGRA(sclLightColorB.Position,
              sclLightColorG.Position,
              sclLightColorR.Position,
              sclLightColorA.Position);
   S2 := BGRA(sclColorB.Position,
              sclColorG.Position,
              sclColorR.Position,
              sclColorA.Position);

   p := result.Data;
   for i := 0 to result.NbPixels-1 do
   begin
     colorOscillation := round(((sin(p^.red*Pi/32)+1)/2)*256);
     p^ := Interp256(S1, S2, colorOscillation);
     inc(p);
   end;
 end;

function TCdrTextures.Initialiser(): boolean;
  procedure MiouMiou(const Miou: TSynHighlighterAttributes; const TextColor: TColor; const TextStyles: TFontStyles = []);
  begin
    Miou.Foreground := TextColor;
    Miou.Style      := TextStyles;
  end;
begin
  result := false;
  // créer une texture
  editSizeTextureX.AsInteger  := 128;
  editTileOverlap.AsInteger   := 4;
  FMaTextureDeTest := CreateWaterTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger);
  // combo resample
  cmbResampleFilter.Clear;
  cmbResampleFilter.Items.Add('rfBox');
  cmbResampleFilter.Items.Add('rfLinear');
  cmbResampleFilter.Items.Add('rfHalfCosine');
  cmbResampleFilter.Items.Add('rfCosine');
  cmbResampleFilter.Items.Add('rfBicubic');
  cmbResampleFilter.Items.Add('rfMitchell');
  cmbResampleFilter.Items.Add('rfSpline');
  cmbResampleFilter.Items.Add('rfLanczos2');
  cmbResampleFilter.Items.Add('rfLanczos3');
  cmbResampleFilter.Items.Add('rfLanczos4');
  cmbResampleFilter.Items.Add('rfBestQuality');
  cmbResampleFilter.DropDownCount := cmbResampleFilter.Items.Count;
  cmbResampleFilter.ItemIndex := 3; //rfcosine
  // éditeur de code
  MiouMiou(SynPasTextures.KeyAttri, clBlue, [fsBold]);
  MiouMiou(SynPasTextures.CommentAttri, clTeal, [fsItalic]);
  MiouMiou(SynPasTextures.StringAttri , clFuchsia);
  MiouMiou(SynPasTextures.NumberAttri , clNavy, [fsBold]);
  editCodePascalTexture.Clear;
  result := true;
end;

function TCdrTextures.CreateAnyTexture(tx, ty: integer; const DoEcho: boolean): TBGRABitmap;
const
  blurSize     = 5;
var
  WU           : TRect;
  temp         : TBGRABitmap;
  phong        : TPhongShading;
  QTileOverlap : LongInt;
begin
   QTileOverlap := editTileOverlap.AsInteger;
   case cmbPerlinFunc.ItemIndex of
     0: result := CreateCyclicPerlinNoiseMap(tx, ty,
                                        sclHorizontalPeriod.Position / 100.0,
                                        sclVerticalPeriod.Position / 100.0,
                                        sclExponent.Position / 100.0,
                                        TResampleFilter(cmbResampleFilter.ItemIndex)
                                        );
     1: result := CreatePerlinNoiseMap(tx, ty,
                                        sclHorizontalPeriod.Position / 100.0,
                                        sclVerticalPeriod.Position / 100.0,
                                        sclExponent.Position / 100.0,
                                        TResampleFilter(cmbResampleFilter.ItemIndex)
                                        );
     2: result := CreateCustomTexture(tx, ty,sclHorizontalPeriod.Position / 100.0,
                                        sclVerticalPeriod.Position / 100.0,
                                        sclExponent.Position / 100.0,
                                        TResampleFilter(cmbResampleFilter.ItemIndex)
                                        );
   end;

   // GetPart crée également l'objet Temp
   if (cmbPerlinFunc.ItemIndex in [0, 1]) then
   begin
     if (DoEcho) then
     begin
       editHorizontalPeriod.Value := sclHorizontalPeriod.Position / 100.0;
       editVerticalPeriod.Value   := sclVerticalPeriod.Position / 100.0;
       editExponent.Value         := sclExponent.Position / 100.0;

       editLightColorB.AsInteger := sclLightColorB.Position;
       editLightColorG.AsInteger := sclLightColorG.Position;
       editLightColorR.AsInteger := sclLightColorR.Position;
       editLightColorA.AsInteger := sclLightColorA.Position;
       editColorB.AsInteger      := sclColorB.Position;
       editColorG.AsInteger      := sclColorG.Position;
       editColorR.AsInteger      := sclColorR.Position;
       editColorA.AsInteger      := sclColorA.Position;
     end;
     WU := MakeTRect(-QTileOverlap, -QTileOverlap,
                      tx + QTileOverlap, ty + QTileOverlap);
     temp:= result.GetPart(WU) as TBGRABitmap;
     BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
     try
       phong := TPhongShading.Create;
       phong.LightSourceDistanceFactor := 0;
       phong.LightDestFactor           := 0;
       phong.LightSourceIntensity      := 150;
       phong.LightPositionZ            := 80;
       phong.LightColor                := BGRA(sclLightColorB.Position,
                                               sclLightColorG.Position,
                                               sclLightColorR.Position,
                                               sclLightColorA.Position);
       phong.NegativeDiffusionFactor   := 0.3;
       phong.SpecularIndex             := 20;
       phong.AmbientFactor             := 0.4;
       phong.Draw(result,temp,20,-blurSize,-blurSize,
                           BGRA(sclColorB.Position,
                                sclColorG.Position,
                                sclColorR.Position,
                                sclColorA.Position));




     finally
       FreeAndNil(phong);
       FreeAndNil(temp);
     end;
   end; //  if (cmbPerlinFunc.ItemIndex in [0, 1]) then
end;
procedure TCdrTextures.DefineColorFromScrollbars();
begin
  pnlColor.Color       := RGBToColor(sclColorR.Position, sclColorG.Position, sclColorB.Position);
  editColorR.AsInteger := sclColorR.Position;
  editColorG.AsInteger := sclColorG.Position;
  editColorB.AsInteger := sclColorB.Position;
  editColorA.AsInteger := sclColorA.Position;
  editCouleurHTML.text := ColorToHTMLColor(pnlColor.Color);
  editCouleurSVG.text  := SVGColor(pnlColor.Color);
  editCouleurKML.text  := KMLColor(sclColorR.Position, sclColorG.Position, sclColorB.Position, sclColorA.Position);
  //editCouleurRGBA.text := Format('$%.2Xd', [MakeTColorRGBA(sclRValue.Position, sclGValue.Position, sclBValue.Position, sclAValue.Position)]);
end;

procedure TCdrTextures.RedigerPascalCodeForTexture();
var
  EWE: String;
  procedure MiouMiou(const Miou: string);
  begin
    editCodePascalTexture.Lines.Add(Miou);
  end;
begin
  {
    function CreateTextureSand(): TBGRABitmap;
    const
      blurSize     = 5;
      tile_overlap = 4;
    var
      QTileOverlap : LongInt;
      i: integer
      S1, S2: TBGRAPixel;
      colorOscillation: integer;
      p: PBGRAPixel;
    begin
      QTileOverlap := 4;
      result := CreateCustomTexture(128, 128, 4, 1.33, 1.42, 0.60000, rfCosine);
      // GetPart crée également l'objet Temp
      // JESUS = HITLER -- FUCK THE CHRIST
      // Marble or wooden like texture
      result := CreateCyclicPerlinNoiseMap(128, 128, 4, 1.33, 1.42, 0.60000, rfCosine);
      S1 := BGRA(105, 233, 240, 255);
      S2 := BGRA(28, 139, 166, 255);
      p := result.Data;
      for i := 0 to result.NbPixels-1 do
      begin
        colorOscillation := round(((sin(p^.red*Pi/32)+1)/2)*256);
        p^ := Interp256(S1, S2, colorOscillation);
        inc(p);
      end;
    end;
  //}

  editCodePascalTexture.Clear();
  MiouMiou(Format('function CreateTexture%s(): TBGRABitmap;', [trim(editNatureTexture.Text)]));
  MiouMiou('const');
  MiouMiou(Format('  blurSize     = %d;', [5]));
  MiouMiou(Format('  tile_overlap = %d;', [4]));
  MiouMiou('var');
  MiouMiou('  QTileOverlap : LongInt;');
  if (cmbPerlinFunc.ItemIndex in [0, 1]) then
  begin
    MiouMiou('  WU      : TRect;');
    MiouMiou('  temp    : TBGRABitmap;');
    MiouMiou('  phong   : TPhongShading;');
  end;
  if (2 = cmbPerlinFunc.ItemIndex) then
  begin
    MiouMiou('  i: integer');
    MiouMiou('  S1, S2: TBGRAPixel;');
    MiouMiou('  colorOscillation: integer;');
    MiouMiou('  p: PBGRAPixel;');
  end;
  MiouMiou('begin');
    MiouMiou(Format('  QTileOverlap := %d;', [editTileOverlap.AsInteger]));
    case cmbPerlinFunc.ItemIndex of
      0: EWE := 'CreateCyclicPerlinNoiseMap';
      1: EWE := 'CreatePerlinNoiseMap';
      2: EWE := 'CreateCustomTexture';
    end;
    MiouMiou('  // GetPart crée également l''objet Temp');
    case (cmbPerlinFunc.ItemIndex) of
      0, 1:
      begin
        MiouMiou(Format(' result := %s(%d, %d, %d, %.2f, %.2f, %.5f, %s);',
                    [EWE,
                     editSizeTextureX.AsInteger,
                     editSizeTextureX.AsInteger,
                     editTileOverlap.AsInteger,
                     sclHorizontalPeriod.Position / 100.0,
                     sclVerticalPeriod.Position / 100.0,
                     sclExponent.Position / 100.0,
                     cmbResampleFilter.Items[cmbResampleFilter.ItemIndex]
                     ]));
        MiouMiou('  // Bumped procedural texture');
        MiouMiou('  WU := MakeTRect(-tile_overlap,-tile_overlap,tx+tile_overlap,ty+tile_overlap);');
        MiouMiou('  temp:= result.GetPart(WU) as TBGRABitmap;');


        MiouMiou('  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));');
        MiouMiou('  try');
        MiouMiou('    phong := TPhongShading.Create;');
        MiouMiou(Format('    phong.LightSourceDistanceFactor := %d;', [0]));
        MiouMiou(Format('    phong.LightDestFactor           := %d;', [0]));
        MiouMiou(Format('    phong.LightSourceIntensity      := %d;', [150]));
        MiouMiou(Format('    phong.LightPositionZ            := %d;', [80]));
        MiouMiou(Format('    phong.LightColor := BGRA(%d, %d, %d, %d);', [sclLightColorB.Position, sclLightColorG.Position, sclLightColorR.Position, sclLightColorA.Position]));
        MiouMiou('');
        MiouMiou(Format('    phong.NegativeDiffusionFactor   := %.2f; ', [0.3]));
        MiouMiou(Format('    phong.SpecularIndex             := %d;'  , [20]));
        MiouMiou(Format('    phong.AmbientFactor            := %.3f;', [0.4]));
        MiouMiou(Format('    phong.Draw(Result, Temp, %d, -blurSize,-blurSize, BGRA(%d, %d, %d, %d));', [20, sclColorB.Position, sclColorG.Position, sclColorR.Position,  sclColorA.Position]));
        MiouMiou('  finally');
        MiouMiou('    FreeAndNil(phong);');
        MiouMiou('    FreeAndNil(temp);');
        MiouMiou('  end;');

      end;
      2:
      begin
        MiouMiou('  // Marble or wooden like texture');
        MiouMiou(Format('  result := %s(%d, %d, %d, %.2f, %.2f, %.5f, %s);',
                    [EWE,
                     editSizeTextureX.AsInteger,
                     editSizeTextureX.AsInteger,
                     editTileOverlap.AsInteger,
                     sclHorizontalPeriod.Position / 100.0,
                     sclVerticalPeriod.Position / 100.0,
                     sclExponent.Position / 100.0,
                     cmbResampleFilter.Items[cmbResampleFilter.ItemIndex]
                     ]));
                     //*)
       MiouMiou(Format('  S1 := BGRA(%d, %d, %d, %d);', [sclLightColorB.Position, sclLightColorG.Position, sclLightColorR.Position, sclLightColorA.Position]));
       MiouMiou(Format('  S2 := BGRA(%d, %d, %d, %d);', [sclColorB.Position, sclColorG.Position, sclColorR.Position, sclColorA.Position]));
       MiouMiou('  p := result.Data;');
       MiouMiou('  for i := 0 to result.NbPixels-1 do');
       MiouMiou('  begin');
       MiouMiou('    colorOscillation := round(((sin(p^.red*Pi/32)+1)/2)*256);');
       MiouMiou('    p^ := Interp256(S1, S2, colorOscillation);');
       MiouMiou('    inc(p);');
       MiouMiou('  end;');
      end;
    end;
  MiouMiou('end;');
  try
    FMaTextureDeTest.SaveToFile(GetGHTopoDirectory() + 'MaTexture.png');
  except
    ShowMessage('Echec création du PNG de la texture');
  end;
end;

end.

