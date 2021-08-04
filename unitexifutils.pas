unit UnitExifUtils;
// EXIF
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Classes, SysUtils, math,
  fpeMetaData, fpeGlobal, fpeTags, fpeExifData, fpeIPTCData, typinfo,
  Grids
  ;
function BytesToStr(B: fpeGlobal.TBytes; MaxLen: Integer): String;
type

{ TImgInfoWithGPS }

 TImgInfoWithGPS = class(TImgInfo)
  private
  public
    function  ExtractWGSCoordinates(out Lat, Lon: double; out Msg: string): boolean;
    procedure DisplayMetadataInGrid(const ExifGrid: TStringGrid);
end;

implementation
function BytesToStr(B: fpeGlobal.TBytes; MaxLen: Integer): String;
var
  i, n: Integer;
  suffix: String = '';
begin
  Result := '';
  n := Length(B);
  if n = 0 then
    exit;
  Result := Format('%.2x', [B[0]]);
  if n > MaxLen then
  begin
    n := MaxLen;
    suffix := '...';
  end;
  for i := 1 to n-1 do
    Result := Format('%s %.2x', [Result, B[i]]);
  Result := Result + suffix;
end;

{ TImgInfoWithGPS }

function TImgInfoWithGPS.ExtractWGSCoordinates(out Lat, Lon: double; out Msg: string): boolean;
const
  TAG_GPSLatitude      : string = 'GPSLatitude';
  TAG_GPSLongitude     : string = 'GPSLongitude';
  TAG_GPSLatitudeRef   : string = 'GPSLatitudeRef';
  TAG_GPSLongitudeRef  : string = 'GPSLongitudeRef';
var
  QTextLat, QTextLon, QLatRef, QLonRef: TTag;
  QRefLon, QRefLat: string;

begin
  Msg := 'Coordonnées invalides';
  result := false;
  QTextLat := nil;
  QTextLon := nil;
  QLatRef  := nil;
  QLonRef  := nil;
  QTextLat := ExifData.FindTagByName(TAG_GPSLatitude);
  QTextLon := ExifData.FindTagByName(TAG_GPSLongitude);
  QLatRef  :=  ExifData.FindTagByName(TAG_GPSLatitudeRef);
  QLonRef  :=  ExifData.FindTagByName(TAG_GPSLongitudeRef);
  if (QTextLat = nil) then begin Msg := 'GPS Lat HS'; exit; end;
  if (QTextLon = nil) then begin Msg := 'GPS Lon HS'; exit; end;
  if (QLatRef  = nil) then begin Msg := 'GPS LatRef HS'; exit; end;
  if (QLonRef  = nil) then begin Msg := 'GPS LonRef HS'; exit; end;

  QRefLat := IIF(Pos('S', QLatRef.AsString) > 0, 'S', 'N');
  QRefLon := IIF(Pos('W', QLonRef.AsString) > 0, 'W', 'E');
  AfficherMessageErreur(QRefLon + ' ' + QRefLat);

  Lat := DegMinSec2DegDec(QTextLat.AsString + QRefLat);
  Lon := DegMinSec2DegDec(QTextLon.AsString + QRefLon);
  Result  := InRange(Lat, -90.00, 90.00) AND
             InRange(Lon, -180.0, 180.0);

  AfficherMessageErreur(Format('GPS %s: %.5f, %.5f', [booltostr(Result, 'OK', 'HS'), Lat, Lon]));

end;

procedure TImgInfoWithGPS.DisplayMetadataInGrid(const ExifGrid: TStringGrid);
const
  COL_KEY          = 0;
  COL_VALUE        = 1;
  COL_Description  = 2;
  COL_TagID        = 3;
  COL_TagGroup     = 4;
  COL_Tag          = 5;
  COL_TagType      = 6;
  COL_Count        = 7;
  COL_Endian       = 8;
  COL_RAW_DATA     = 9;
var
  i, row, w, NbTagsExif: Integer;
  lTag: TTag;
  suffix, QMsg: String;
  QLat, QLon: double;
  EWE: Boolean;
begin
  if (HasExif) then
  begin
    //FImageOrientation := FImgInfo.ExifData.ImgOrientation;
    self.ExifData.ExportOptions := self.ExifData.ExportOptions + [eoTruncateBinary];



    ExifGrid.RowCount := self.ExifData.TagCount + 1;

    ExifGrid.ColCount := 10;
    ExifGrid.Cells[COL_KEY         , 0] := 'Name';           //** Key
    ExifGrid.Cells[COL_VALUE       , 0] := 'Value';          //** Value
    ExifGrid.Cells[COL_Description , 0] := 'Description';
    ExifGrid.Cells[COL_TagID       , 0] := 'Tag ID';
    ExifGrid.Cells[COL_TagGroup    , 0] := 'Tag Group';
    ExifGrid.Cells[COL_Tag         , 0] := 'Tag';
    ExifGrid.Cells[COL_TagType     , 0] := 'Tag type';


    ExifGrid.Cells[COL_Count       , 0] := 'Count';
    ExifGrid.Cells[COL_Endian      , 0] := 'Endian';

    ExifGrid.Cells[COL_RAW_DATA    , 0] := 'Raw data';

    ExifGrid.FixedCols := 0;
    NbTagsExif := self.ExifData.TagCount;
    if (0 = NbTagsExif) then exit;
    for i := 0 to NbTagsExif - 1 do
    begin
      row := i+1;
      lTag := self.ExifData.TagByIndex[i];
      (*
      if lTag is TMakerNoteStringTag then
        suffix := ':' + IntToStr(TMakerNoteStringTag(lTag).Index)
      else if lTag is TMakerNoteIntegerTag then
        suffix := ':' + IntToStr(TMakerNoteIntegerTag(lTag).Index)
      else if lTag is TMakerNoteFloatTag then
        suffix := ':' + IntToStr(TMakerNoteFloatTag(lTag).Index)
      else

      //*)
      suffix := '';
      if lTag is TVersionTag then TVersionTag(lTag).Separator := '.';
      ExifGrid.Cells[COL_KEY         , row] := lTag.Name;                 //** Key
      ExifGrid.Cells[COL_VALUE       , row] := lTag.AsString;             //** Value
      ExifGrid.Cells[COL_Description , row] := lTag.Description;

      ExifGrid.Cells[COL_TagID       , row] := Format('%0:d ($%0:4x)', [lTag.TagIDRec.Tag]);
      ExifGrid.Cells[COL_TagGroup    , row] := NiceGroupNames[lTag.Group];
      ExifGrid.Cells[COL_Tag         , row] := Format('$%.04x:$%.04x%s', [lTag.TagIDRec.Parent, lTag.TagIDRec.Tag, suffix]);


      ExifGrid.Cells[COL_TagType     , row] := GetEnumName(TypeInfo(TTagType), integer(lTag.TagType));
      ExifGrid.Cells[COL_Count       , row] := IntToStr(lTag.Count);
      ExifGrid.Cells[COL_Endian      , row] := IIF(lTag.BigEndian, 'BE', 'LE');

      ExifGrid.Cells[COL_RAW_DATA, row] := BytesToStr(lTag.RawData, 16);
    end;
    // mise à largeur des colonnes selon contenu
    for i:= 0 to ExifGrid.ColCount-1 do
    begin
      w := ExifGrid.Canvas.TextWidth(ExifGrid.Cells[i, 0]);
      for row := 1 to ExifGrid.RowCount-1 do
        w := Max(w, ExifGrid.Canvas.TextWidth(ExifGrid.Cells[i, row]));
      ExifGrid.ColWidths[i] := w + 4*varCellPadding;
    end;
  end;

end;

end.

