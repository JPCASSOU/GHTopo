// exemple de script utilisant PascalScript
program Toto;
var
  MaillageStatus : integer;
  S            : string;
  i, Nb,  EWE  : integer;
  X0, Y0       : double;
  X1, Y1       : double;
  Pas          : double;

  PtInX , PtInY : double;
  PtOutX, PtOutY: double;
  QV: extended;
  Declimag: double;
begin
  print('Cavité: ' + GetNomEtude());
  print('====================');
  printf('%d espaces de noms', [GetNbNamespaces()]);
  printf('%d entrées', [GetNbEntrees()]);
  printf('%d séries', [GetNbSeries()]);
  printf('%d réseaux', [GetNbReseaux()]);
  printf('%d secteurs', [GetNbSecteurs()]);
  printf('%d expés', [GetNbExpes()]);
  printf('%d codes', [GetNbCodes()]);
  printf('%d antennes', [GetNbAntennes()]);
  print('******************************************');
  EWE := 42;
  print(GetGHTopoDirectory());
  //EWE := SelectionCouleurToporobot(EWE);
  printf('EWE = %d', [EWE]);
  PtInX := 44.25;     PtInY := -0.015;
  print('Déclinaison Magnétique');
  Declimag := CalculerDeclinaisonMagnetique( PtInX,  PtInY, 1200, 2001, 05, 28);
  printf('Déclinaison magnétique: %.8f', [Declimag]);
  ConversionCoordonneesIsoleesEPSG(4326, 2154, PtInX, PtInY, PtOutX, PtOutY);
  print(Format('In: Lon = %.8f, Lat = %.8f; Out: X = %.3f, Y= %.3f', [PtInX, PtInY, PtOutX, PtOutY]));
  print(KMLColor(255,128,112,255));
  PtInX := 30.00;     PtInY := 40.00;
  printf('L = %.3f',  [hypot2D(PtInX, PtInY)]);
  printf('L = %.3f',  [hypot3D(30.00, 45.00, 60.00)]);
  printf('Az = %.3f',  [GetAzimut(150.00, 10.00, 360.00)]);
  print(UpperCase('Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch'));
  print(ConvertirEnFiltreSeries('2335;2357;2374;2343;2347;2349;2350;2352;2354;6038;2334;2336;2376;2351;2337;2355;2358-2361;2364;2367;2340;2221-2291;6001-6128'));
  // Maillages
  print('Chargement d''un maillage');
  MaillageStatus := MNT_LoadMaillage(GetGHTopoDirectory() + 'castet_miu.mai');
  case MaillageStatus of
    -64: print('*** Maillage non assigné');
     -1: print('*** Erreur en chargement du maillage');
      0: printf('Maillage chargé: %d noeuds et %d triangles', [MNT_GetNbVertex(), MNT_GetNbTriangles()]);
      1: printf('Maillage valide déjà en mémoire: %d noeuds et %d triangles', [MNT_GetNbVertex(), MNT_GetNbTriangles()]);
  else
  end;
  if (MaillageStatus >= 0) then
  begin
    MNT_ClearProfils();
    X0 :=  402700.00;
    Y0 := 3090400.00;
    X1 :=  403600.00;
    Y1 := 3091250.00;

    Pas := 20.00;
    printf('Pas = %.2f', [Pas]);
    MNT_ClearProfils();
    for i := 0 to 50 do
    begin

      S := sprintf('Profil_%d_X_%d', [i, round(X0 + Pas * i)]);
      printf('-- Adding profil: %d: %s', [i, S]);
      MNT_AddProfil(S, X0 + Pas * i, Y0, X0 + Pas * i, Y1);

    end;
    printf('-- %d profils added', [MNT_GetNbProfils()]);
    for i := 0 to 50 do
    begin

      S := sprintf('Profil_%d_X_%d', [i, round(X0 + Pas * i)]);
      printf('-- Adding profil: %d: %s', [i, S]);
      MNT_AddProfil(S, X0, Y0 + Pas * i, X1, Y0  + Pas * i);

    end;
    printf('-- %d profils added', [MNT_GetNbProfils()]);

  end;
  // export stations
  (*
 if (BeginListeFichesStations()) then
  begin
    Nb := getnbstationsofseriebynoserie(1);
    printf('%d stations', [Nb]);
    for i := 1 to Nb - 1 do AddFicheStation(1, i);
    ExportFichesToPDF(GetGHTopoDirectory() + 'toto.pdf', false);
    EndListeFichesStations();
  end;
  //*)
end.
