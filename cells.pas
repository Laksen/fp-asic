unit cells;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, geometry;

type
  TLayerType = (ltUnknown, ltCut, ltRouting, ltPoly);

  TLayer = class
  private
    fIndex: longint;
    fLayerType: TLayerType;
    fName: string;
    fPitch,
    fSpacing,
    fWidth: double;
  public
    constructor Create;

    property LayerType: TLayerType read fLayerType write fLayerType;
    property Spacing: double read fSpacing write fSpacing;
    property Pitch: double read fPitch write fPitch;
    property Width: double read fWidth write fWidth;

    property Index: longint read fIndex write fIndex;
    property Name: string read fName write fName;
  end;

  TPoly = record
    Poly: TPolygon;
    Layer: TLayer;
  end;

  TPinDirection = (pdUnknown, pdIn, pdOut, pdInOut);
  TPinClass = (pcNone, pcPad, pcClock, pcPower);
  TPinShape = (psDefault, psAbutment);

  PPin = ^TPin;
  TPin = record
    Name: string;
    Polygons: array of TPoly;
    Direction: TPinDirection;
    PinClass: TPinClass;
    Shape: TPinShape;
  end;

  TCellClass = (ccNone,
                ccPad, ccCore, ccBlock, ccCover, ccRing,
                ccEndCapTL, ccEndCapTR, ccEndCapBL, ccEndCapBR);

  TCell = class
  private
    fCellClass: TCellClass;
    fName: string;
    fPinsDefined: boolean;
    fPolys, fObs: array of TPoly;
    fPins: array of TPin;
    fSize: TCoordinate;
    function GetArea: TArea;
    function GetObstructionCount: longint;
    function GetObstructions(AIndex: longint): TPoly;
    function GetPins(AIndex: longint): TPin;
    function GetPinsCount: longint;
    function GetPolygonCount: longint;
    function GetPolygons(AIndex: longint): TPoly;
  public
    constructor Create(const AName: string);

    procedure AddPoly(const APoly: TPoly);
    procedure AddObstruction(const AObs: TPoly);
    procedure AddPin(const AName: string; const APoly: TPoly; ADirection: TPinDirection; AClass: TPinClass; AShape: TPinShape);

    function FindPin(const AName: string): PPin;

    property Name: string read fName;
    property PinsDefined: boolean read fPinsDefined;
    property Area: TArea read GetArea;
    property Size: TCoordinate read fSize write fSize;
    property CellClass: TCellClass read fCellClass write fCellClass;

    property PolygonCount: longint read GetPolygonCount;
    property Polygons[AIndex: longint]: TPoly read GetPolygons;

    property ObstructionCount: longint read GetObstructionCount;
    property Obstructions[AIndex: longint]: TPoly read GetObstructions;

    property PinsCount: longint read GetPinsCount;
    property Pins[AIndex: longint]: TPin read GetPins;
  end;

  TVia = class(TCell)
  end;

var
  CoreSize: TCoordinate;

function GetLayerRect(ALayer: TLayer; const AStart, AStop: TCoordinate): TPoly;

function GetLayerCount: longint;
function GetLayer(const AIndex: longint): TLayer;
function GetLayer(const AName: string): TLayer;
function GetRoutingLayerCount: longint;

function HasCell(const AName: string): boolean;
function FindCell(const AName: string): TCell;

function GetCellCount: longint;
function GetCell(AIndex: longint): TCell;

procedure RegisterCell(ACell: TCell);

implementation

var
  Layers,
  CellDB: TStringList;

function GetLayerRect(ALayer: TLayer; const AStart, AStop: TCoordinate): TPoly;
begin
  result.Layer:=ALayer;
  Result.Poly:=GetRect(AStart, AStop);
end;

function GetLayerCount: longint;
begin
  result:=Layers.Count;
end;

function GetLayer(const AIndex: longint): TLayer;
begin
  result:=TLayer(Layers.Objects[AIndex]);
end;

function GetLayer(const AName: string): TLayer;
var
  idx: Integer;
begin
  if Layers.Find(AName, idx) then
    result:=TLayer(Layers.Objects[idx])
  else
  begin
    result:=TLayer.Create;
    result.Name:=AName;
    Layers.AddObject(AName, result);
  end;
end;

function GetRoutingLayerCount: longint;
var
  i: longint;
begin
  result:=0;

  for i:=0 to Layers.Count-1 do
    if TLayer(Layers.Objects[i]).LayerType=ltRouting then
      inc(result);
end;

function HasCell(const AName: string): boolean;
var
  idx: Integer;
begin
  result:=CellDB.Find(AName, idx);
end;

function FindCell(const AName: string): TCell;
var
  idx: Integer;
begin
  if CellDB.Find(AName, idx) then
    result:=TCell(CellDB.Objects[idx])
  else
    raise exception.CreateFmt('Failed to find cell: "%s"', [AName]);
end;

function GetCellCount: longint;
begin
  result:=CellDB.Count;
end;

function GetCell(AIndex: longint): TCell;
begin
  result:=TCell(CellDB.Objects[AIndex]);
end;

procedure RegisterCell(ACell: TCell);
begin
  CellDB.AddObject(ACell.Name, ACell);
end;

constructor TLayer.Create;
begin
  inherited Create;
  fName:='';
  fSpacing:=0;
  fPitch:=0;
  fLayerType:=ltUnknown;
  fWidth:=0;
end;

function TCell.GetPolygonCount: longint;
begin
  result:=Length(fPolys);
end;

function TCell.GetObstructionCount: longint;
begin
  result:=Length(fObs);
end;

function TCell.GetArea: TArea;
begin
  result:=fSize.X*fSize.y;
end;

function TCell.GetObstructions(AIndex: longint): TPoly;
begin
  result:=fObs[AIndex];
end;

function TCell.GetPins(AIndex: longint): TPin;
begin
  result:=fPins[AIndex];
end;

function TCell.GetPinsCount: longint;
begin
  result:=Length(fPins);
end;

function TCell.GetPolygons(AIndex: longint): TPoly;
begin
  result:=fPolys[AIndex];
end;

constructor TCell.Create(const AName: string);
begin
  inherited Create;
  setlength(fPolys, 0);
  setlength(fObs, 0);
  setlength(fPins, 0);
  fPinsDefined:=false;
  fName:=AName;
end;

procedure TCell.AddPoly(const APoly: TPoly);
begin
  setlength(fPolys, high(fPolys)+2);
  fPolys[high(fPolys)]:=APoly;
end;

procedure TCell.AddObstruction(const AObs: TPoly);
begin
  setlength(fObs, high(fObs)+2);
  fObs[high(fObs)]:=AObs;
end;

procedure TCell.AddPin(const AName: string; const APoly: TPoly; ADirection: TPinDirection; AClass: TPinClass; AShape: TPinShape);
var
  i,x: longint;
begin
  x:=-1;
  for i:= 0 to High(fPins) do
  begin
    if fPins[i].Name=AName then
    begin
      x:=i;
      break;
    end;
  end;

  if x=-1 then
  begin
    setlength(fPins, high(fPins)+2);
    x:=high(fPins);
    fPins[x].Name:=AName;
  end;

  setlength(fPins[x].Polygons, high(fPins[x].Polygons)+2);
  fPins[x].Polygons[high(fPins[x].Polygons)]:=APoly;
  fPins[x].Direction:=ADirection;
  fPins[x].PinClass:=AClass;
  fPins[x].Shape:=AShape;
end;

function TCell.FindPin(const AName: string): PPin;
var
  i: longint;
begin
  result:=nil;

  for i:=0 to high(fPins) do
    if fPins[i].Name=AName then
      exit(@fPins[i]);
end;

initialization
  CellDB:=TStringList.Create;
  CellDB.CaseSensitive:=true;
  CellDB.Sorted:=true;
  CellDB.Duplicates:=dupIgnore;
  CellDB.OwnsObjects:=true;

  Layers:=TStringList.Create;
  Layers.CaseSensitive:=true;
  Layers.Sorted:=true;
  Layers.Duplicates:=dupIgnore;
  Layers.OwnsObjects:=true;
finalization
  CellDB.Free;
  Layers.Free;

end.

