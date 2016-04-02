unit cells;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type
  TUnit = int64;

  TCoordinate = record
    X,Y: int64;
  end;

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
    Points: array of TCoordinate;
    Layer: TLayer;
  end;

  TPin = record
    Name: string;
    Polygons: array of TPoly;
  end;

  TCell = class
  private
    fName: string;
    fPinsDefined: boolean;
    fPolys, fObs: array of TPoly;
    fPins: array of TPin;
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
    procedure AddPin(const AName: string; const APoly: TPoly);

    property Name: string read fName;
    property PinsDefined: boolean read fPinsDefined;

    property PolygonCount: longint read GetPolygonCount;
    property Polygons[AIndex: longint]: TPoly read GetPolygons;

    property ObstructionCount: longint read GetObstructionCount;
    property Obstructions[AIndex: longint]: TPoly read GetObstructions;

    property PinsCount: longint read GetPinsCount;
    property Pins[AIndex: longint]: TPin read GetPins;
  end;

  TVia = class(TCell)
  end;

function Coord(const AX,AY: TUnit): TCoordinate;
function GetCoord(const AX,AY: double): TCoordinate;
function Sub(const A,B: TCoordinate): TCoordinate;
function GetRect(ALayer: TLayer; const AStart, AStop: TCoordinate): TPoly;

procedure MinMax(const A,B: TCoordinate; out AMin, AMax: TCoordinate);
procedure MinMax(const APoly: TPoly; out AMin, AMax: TCoordinate);

function GetLayerCount: longint;
function GetLayer(const AIndex: longint): TLayer;
function GetLayer(const AName: string): TLayer;

function HasCell(const AName: string): boolean;
function FindCell(const AName: string): TCell;

function GetCellCount: longint;
function GetCell(AIndex: longint): TCell;

procedure RegisterCell(ACell: TCell);

implementation

const
  UnitsPerMeter = 10e10;

var
  Layers,
  CellDB: TStringList;

function MeterToUnit(AValue: double): TUnit;
begin
  result:=round(UnitsPerMeter*AValue);
end;

function Coord(const AX, AY: TUnit): TCoordinate;
begin
  result.X:=AX;
  result.Y:=AY;
end;

function GetCoord(const AX, AY: double): TCoordinate;
begin
  result.X:=MeterToUnit(AX);
  result.Y:=MeterToUnit(AY);
end;

function Sub(const A, B: TCoordinate): TCoordinate;
begin
  result.x:=a.x-b.x;
  result.y:=a.y-b.y;
end;

function GetRect(ALayer: TLayer; const AStart, AStop: TCoordinate): TPoly;
begin
  result.Layer:=ALayer;
  setlength(result.Points,5);
  result.Points[0]:=AStart;
  result.Points[1]:=Coord(AStop.X, AStart.Y);
  result.Points[2]:=AStop;
  result.Points[3]:=Coord(AStart.X, AStop.Y);
  result.Points[4]:=AStart;
end;

procedure MinMax(const A, B: TCoordinate; out AMin, AMax: TCoordinate);
begin
  AMin.x:=Min(a.x, b.x);
  AMin.y:=Min(a.y, b.y);

  AMax.x:=Max(a.x, b.x);
  AMax.y:=Max(a.y, b.y);
end;

procedure MinMax(const APoly: TPoly; out AMin, AMax: TCoordinate);
var
  i: longint;
begin
  AMin:=APoly.Points[0];
  AMax:=AMin;

  for i:=1 to high(APoly.Points) do
  begin
    MinMax(amin, APoly.Points[i], AMin, AMax);
    MinMax(amax, APoly.Points[i], AMin, AMax);
  end;
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
  fPolys[high(fPolys)].Layer:=APoly.Layer;
  fPolys[high(fPolys)].Points:=Copy(APoly.Points);
end;

procedure TCell.AddObstruction(const AObs: TPoly);
begin
  setlength(fObs, high(fObs)+2);
  fObs[high(fObs)].Layer:=AObs.Layer;
  fObs[high(fObs)].Points:=Copy(AObs.Points);
end;

procedure TCell.AddPin(const AName: string; const APoly: TPoly);
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
  fPins[x].Polygons[high(fPins[x].Polygons)].Points:=Copy(APoly.Points);
  fPins[x].Polygons[high(fPins[x].Polygons)].Layer:=APoly.Layer;
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

