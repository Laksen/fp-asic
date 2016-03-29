unit cells;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TUnit = int64;

  TCoordinate = record
    X,Y: int64;
  end;

  TLayerType = (ltUnknown, ltCut, ltRouting, ltPoly);

  TLayer = class
  private
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

    property Name: string read fName write fName;
  end;

  TPoly = record
    Points: array of TCoordinate;
    Layer: TLayer;
  end;

  TCell = class
  private
    fName: string;
    fPinsDefined: boolean;
    fPolys: array of TPoly;
    function GetPolygonCount: longint;
    function GetPolygons(AIndex: longint): TPoly;
  public
    constructor Create(const AName: string);

    procedure AddPoly(const APoly: TPoly);

    property Name: string read fName;
    property PinsDefined: boolean read fPinsDefined;

    property PolygonCount: longint read GetPolygonCount;
    property Polygons[AIndex: longint]: TPoly read GetPolygons;
  end;

function GetCoord(const AX,AY: double): TCoordinate;

function GetLayer(const AName: string): TLayer;

function HasCell(const AName: string): boolean;
function FindCell(const AName: string): TCell;
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

function GetCoord(const AX, AY: double): TCoordinate;
begin
  result.X:=MeterToUnit(AX);
  result.Y:=MeterToUnit(AY);
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

function TCell.GetPolygons(AIndex: longint): TPoly;
begin
  result:=fPolys[AIndex];
end;

constructor TCell.Create(const AName: string);
begin
  inherited Create;
  setlength(fPolys, 0);
  fPinsDefined:=false;
  fName:=AName;
end;

procedure TCell.AddPoly(const APoly: TPoly);
begin
  setlength(fPolys, high(fPolys)+2);
  fPolys[high(fPolys)]:=APoly;
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

