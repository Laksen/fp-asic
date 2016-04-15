unit geometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type
  TUnit = int64;
  TArea = TUnit;

  TCoordinate = record
    X,Y: int64;
  end;

  TPolygon = record
    Rect: boolean;

    Start, Stop: TCoordinate;
    Points: array of TCoordinate;
  end;

function MeterToUnit(AValue: double): TUnit;
function UnitToMeters(AValue: TUnit): double;

function Coord(const AX,AY: TUnit): TCoordinate;
function GetCoord(const AX,AY: double): TCoordinate;
function GetRect(const AStart, AStop: TCoordinate): TPolygon;
function GetPolygon(const APoints: array of TCoordinate): TPolygon;

function Perpendicular(const A,B,C: TCoordinate): boolean;

procedure MinMax(const A,B: TCoordinate; out AMin, AMax: TCoordinate);
procedure MinMax(const APoly: TPolygon; out AMin, AMax: TCoordinate);

operator =(const A,B: TCoordinate): boolean;
operator +(const A,B: TCoordinate): TCoordinate;
operator -(const A,B: TCoordinate): TCoordinate;
operator -(const A: TCoordinate): TCoordinate;
operator *(const A,B: TCoordinate): TUnit;

function Inside(const A: TPolygon; const B: TCoordinate): boolean;
function Overlap(const A, B: TPolygon): boolean;

function Distance(const A,B: TCoordinate): TUnit;
function DistanceSqr(const A,B: TCoordinate): TUnit;

function NearestPoint(const A: TPolygon; const B: TCoordinate; ACornerSeparation: TUnit): TCoordinate;
function NearestPoint(const AStart, AStop: TCoordinate; const B: TCoordinate; ACornerSeparation: TUnit): TCoordinate;

implementation

const
  UnitsPerMeter = 10e10;
  MetersPerUnit = 1/UnitsPerMeter;

function MeterToUnit(AValue: double): TUnit;
begin
  result:=round(UnitsPerMeter*AValue);
end;

function UnitToMeters(AValue: TUnit): double;
begin
  result:=AValue/UnitsPerMeter;
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

function GetRect(const AStart, AStop: TCoordinate): TPolygon;
begin
  result.Rect:=true;

  MinMax(AStart, AStop, result.Start, result.Stop);

  setlength(result.Points,5);
  result.Points[0]:=result.Start;
  result.Points[1]:=Coord(result.Stop.X, result.Start.Y);
  result.Points[2]:=result.Stop;
  result.Points[3]:=Coord(result.Start.X, result.Stop.Y);
  result.Points[4]:=result.Start;
end;

function Perpendicular(const A,B,C: TCoordinate): boolean;
begin
  result:=((a-b)*(c-b))=0;
end;

function GetPolygon(const APoints: array of TCoordinate): TPolygon;
begin
  result.Rect:=false;

  setlength(result.Points, length(APoints));
  move(APoints[0], result.Points[0], length(APoints)*sizeof(TCoordinate));

  MinMax(result, result.Start, result.Stop);

  if (length(APoints)=5) and
     (APoints[0]=APoints[4]) and
     Perpendicular(APoints[0],APoints[1], APoints[2]) and
     Perpendicular(APoints[1],APoints[2], APoints[3]) and
     Perpendicular(APoints[2],APoints[3], APoints[4]) then
    result.Rect:=true;
end;

procedure MinMax(const A, B: TCoordinate; out AMin, AMax: TCoordinate);
begin
  AMin.x:=Min(a.x, b.x);
  AMin.y:=Min(a.y, b.y);

  AMax.x:=Max(a.x, b.x);
  AMax.y:=Max(a.y, b.y);
end;

procedure MinMax(const APoly: TPolygon; out AMin, AMax: TCoordinate);
var
  i: longint;
begin
  if APoly.Rect then
  begin
    AMin:=APoly.Start;
    AMax:=APoly.Stop;
    exit;
  end;

  AMin:=APoly.Points[0];
  AMax:=AMin;

  for i:=1 to high(APoly.Points) do
  begin
    MinMax(amin, APoly.Points[i], AMin, AMax);
    MinMax(amax, APoly.Points[i], AMin, AMax);
  end;
end;

operator=(const A, B: TCoordinate): boolean;
begin
  result:=(a.X=b.X) and (a.Y=b.y);
end;

operator+(const A, B: TCoordinate): TCoordinate;
begin
  result.x:=a.x+b.x;
  result.y:=a.y+b.y;
end;

operator-(const A, B: TCoordinate): TCoordinate;
begin
  result.x:=a.x-b.x;
  result.y:=a.y-b.y;
end;

operator-(const A: TCoordinate): TCoordinate;
begin
  result.x:=-a.x;
  result.y:=-a.y;
end;

operator*(const A, B: TCoordinate): TUnit;
begin
  result:=Round((a.x*MetersPerUnit*b.x)+(a.y*MetersPerUnit*b.y));
end;

function OverlapRects(const A, B: TPolygon): boolean;
begin
  result:=
    (a.Start.X<=b.Stop.x) and (a.Start.y<=b.Stop.y) and
    (a.stop.x>=b.start.x) and (a.Stop.y>=b.Start.y);
end;

function Inside(const A: TPolygon; const B: TCoordinate): boolean;
begin
  result:=
    (a.Start.X<=b.X) and (a.Stop.X>=b.X) and
    (a.Start.y<=b.y) and (a.stop.y>=b.y);

  if not a.Rect then
  begin
    raise ENotImplemented.Create('Not implemented');
  end;
end;

function Overlap(const A, B: TPolygon): boolean;
begin
  if A.Rect and b.Rect then
    result:=OverlapRects(A,B)
  else
  begin
    result:=OverlapRects(A,B);
    if result then
    begin
      raise ENotImplemented.Create('Not implemented');
    end;
  end;
end;

function Distance(const A, B: TCoordinate): TUnit;
begin
  result:=Round(Sqrt(A*B));
end;

function DistanceSqr(const A, B: TCoordinate): TUnit;
begin
  result:=A*B;
end;

function NearestPoint(const A: TPolygon; const B: TCoordinate; ACornerSeparation: TUnit): TCoordinate;
begin

end;

function NearestPoint(const AStart, AStop: TCoordinate; const B: TCoordinate; ACornerSeparation: TUnit): TCoordinate;
begin

end;

end.

