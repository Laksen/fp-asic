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
operator*(const A: TCoordinate; B: double): TCoordinate;
operator *(const A,B: TCoordinate): TUnit;
operator div(const A: TCoordinate; B: longint): TCoordinate;
operator /(const A: TCoordinate; B: TUnit): TCoordinate;

function Normalize(const A: TCoordinate): TCoordinate;

function Inside(const A: TPolygon; const B: TCoordinate): boolean;
function Overlap(const A, B: TPolygon): boolean;
function GetOverlap(const A, B: TPolygon): TPolygon;

type
  TOrientation = (oAX, oBX);

function IsMosOverlap(const A,B: TPolygon; out AOrientation: TOrientation): boolean;

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
var
  s,t: TCoordinate;
begin
  s.x:=Min(a.x, b.x);
  s.y:=Min(a.y, b.y);

  t.x:=Max(a.x, b.x);
  t.y:=Max(a.y, b.y);

  AMin:=s;
  AMax:=t;
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
    AMin.x:=Min(AMin.X, APoly.Points[i].X);
    AMin.y:=Min(AMin.y, APoly.Points[i].y);

    AMax.x:=Max(AMax.X, APoly.Points[i].X);
    AMax.y:=Max(AMax.y, APoly.Points[i].y);
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

operator*(const A: TCoordinate; B: double): TCoordinate;
begin
  result.X:=round(A.X*B);
  result.Y:=round(A.Y*B);
end;

operator*(const A, B: TCoordinate): TUnit;
begin
  result:=Round((a.x*MetersPerUnit*b.x)+(a.y*MetersPerUnit*b.y));
end;

operator div(const A: TCoordinate; B: longint): TCoordinate;
begin
  result.X:=A.X div B;
  result.Y:=A.Y div B;
end;

operator/(const A: TCoordinate; B: TUnit): TCoordinate;
var
  x: double;
begin
  if B=0 then
    result:=A
  else
  begin
    x:=1/(B*MetersPerUnit);
    result:=Coord(Round(A.X*x), Round(A.Y*x));
  end;
end;

function OverlapRects(const A, B: TPolygon): boolean;
begin
  result:=
    (a.Start.X<=b.Stop.x) and (a.Start.y<=b.Stop.y) and
    (a.stop.x>=b.start.x) and (a.Stop.y>=b.Start.y);
end;

function Normalize(const A: TCoordinate): TCoordinate;
var
  l: TUnit;
begin
  l:=Distance(A,Coord(0,0));

  if l=0 then
    exit(a)
  else
    exit(a/l);
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

function GetOverlap(const A, B: TPolygon): TPolygon;
var
  x, y: TCoordinate;
begin
  if Overlap(a,b) then
  begin
    x:=Coord(max(a.Start.X, b.Start.x), max(a.Start.y, b.Start.y));
    y:=Coord(min(a.Stop.X, b.Stop.x), min(a.Stop.y, b.Stop.y));

    result:=GetRect(x,y);
  end
  else
    result:=GetRect(Coord(0,0), Coord(0,0));
end;

function IsMosOverlap(const A, B: TPolygon; out AOrientation: TOrientation): boolean;
begin
  result:=Overlap(A,B);
  AOrientation:=oAX;

  if result then
  begin
    if (A.Start.Y>B.Start.Y) and (A.Stop.Y<B.Start.Y) and
       (A.Start.X<B.Start.X) and (A.Stop.X>B.Stop.X) then
      AOrientation:=oAX
    else if (A.Start.Y<B.Start.Y) and (A.Stop.Y>B.Start.Y) and
       (A.Start.X>B.Start.X) and (A.Stop.X<B.Stop.X) then
      AOrientation:=oBX
    else
      result:=false;
  end;
end;

function Distance(const A, B: TCoordinate): TUnit;
var
  s: TCoordinate;
begin
  s:=(b-a);
  result:=MeterToUnit(Sqrt(UnitToMeters(S*S)));
end;

function DistanceSqr(const A, B: TCoordinate): TUnit;
var
  s: TCoordinate;
begin
  s:=(b-a);
  result:=s*s;
end;

function NearestPoint(const A: TPolygon; const B: TCoordinate; ACornerSeparation: TUnit): TCoordinate;
var
  res: TCoordinate;
  nd, bd: TUnit;
  i: longint;
begin
  if Inside(a, b) then
    exit(b)
  else
  begin
    result:=NearestPoint(a.Points[0], a.Points[1], b, ACornerSeparation);
    bd:=DistanceSqr(result,b);

    for i:=1 to high(a.Points)-1 do
    begin
      res:=NearestPoint(a.Points[i], a.Points[i+1], b, ACornerSeparation);
      nd:=DistanceSqr(res,b);

      if nd<bd then
      begin
        result:=res;
        bd:=nd;
      end;
    end;
  end;
end;

function NearestPoint(const AStart, AStop: TCoordinate; const B: TCoordinate; ACornerSeparation: TUnit): TCoordinate;
var
  s: TCoordinate;
  d,l, cs: double;
begin
  if Distance(AStart,AStop)<=(ACornerSeparation*2) then
    result:=((AStop-AStart) div 2)+AStart
  else
  begin
    s:=Normalize(AStop-AStart);
    d:=UnitToMeters(distance(AStop,AStart));
    cs:=UnitToMeters(ACornerSeparation);

    l:=((b-AStart)*s)/(s*s);

    if l<cs then
      l:=cs
    else if l>d-cs then
      l:=d-cs;

    result:=AStart+s*l;
  end;
end;

end.

