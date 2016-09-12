unit geometrytest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TGeoTest= class(TTestCase)
  published
    procedure TestUnits;
    procedure TestCoordinates;
    procedure TestSimple;
    procedure TestDistance;
  end;

implementation

uses
  geometry;

procedure TGeoTest.TestUnits;
begin
  AssertEquals(+0,UnitToMeters(MeterToUnit(+0)));
  AssertEquals(+1,UnitToMeters(MeterToUnit(+1)));
  AssertEquals(-1,UnitToMeters(MeterToUnit(-1)));

  AssertEquals(+0,MeterToUnit(UnitToMeters(+0)));
  AssertEquals(+1,MeterToUnit(UnitToMeters(+1)));
  AssertEquals(-1,MeterToUnit(UnitToMeters(-1)));
end;

procedure TGeoTest.TestCoordinates;
var
  c, b, a: TCoordinate;
begin
  c:=GetCoord(1,2);
  AssertEquals(1, UnitToMeters(c.X));
  AssertEquals(2, UnitToMeters(c.Y));

  c:=Coord(1,2);
  AssertEquals(1, c.X);
  AssertEquals(2, c.Y);

  a:=GetCoord(100,1);
  b:=GetCoord(100,100);
  c:=GetCoord(1,100);

  AssertEquals(100*100+1*100, UnitToMeters(a*b));
  AssertEquals(100*1+1*100, UnitToMeters(a*c));

  AssertEquals(0, UnitToMeters((a-b).x));
  AssertEquals(-99, UnitToMeters((a-b).y));

  AssertEquals(200, UnitToMeters((a+b).x));
  AssertEquals(101, UnitToMeters((a+b).y));

  AssertEquals(-100, UnitToMeters((-a).x));
  AssertEquals(-1, UnitToMeters((-a).y));

  AssertEquals(true, Perpendicular(a,b,c));
end;

procedure TGeoTest.TestSimple;
var
  c, b: TPolygon;
begin
  c:=GetRect(GetCoord(0,0), GetCoord(100,100));
  AssertEquals(true, c.Rect);
  AssertEquals(5, length(c.Points));
  AssertEquals(true, c.Points[0]=c.Start);
  AssertEquals(true, c.Points[2]=c.Stop);

  b:=GetRect(GetCoord(-20,40), GetCoord(120,60));
  AssertEquals(true, Overlap(b,c));
  AssertEquals(true, Overlap(c,b));

  b:=GetRect(GetCoord(20,40), GetCoord(80,60));
  AssertEquals(true, Overlap(b,c));
  AssertEquals(true, Overlap(c,b));

  b:=GetRect(GetCoord(-20,40), GetCoord(80,60));
  AssertEquals(true, Overlap(b,c));
  AssertEquals(true, Overlap(c,b));

  b:=GetRect(GetCoord(-20,-20), GetCoord(20,20));
  AssertEquals(true, Overlap(b,c));
  AssertEquals(true, Overlap(c,b));

  b:=GetRect(GetCoord(-20,-20), GetCoord(0,0));
  AssertEquals(true, Overlap(b,c));
  AssertEquals(true, Overlap(c,b));

  b:=GetRect(GetCoord(-20,-20), GetCoord(-1,-1));
  AssertEquals(false, Overlap(b,c));
  AssertEquals(false, Overlap(c,b));

  b:=GetRect(GetCoord(0,-2), GetCoord(100,-1));
  AssertEquals(false, Overlap(b,c));
  AssertEquals(false, Overlap(c,b));

  b:=GetRect(GetCoord(0,0), GetCoord(100,100));
  AssertEquals(True, Overlap(b,c));
  AssertEquals(True, Overlap(c,b));

  b:=GetRect(GetCoord(-1,-1), GetCoord(101,101));
  AssertEquals(True, Overlap(b,c));
  AssertEquals(True, Overlap(c,b));
end;

procedure TGeoTest.TestDistance;
var
  a, b, c: TCoordinate;
  r: TPolygon;
begin
  a:=GetCoord(10,10);
  b:=GetCoord(20,10);

  AssertEquals(10, UnitToMeters(Distance(a,b)));

  r:=GetRect(GetCoord(10,10), GetCoord(20,20));

  c:=NearestPoint(r,a,0);
  AssertEquals(10, UnitToMeters(c.X));
  AssertEquals(10, UnitToMeters(c.Y));

  c:=NearestPoint(r,GetCoord(15,0),0);
  AssertEquals(15, UnitToMeters(c.X));
  AssertEquals(10, UnitToMeters(c.Y));

  c:=NearestPoint(r,GetCoord(1,0),MeterToUnit(1));
  AssertEquals(11, UnitToMeters(c.X));
  AssertEquals(10, UnitToMeters(c.Y));
end;

initialization
  RegisterTest(TGeoTest);

end.

