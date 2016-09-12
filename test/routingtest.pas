unit routingtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  routing, geometry;

type
  TRouteTest= class(TTestCase)
  published
    procedure TestAStar;
  end;

implementation

procedure TRouteTest.TestAStar;
var
  mg: TMetalGrid;
  res: TRoute;
begin
  mg:=TMetalGrid.Create(MeterToUnit(1),Coord(0,0), Coord(10,10),2);

  res:=mg.AStar(0,1,Coord(0,0), Coord(0,0));
  AssertEquals('Route1', 2, length(res));

  res:=mg.AStar(0,1,Coord(0,0), Coord(1,1));
  AssertEquals('Route2', 4, length(res));

  mg.Occupy(Coord(1,0),0);
  res:=mg.AStar(0,0,Coord(0,0), Coord(2,0));
  AssertEquals('Route3', 5, length(res));

  mg.Free;
end;

initialization
  RegisterTest(TRouteTest);

end.

