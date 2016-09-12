unit routingutils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGridPoint = record
    Point: TPoint;
    Layer: longint;
    class operator Equal(const A, B: TGridPoint): boolean;
    class operator NotEqual(const A, B: TGridPoint): boolean;
    class operator LessThan(const A, B: TGridPoint): boolean;
    class operator GreaterThan(const A, B: TGridPoint): boolean;
    //class operator LessThanEqual(const A, B: TGridPoint): boolean;
  end;


implementation

class operator TGridPoint.Equal(const A, B: TGridPoint): boolean;
begin
  result:=(A.Layer=B.Layer) and (a.Point.x=b.point.x) and (a.point.y=b.point.y);
end;

class operator TGridPoint.NotEqual(const A, B: TGridPoint): boolean;
begin
  result:=not (A=B)
end;

class operator TGridPoint.LessThan(const A, B: TGridPoint): boolean;
begin
  if (a.layer<B.Layer) then exit(true);
  if (a.Point.y<b.Point.y) then exit(true);
  if (a.point.x<b.point.x) then exit(true);
  exit(false);
end;

class operator TGridPoint.GreaterThan(const A, B: TGridPoint): boolean;
begin
  if (a.layer>B.Layer) then exit(true);
  if (a.Point.y>b.Point.y) then exit(true);
  if (a.point.x>b.point.x) then exit(true);
  exit(false);
end;

{class operator TGridPoint.LessEqual(const A, B: TGridPoint): boolean;
begin
  result:=(a<b) or (a=b);
end;}

end.

