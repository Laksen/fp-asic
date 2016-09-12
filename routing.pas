unit routing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  math,
  chiplayout, geometry, routingutils;

type
  TTrack = class;

  TTrackArray = array of TTrack;

  TTrack = class
  private
    fNet: TNet;
    fPoly: TPolygon;

    // Caching of connected tracks
    fTrackCounter: Int64;
    fTracks: TTrackArray;
  public
    constructor Create(ANet: TNet; const APoly: TPolygon);

    function GetConnected: TTrackArray; virtual;

    property Polygon: TPolygon read fPoly;
    property Net: TNet read fNet;
  end;

  TVia = class(TTrack);

  TRoutePoint = record
    Point: TCoordinate;
    Layer: longint;
  end;

  TRoute = array of TRoutePoint;

  TMetalGrid = class
  private
    fGrid: array of boolean;
    fLayers, fWidth, fHeight: longint;

    fSpacing: TUnit;

    function GetIndex(ALayer, AX, AY: longint): longint;
    function ValidPoint(const APt: TGridPoint): boolean;

    function Unmap(const APoint: TPoint): TCoordinate;
    function Map(const ACoordinate: TCoordinate): TPoint;
  public
    procedure Occupy(const APoint: TCoordinate; ALayer: longint);
    function AStar(AStartLayer, AEndLayer: longint; const AStart, AEnd: TCoordinate): TRoute;

    constructor Create(ASpacing: TUnit; const AMin, AMax: TCoordinate; ALayers: longint);
  end;

procedure AddTrack(ATrack: TTrack);
procedure RemoveTrack(ATrack: TTrack);

implementation

uses
  fgl;

type
  TGridPointSet = specialize TFPGList<TGridPoint>;
  TGridPointMap = specialize TFPGMap<TGridPoint, TGridPoint>;
  TGridPointScoreMap = specialize TFPGMap<TGridPoint, longint>;

var
  Tracks: TObjectList;
  TrackCounter: Int64 = 0;

procedure AddTrack(ATrack: TTrack);
begin
  Tracks.Add(ATrack);
  inc(TrackCounter);
end;

procedure RemoveTrack(ATrack: TTrack);
begin
  Tracks.Extract(ATrack);
  inc(TrackCounter);
end;

function TMetalGrid.GetIndex(ALayer, AX, AY: longint): longint;
begin
  result:=-1;
  if (ALayer<0) or (ALayer>=fLayers) or
     (AX<0) or (AX>=fWidth) or
     (AY<0) or (AY>=fHeight) then exit;

  result:=(ALayer*fHeight+ay)*fWidth+ax;
end;

function TMetalGrid.ValidPoint(const APt: TGridPoint): boolean;
begin
  result:=((Apt.Layer>=0) and (Apt.Layer<fLayers) and
           (Apt.Point.X>=0) and (Apt.Point.X<fWidth) and
           (apt.Point.Y>=0) and (Apt.Point.Y<fHeight));

  if result then
  begin
    if fGrid[GetIndex(APt.Layer,apt.Point.x,apt.Point.y)] then
      result:=false;
  end;
end;

function TMetalGrid.Unmap(const APoint: TPoint): TCoordinate;
begin
  result:=Coord(APoint.x*fSpacing, APoint.y*fSpacing);
end;

function TMetalGrid.Map(const ACoordinate: TCoordinate): TPoint;
var
  tmp: TCoordinate;
begin
  tmp:=ACoordinate/fSpacing;

  result.x:=round((tmp.x));
  result.y:=round((tmp.y));
end;

procedure TMetalGrid.Occupy(const APoint: TCoordinate; ALayer: longint);
begin
  fGrid[GetIndex(ALayer, APoint.x,APoint.y)]:=true;
end;

function GridPoint(const APoint: TPoint; ALayer: longint): TGridPoint;
begin
  result.Point:=APoint;
  Result.Layer:=ALayer;
end;

function CalcNeighbour(const APoint: TGridPoint; AIndex: longint): TGridPoint;
begin
  result:=APoint;

  case AIndex of
    0: dec(result.Layer);
    1: inc(result.Layer);

    2: dec(result.Point.x);
    3: inc(result.Point.x);

    4: dec(result.point.y);
    5: inc(result.point.y);
  end;
end;

function ManhattanDist(const A,B: TGridPoint): longint;
begin
  result:=abs(a.Layer-b.Layer)+abs(A.Point.x-B.Point.x)+abs(a.point.y-b.point.y);
end;

function TMetalGrid.AStar(AStartLayer, AEndLayer: longint; const AStart, AEnd: TCoordinate): TRoute;
var
  closedSet, openSet: TGridPointSet;
  fStart, fStop, current, n, neighbour: TGridPoint;
  cameFrom: TGridPointMap;
  fScore, gScore: TGridPointScoreMap;
  best, tmp, tentative_score: longint;
  i: Integer;
begin
  closedSet:=TGridPointSet.Create;

  fStart:=GridPoint(Map(AStart),AStartLayer);
  fStop:=GridPoint(Map(AEnd),AEndLayer);

  openSet:=TGridPointSet.Create;
  openset.Add(fStart);

  cameFrom:=TGridPointMap.Create;

  gScore:=TGridPointScoreMap.Create;
  gScore[fStart]:=0;

  fScore:=TGridPointScoreMap.Create;
  fScore[fStart]:=ManhattanDist(fStart, fStop);

  while openSet.Count>0 do
  begin
    best:=-1;
    for n in openset do
    begin
      if not fScore.TryGetData(n, tmp) then
        tmp:=-1;

      if (tmp>=0) and ((tmp<best) or (best=-1)) then
      begin
        current:=n;
        best:=tmp;
      end;
    end;

    if (fStop=current) then
    begin
      // TODO
      setlength(result, gScore[current]+1);

      i:=high(result);
      while i>=0 do
      begin
        result[i].Layer:=current.Layer;
        result[i].Point:=Unmap(current.Point);

        dec(i);

        if i>=0 then
        current:=cameFrom[current];
      end;

      closedSet.Free;
      openSet.Free;
      cameFrom.Free;
      gScore.Free;
      fScore.Free;

      exit;
    end;

    openset.Remove(current);
    closedSet.Add(current);

    for i:=0 to 5 do
    begin
      neighbour:=CalcNeighbour(current,i);
      if not ValidPoint(neighbour) then continue;
      if closedSet.IndexOf(neighbour)>=0 then
        continue;

      tentative_score:=gScore[current]+1;

      if openset.IndexOf(neighbour)<0 then
        openset.Add(neighbour)
      else if tentative_score>=gScore[neighbour] then
        continue;

      cameFrom[neighbour]:=current;
      gScore[neighbour]:=tentative_score;
      fScore[neighbour]:=tentative_score+ManhattanDist(neighbour,fStop);
    end;
  end;

  closedSet.Free;
  openSet.Free;
  cameFrom.Free;
  gScore.Free;
  fScore.Free;
  result:=nil;
end;

constructor TMetalGrid.Create(ASpacing: TUnit; const AMin, AMax: TCoordinate; ALayers: longint);
var
  Size: TCoordinate;
  x, y: longint;
begin
  inherited Create;

  fSpacing:=ASpacing;

  Size:=(AMax-AMin)/ASpacing;

  x:=ceil((size.x));
  y:=ceil((size.y));

  fWidth:=x;
  fHeight:=y;
  fLayers:=ALayers;

  SetLength(FGrid, fLayers*fWidth*fHeight);
end;

constructor TTrack.Create(ANet: TNet; const APoly: TPolygon);
begin
  inherited Create;
  fNet:=ANet;
  fPoly:=APoly;

  fTrackCounter:=TrackCounter-1;
  fTracks:=nil;
end;

function TTrack.GetConnected: TTrackArray;
begin
  result:=fTracks;
end;

initialization
  Tracks:=TObjectList.Create(true);
finalization
  Tracks.Free;

end.

