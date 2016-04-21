unit routing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  chiplayout, geometry;

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

procedure AddTrack(ATrack: TTrack);
procedure RemoveTrack(ATrack: TTrack);

implementation

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

