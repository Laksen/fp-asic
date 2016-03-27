program gdstest;

{$mode objfpc}
{$h+}

uses
  cmem,
  sysutils,classes,
  gdsreader, math,
  sdl,
  sdl_gfx;

const
  w = 800;
  h = 600;

var
  s: TFileStream;
  gd: TGDS2Reader;
  min, max: TXY;
  awindow: PSDL_Surface;

procedure WaitWindow;
var
  event : TSDL_Event;
begin
  while SDL_WaitEvent(@event)<>0 do
    begin
      case event.type_ of
        SDL_KEYDOWN:;
        SDL_QUITEV:break;
      end;
    end;
end;

const
  PlotColors: array[0..16] of longword = (
    $FF0000FF,
    $FFFF00FF,
    $FF00FFFF,
    $00FF00FF,
    $00FFFFFF,
    $0000FFFF,
    $7F0000FF,
    $7F7F00FF,
    $7F007FFF,
    $007F00FF,
    $007F7FFF,
    $FF007FFF,
    $7F00FFFF,
    $00FF7FFF,
    $007FFFFF,
    $FF700FFF,
    $7FF00FFF
  );
var
  Layers: array[0..16] of longint;

function GetColor(ALayer: LongInt): longword;
var
  i: Integer;
begin
  result:=PlotColors[ALayer mod 17];
  for i:=0 to high(Layers) do
    if Layers[i]=ALayer then
      begin
        result:=(PlotColors[i]);
        break;
      end
    else if layers[i]=0 then
      begin
        layers[i]:=ALayer;
        result:=(PlotColors[i]);
        break;
      end;

  result:=result and $FFFFFF7F;
end;

procedure Plot;
var
  i, i2: Integer;
  poly: TPolygon;
  ptsx, ptsy: array of SInt16;
  c, scale: TXY;
  st: TStructure;
begin
  SDL_Init(SDL_INIT_VIDEO);
  SDL_VideoInit(nil,0);
  awindow:=SDL_SetVideoMode(w,h,32,SDL_HWSURFACE or SDL_HWPALETTE or SDL_DOUBLEBUF);

  st:=gd.Structure[9];
  writeln(st.Name);

  min:=st.MinPoint;
  max:=st.MaxPoint;

  scale.x:=w/(max.x-min.x);
  scale.y:=h/(max.y-min.y);
  scale.x:=math.min(scale.x,scale.y);
  scale.y:=scale.x;

  writeln((max.x-min.x)*(max.y-min.y));

  for i:= 0 to st.Count-1 do
  begin
    poly:=st.Polygon[i];

    setlength(ptsx, poly.Count);
    setlength(ptsy, poly.Count);

    for i2:=0 to poly.count-1 do
      begin
        c:=poly.Point[i2];

        ptsx[i2]:=round((c.X-min.X)*scale.x);
        ptsy[i2]:=round((c.y-min.y)*scale.y);
      end;

    filledpolygonColor(awindow, @ptsx[0], @ptsy[0], poly.count, GetColor(poly.Layer));
  end;

  SDL_Flip(AWindow);
  WaitWindow;

  SDL_Quit;
end;

var i: longint;
begin
  s:=TFileStream.Create('osu035_stdcells.gds2', fmopenread);
  gd:=TGDS2Reader.Create();
  gd.LoadFromStream(s);

  for i:=0 to gd.count-1 do
    writeln(i,': ',gd.Structure[i].Name);

  plot;

  gd.free;
  s.free;
end.

