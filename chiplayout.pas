unit chiplayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cells, geometry;

type
  TNetClass = (ncNet, ncInput, ncOutput);

  TCellInst = class;

  TCellConnection = record
    Cell: TCellInst;
    Pin: string;
  end;

  TNet = class
  private
    fName: string;
    fNetClass: TNetClass;
    fDestinations: array of TCellConnection;
  public
    procedure Connect(ACell: TCellInst; const APin: string);

    constructor Create(const AName: string; ANetClass: TNetClass);

    property Name: string read fName;
    property NetClass: TNetClass read fNetClass;
  end;

  TPinConnection = record
    Pin: string;
    Net: TNet;
  end;

  TCellInst = class
  private
    fCell: TCell;
    fLoc: TCoordinate;
    fName: string;

    fConnections: array of TPinConnection;
  public
    procedure Connect(const APin: string; ANet: TNet);

    constructor Create(const AName: string; ACell: TCell);

    property Location: TCoordinate read fLoc write fLoc;
    property Cell: TCell read fCell;
  end;

  TGridLayout = class
  private
    fGrid: array of array of boolean;
    fWidth, fHeight: TUnit;

    function Fits(AW,AH, AX,AY: longint): boolean;
  public
    function GetLoc(AX, AY: longint): TCoordinate;
    function Allocate(AWidth, AHeight: TUnit): TCoordinate;

    constructor Create(AGridWidth, AGridHeight: longint; AWidth, AHeight: TUnit);
  end;

  TLayout = class
  private
    fInstances,
    fNets: TStringList;
    function getCount: longint;
    function GetInstance(AIndex: longint): TCellInst;
    function GetNet(AIndex: longint): TNet;
    function GetNetCount: longint;
  public
    function FindNet(const AName: string): TNet;

    function AddNet(const AName: string; ANetClass: TNetClass): TNet;
    function AddCell(const AName: string; ACell: TCell): TCellInst;

    constructor Create;
    destructor Destroy; override;

    property NetCount: longint read GetNetCount;
    property Net[AIndex: longint]: TNet read GetNet;

    property Count: longint read getCount;
    property Instance[AIndex: longint]: TCellInst read GetInstance;
  end;

var
  Layout: TLayout;

procedure LayoutRows;

implementation

uses
  math;

{
  x*w=y*h
  x=y*h/w
  x*y=totalarea/(w*h)
  y=sqrt((totalarea/(w*h))/(h/w))
}

procedure LayoutRows;
var
  Total, SlotSize: TArea;
  i, Count, x, y: longint;
  g: TGridLayout;
  c: TCellInst;
begin
  SlotSize:=CoreSize.X*CoreSize.Y;

  Total:=0;
  for i:=0 to Layout.Count-1 do
    Total:=Total+Layout.Instance[i].Cell.Area;

  if SlotSize<>0 then
  begin
    Count:=ceil(total/SlotSize*1.2);
    y:=ceil(sqrt((Total/SlotSize)/(CoreSize.y/CoreSize.x)));
    x:=ceil(y*CoreSize.Y/CoreSize.X);

    //writeln('Grid = ',UnitToMeters(x*CoreSize.x), ' x ',UnitToMeters(y*CoreSize.y));

    g:=TGridLayout.Create(x, y, CoreSize.x, CoreSize.y);

    for i:=0 to Layout.Count-1 do
    begin
      c:=Layout.Instance[i];
      c.Location:=g.Allocate(c.Cell.Size.X, c.Cell.Size.Y);
    end;

    g.Free;
  end;
end;

function TGridLayout.Fits(AW, AH, AX, AY: longint): boolean;
var
  i, i2: longint;
begin
  result:=false;
  for i:=0 to AH-1 do
    for i2:=0 to AW-1 do
      if fGrid[i+ay][i2+ax] then
        exit;
  result:=true;
end;

function TGridLayout.GetLoc(AX, AY: longint): TCoordinate;
begin
  result:=Coord(fWidth*AX, round(fHeight*AY*1.3));
end;

function TGridLayout.Allocate(AWidth, AHeight: TUnit): TCoordinate;
var
  w,h, i,i2,
  x,y: longint;
begin
  w:=ceil(AWidth/fWidth);
  h:=ceil(AHeight/fHeight);

  for i:=0 to length(fGrid)-h do
    for i2:=0 to length(fGrid[i])-w do
      if fits(w,h,i2,i) then
      begin
        for y:=0 to h-1 do
          for x:=0 to w-1 do
            fGrid[i+y][i2+x]:=true;
        exit(GetLoc(i2,i));
      end;
end;

constructor TGridLayout.Create(AGridWidth, AGridHeight: longint; AWidth, AHeight: TUnit);
var
  i: longint;
begin
  inherited Create;
  fWidth:=AWidth;
  fHeight:=AHeight;

  setlength(fGrid, AGridHeight);

  for i:=0 to high(fGrid) do
  begin
    setlength(fGrid[i], AGridWidth);

    fillchar(fGrid[i][0], length(fGrid[i]), false);
  end;
end;

procedure TNet.Connect(ACell: TCellInst; const APin: string);
begin
  SetLength(fDestinations, high(fDestinations)+2);
  fDestinations[high(fDestinations)].Pin:=APin;
  fDestinations[high(fDestinations)].Cell:=ACell;
end;

constructor TNet.Create(const AName: string; ANetClass: TNetClass);
begin
  inherited Create;
  fName:=AName;
  fNetClass:=ANetClass;
end;

procedure TCellInst.Connect(const APin: string; ANet: TNet);
begin
  SetLength(fConnections, high(fConnections)+2);
  fConnections[high(fConnections)].Pin:=APin;
  fConnections[high(fConnections)].Net:=ANet;
end;

constructor TCellInst.Create(const AName: string; ACell: TCell);
begin
  inherited Create;
  fName:=AName;
  fCell:=ACell;
end;

function TLayout.getCount: longint;
begin
  result:=fInstances.Count;
end;

function TLayout.GetInstance(AIndex: longint): TCellInst;
begin
  result:=TCellInst(fInstances.Objects[AIndex]);
end;

function TLayout.GetNet(AIndex: longint): TNet;
begin
  result:=TNet(fNets.Objects[AIndex]);
end;

function TLayout.GetNetCount: longint;
begin
  result:=fNets.Count;
end;

function TLayout.FindNet(const AName: string): TNet;
var
  idx: Integer;
begin
  result:=nil;
  if fNets.Find(AName, idx) then
    result:=TNet(fNets.Objects[idx])
  else
    raise exception.Create('Could not find net with name: "'+AName+'"');
end;

function TLayout.AddNet(const AName: string; ANetClass: TNetClass): TNet;
begin
  result:=TNet.Create(AName, ANetClass);
  fNets.AddObject(AName, result);
end;

function TLayout.AddCell(const AName: string; ACell: TCell): TCellInst;
begin
  result:=TCellInst.Create(AName, ACell);
  fInstances.AddObject(AName, result);
end;

constructor TLayout.Create;
begin
  inherited Create;
  fInstances:=TStringList.Create;
  fInstances.OwnsObjects:=true;
  fInstances.CaseSensitive:=true;
  fInstances.Duplicates:=dupIgnore;
  fInstances.Sorted:=true;

  fNets:=TStringList.Create;
  fNets.OwnsObjects:=true;
  fNets.CaseSensitive:=true;
  fNets.Duplicates:=dupIgnore;
  fNets.Sorted:=true;
end;

destructor TLayout.Destroy;
begin
  fNets.Free;
  fInstances.Free;
  inherited Destroy;
end;

initialization
  Layout:=TLayout.Create;
finalization
  Layout.Free;

end.

