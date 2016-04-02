unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, ExtCtrls, ColorBox, Grids,
  gl,
  gdsreader, lefreader, blifreader,
  cells, StdCtrls, Types;

type
  TDrawMode = (dmNone, dmCell, dmNetList);

  TLayerInfo = record
    Layer: TLayer;
    Visible: boolean;
    Color: TColor;
  end;

  TForm1 = class(TForm)
    LayerGrid: TDrawGrid;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    RenderLayersMenu: TMenuItem;
    RenderObsMenu: TMenuItem;
    RenderPinsMenu: TMenuItem;
    NewProjectMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveProjectMenu: TMenuItem;
    SaveProjectAsMenu: TMenuItem;
    LoadProjectMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    MenuItem7: TMenuItem;
    AddFileItem: TMenuItem;
    oglView: TOpenGLControl;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tvTech: TTreeView;
    procedure AddFileItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure LayerGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure LayerGridCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure LayerGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure LayerGridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure LayerGridSetCheckboxState(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
    procedure oglViewMakeCurrent(Sender: TObject; var Allow: boolean);
    procedure oglViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure oglViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure oglViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure oglViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure oglViewPaint(Sender: TObject);
    procedure RenderMenuClick(Sender: TObject);
    procedure tvTechSelectionChanged(Sender: TObject);
  private
    function PosToWorld(const APos: TPoint): TCoordinate;
  private
    DrawMode: TDrawMode;

    IsDragging: boolean;
    MouseStart,
    Mouse: TCoordinate;

    Offset: TCoordinate;
    Scale: int64;

    CurrentCell: TCell;

    procedure DrawCell;
    procedure ResetZoom;

    procedure DrawPoly(const APoly: TPoly);

    procedure DoLoadGDS;
    procedure DoLoadLEF;
    procedure DoLoadBLIF;

    procedure UpdateCells;
    procedure ReorderLayers;
  private
    Layers: array of TLayerInfo;

    procedure EnsureLayerInfo(ALayer: TLayer);
    function GetLayerInfoIdx(ALayer: TLayer): longint;
    function GetLayerInfo(ALayer: TLayer): TLayerInfo;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  math;

{$R *.lfm}

function LayerIndexCompare(Item1, Item2: Pointer): Integer;
begin
  result:=TLayer(Item1).Index-TLayer(Item2).Index;
end;

procedure TForm1.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.LayerGridButtonClick(Sender: TObject; aCol, aRow: Integer);
begin
  //LayerGrid.Columns[1].
end;

procedure TForm1.LayerGridCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
begin
end;

procedure TForm1.LayerGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if aCol=2 then
  begin
    LayerGrid.Canvas.Brush.Color:=Layers[aRow].Color;
    LayerGrid.Canvas.FillRect(Rect(aRect.Left+1,aRect.Top+1,aRect.Left+9,aRect.Bottom-1));

    LayerGrid.Canvas.TextRect(aRect, aRect.Left+10,aRect.top, Layers[aRow].Layer.Name);
  end;
end;

procedure TForm1.LayerGridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
begin
  if Layers[ARow].Visible then
    value:=cbChecked
  else
    value:=cbUnchecked;
end;

procedure TForm1.LayerGridSetCheckboxState(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
begin
  Layers[aRow].Visible:=not Layers[aRow].Visible;
  oglView.Repaint;
end;

procedure TForm1.oglViewMakeCurrent(Sender: TObject; var Allow: boolean);
begin
  Allow:=true;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0,oglView.ClientWidth,oglView.ClientHeight,0,0.1,1000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glEnable(GL_DEPTH_TEST);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure TForm1.oglViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseStart:=PosToWorld(Point(x,y));
  IsDragging:=true;
end;

procedure TForm1.oglViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if IsDragging then
  begin
    Mouse:=Sub(PosToWorld(Point(x,y)), MouseStart);
    oglView.Repaint;
  end;
end;

procedure TForm1.oglViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if IsDragging then
  begin
    Mouse:=Sub(PosToWorld(Point(x,y)), MouseStart);

    Offset.x:=Offset.x+Mouse.x;
    Offset.y:=Offset.y+Mouse.y;

    Mouse:=Coord(0,0);
    oglView.Repaint;

    IsDragging:=false;
  end;
end;

procedure TForm1.oglViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  cp, np: TCoordinate;
begin
  Handled:=true;

  cp:=PosToWorld(MousePos);

  if WheelDelta>0 then
    scale:=round(scale/1.2)
  else
    scale:=round(scale*1.2);

  np:=PosToWorld(MousePos);

  offset:=sub(offset, sub(cp,np));

  oglView.Repaint;
end;

procedure TForm1.AddFileItemClick(Sender: TObject);
begin
  if tvTech.Items.TopLvlItems[0].Items[0].Selected then
    DoLoadGDS
  else if tvTech.Items.TopLvlItems[0].Items[1].Selected then
    DoLoadLEF
  else if tvTech.Items.TopLvlItems[0].Items[2].Selected then
    DoLoadBlif;
end;

procedure TForm1.oglViewPaint(Sender: TObject);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  case DrawMode of
    dmNone:
      begin
        glClearColor(0,0,0,0);
        glClear(GL_COLOR_BUFFER_BIT);
      end;
    dmCell:
      DrawCell;
  end;

  oglView.SwapBuffers;
end;

procedure TForm1.RenderMenuClick(Sender: TObject);
begin
  TMenuItem(sender).Checked:=not TMenuItem(sender).Checked;
  oglView.Repaint;
end;

procedure TForm1.tvTechSelectionChanged(Sender: TObject);
begin
  if tvTech.Selected.HasAsParent(tvTech.Items.TopLvlItems[1]) then
  begin
    DrawMode:=dmCell;
    CurrentCell:=FindCell(tvTech.Selected.Text);

    ResetZoom;
  end
  else
    DrawMode:=dmNone;

  oglView.Repaint;
end;

function TForm1.PosToWorld(const APos: TPoint): TCoordinate;
var
  rScale: double;
begin
  rScale:=scale;

  result:=Coord(round(APos.x*rScale+Offset.X), round(APos.Y*rScale+Offset.Y));
end;

procedure TForm1.DrawCell;
var
  i, i2, layer: longint;
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glTranslated(0,0,-20);
  glScaled(1/Scale,1/scale,1);
  glTranslated(Offset.x,offset.y,0);

  glTranslated(mouse.x,mouse.y,0);

  glenable(GL_BLEND);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  for layer:=0 to high(Layers) do
  begin
    if RenderLayersMenu.Checked then
      for i:=0 to CurrentCell.PolygonCount-1 do
        if CurrentCell.Polygons[i].Layer=layers[layer].Layer then
          DrawPoly(CurrentCell.Polygons[i]);

    if RenderObsMenu.Checked then
      for i:=0 to CurrentCell.ObstructionCount-1 do
        if CurrentCell.Obstructions[i].Layer=layers[layer].Layer then
          DrawPoly(CurrentCell.Obstructions[i]);

    if RenderPinsMenu.Checked then
      for i:=0 to CurrentCell.PinsCount-1 do
        for i2:=0 to high(CurrentCell.Pins[i].Polygons) do
           if CurrentCell.Pins[i].Polygons[i2].Layer=layers[layer].Layer then
             DrawPoly(CurrentCell.Pins[i].Polygons[i2]);
  end;

  gldisable(GL_BLEND);
end;

procedure TForm1.ResetZoom;
var
  HasFirst: Boolean;
  i, i2, Start: longint;
  mi, ma, ti, ta: TCoordinate;
  dx, dy: Int64;
begin
  if (DrawMode=dmCell) and
     assigned(CurrentCell) and
     ((CurrentCell.PolygonCount>0) or
      (CurrentCell.PinsCount>0) or
      (CurrentCell.ObstructionCount>0)) then
  begin
    HasFirst:=false;

    if CurrentCell.PolygonCount>0 then
    begin
      HasFirst:=true;
      MinMax(CurrentCell.Polygons[0], mi, ma);

      for i:=1 to CurrentCell.PolygonCount-1 do
      begin
        MinMax(CurrentCell.Polygons[i], ti, ta);

        MinMax(mi,ti,mi,ma);
        MinMax(mi,ta,mi,ma);
      end;
    end;

    for i2:=0 to CurrentCell.PinsCount-1 do
    begin
      if length(CurrentCell.Pins[i2].Polygons)<=0 then continue;

      if not HasFirst then
      begin
        HasFirst:=true;
        MinMax(CurrentCell.Pins[i2].Polygons[0], mi, ma);
        Start:=1;
      end
      else
        Start:=0;

      for i:=start to high(CurrentCell.Pins[i2].Polygons) do
      begin
        MinMax(CurrentCell.Pins[i2].Polygons[i], ti, ta);

        MinMax(mi,ti,mi,ma);
        MinMax(mi,ta,mi,ma);
      end;
    end;

    if CurrentCell.ObstructionCount>0 then
    begin
      if not HasFirst then
      begin
        HasFirst:=true;
        MinMax(CurrentCell.Obstructions[0], mi, ma);
        Start:=1;
      end
      else
        Start:=0;

      for i:=start to CurrentCell.ObstructionCount-1 do
      begin
        MinMax(CurrentCell.Obstructions[i], ti, ta);

        MinMax(mi,ti,mi,ma);
        MinMax(mi,ta,mi,ma);
      end;
    end;
  end;

  dx:=ma.x-mi.x;
  dy:=ma.y-mi.y;

  Scale:=max(dx div oglView.ClientWidth,dy div oglView.ClientHeight);
  Offset:=Coord(0,0);
end;

procedure TForm1.DrawPoly(const APoly: TPoly);
var
  i: longint;
  li: TLayerInfo;
begin
  li:=GetLayerInfo(APoly.Layer);

  if not li.Visible then exit;

  glColor4ub(Red(li.Color), green(li.Color), blue(li.Color), 128);

  glBegin(GL_POLYGON);

  for i:=0 to high(APoly.Points) do
    glVertex3d(APoly.Points[i].X, APoly.Points[i].y, GetLayerInfoIdx(APoly.Layer));

  glEnd();
end;

procedure TForm1.DoLoadGDS;
var
  Fns: TStringList;
  MapFileName: String;
  i: longint;
begin
  Fns:=TStringList.Create;
  try
    OpenDialog1.Filter:='GDS2 Files|*.gds2';
    OpenDialog1.Title:='Select GDS2 file(s)';
    OpenDialog1.Options:=OpenDialog1.Options+[ofAllowMultiSelect];

    if OpenDialog1.Execute then
      Fns.Assign(OpenDialog1.Files)
    else
      exit;

    OpenDialog1.Options:=OpenDialog1.Options-[ofAllowMultiSelect];

    OpenDialog1.Filter:='GDS2 Stream Map|*.map';
    OpenDialog1.Title:='Select GDS2 Stream Mapping file';

    MapFileName:='';
    if OpenDialog1.Execute then
      MapFileName:=OpenDialog1.FileName;

    for i:=0 to Fns.Count-1 do
    begin
      LoadGDS(Fns[i], MapFileName);

      tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[0], ExtractRelativepath(GetCurrentDir, Fns[i]));
      tvTech.Items.TopLvlItems[0].items[0].Expand(true);
    end;
  finally
    Fns.Free;
  end;

  UpdateCells;
end;

procedure TForm1.DoLoadLEF;
var
  FileName: String;
begin
  OpenDialog1.Filter:='LEF Files|*.lef';
  OpenDialog1.Title:='Select LEF file';

  if OpenDialog1.Execute then
    FileName:=OpenDialog1.FileName
  else
    exit;

  LoadLEF(FileName);

  tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[1], ExtractRelativepath(GetCurrentDir, Filename));
  tvTech.Items.TopLvlItems[0].items[1].Expand(true);

  UpdateCells;

  ReorderLayers;
end;

procedure TForm1.DoLoadBLIF;
var
  FileName: String;
begin
  OpenDialog1.Filter:='BLIF Files|*.blif';
  OpenDialog1.Title:='Select BLIF file';

  if OpenDialog1.Execute then
    FileName:=OpenDialog1.FileName
  else
    exit;

  //LoadBlif(FileName);

  tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[2], ExtractRelativepath(GetCurrentDir, Filename));
  tvTech.Items.TopLvlItems[0].items[2].Expand(true);

  UpdateCells;
end;

procedure TForm1.UpdateCells;
var
  i: longint;
  p: TTreeNode;
  ex: Boolean;
begin
  tvTech.BeginUpdate;
  try
    p:=tvTech.Items.TopLvlItems[1];
    ex:=p.Expanded;

    for i:=p.Count-1 downto 0 do
      tvTech.Items.Delete(p.Items[i]);

    for i:=0 to GetCellCount-1 do
      tvTech.Items.AddChild(p, GetCell(i).Name);

    if ex then
      p.Expand(true);
  finally
    tvTech.EndUpdate;
  end;

  for i := 0 to GetLayerCount-1 do
    EnsureLayerInfo(GetLayer(i));

  LayerGrid.RowCount:=length(Layers);
end;

procedure TForm1.ReorderLayers;
var
  tl: TList;
  i: longint;
  Layers2: array of TLayerInfo;
begin
  tl:=TList.Create;

  for i:=0 to High(Layers) do
    tl.Add(layers[i].Layer);

  tl.Sort(@LayerIndexCompare);

  Layers2:=Copy(Layers);

  for i:=0 to High(Layers) do
    Layers2[i]:=GetLayerInfo(TLayer(tl[i]));

  Layers:=Copy(Layers2);

  tl.Free;
end;

function RandomColor: TColor;
begin
  result:=RGBToColor(random(256), random(256), random(256));
end;

procedure TForm1.EnsureLayerInfo(ALayer: TLayer);
var
  i: longint;
begin
  for i:=0 to high(Layers) do
    if Layers[i].Layer=ALayer then
      exit();

  setlength(layers, high(layers)+2);
  layers[high(layers)].Color:=RandomColor;
  layers[high(Layers)].Layer:=ALayer;
  layers[high(Layers)].Visible:=true;
end;

function TForm1.GetLayerInfoIdx(ALayer: TLayer): longint;
var
  i: longint;
begin
  EnsureLayerInfo(ALayer);

  result:=0;
  for i:=0 to high(Layers) do
    if Layers[i].Layer=ALayer then
      exit(i);
end;

function TForm1.GetLayerInfo(ALayer: TLayer): TLayerInfo;
var
  i: longint;
begin
  EnsureLayerInfo(ALayer);

  for i:=0 to high(Layers) do
    if Layers[i].Layer=ALayer then
      exit(layers[i]);
end;

end.

