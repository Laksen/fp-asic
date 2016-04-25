unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, ExtCtrls, ColorBox, Grids,
  gl, GLext,
  gdsreader, lefreader, libreader, blifreader,
  cells, geometry, drc,
  dom, XMLRead,
  StdCtrls, Types;

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
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
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
    TrackBar1: TTrackBar;
    tvTech: TTreeView;
    procedure AddFileItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure LayerGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure LayerGridCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure LayerGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure LayerGridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure LayerGridSetCheckboxState(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
    procedure MenuItem4Click(Sender: TObject);
    procedure oglViewMakeCurrent(Sender: TObject; var Allow: boolean);
    procedure oglViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure oglViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure oglViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure oglViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure oglViewPaint(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RenderMenuClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure tvTechSelectionChanged(Sender: TObject);
  private
    function PosToWorld(const APos: TPoint): TCoordinate;
  private
    GotCallList: boolean;
    CallList: array of GLuint;
    procedure RegenCallLists;
    procedure DrawLists;
  private
    DrawMode: TDrawMode;

    IsDragging: boolean;
    MouseStart,
    Mouse: TCoordinate;

    Offset: TCoordinate;
    Scale: int64;

    CurrentCell: TCell;

    procedure DrawCell(ACell: TCell; ALocation: TCoordinate; ALayer: longint);
    procedure DrawCell(ACell: TCell; ALocation: TCoordinate);
    procedure DrawNetlist;
    procedure ResetZoom;

    procedure DrawPolyC(const APoly: TPoly; ALayerIdx: longint);
    procedure DrawPoly(const APoly: TPoly);

    procedure DoLoadStack;
    procedure DoLoadGDS;
    procedure DoLoadLEF;
    procedure DoLoadLib;
    procedure DoLoadBLIF;

    procedure UpdateCells;
    procedure ReorderLayers;
  private
    Layers: array of TLayerInfo;

    procedure EnsureLayerInfo(ALayer: TLayer);
    function GetLayerInfoIdx(ALayer: TLayer): longint;
    function GetLayerInfo(ALayer: TLayer): TLayerInfo;
  end;

var
  Form1: TForm1;

implementation

uses
  math, chiplayout;

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

procedure TForm1.MenuItem4Click(Sender: TObject);
var
  p1, p2: TPoly;
  i, i2: longint;
  ori: TOrientation;
  s: String;
  p: TPolygon;
  w,l: TUnit;
begin
  s:='';

  for i:=0 to CurrentCell.PolygonCount-1 do
  begin
    p1:=CurrentCell.Polygons[i];

    if p1.Layer.Name<>'poly' then
      continue;

    for i2:=0 to CurrentCell.PolygonCount-1 do
      if i<>i2 then
      begin
        p2:=CurrentCell.Polygons[i2];

        if p2.Layer.Name<>'active' then
          continue;

        if IsMosOverlap(p1.Poly, p2.Poly, ori) then
        begin
          p:=GetOverlap(p1.Poly, p2.Poly);

          if ori=oAX then
          begin
            l:=p.Stop.Y-p.Start.Y;
            w:=p.Stop.X-p.Start.X;
          end
          else
          begin
            w:=p.Stop.Y-p.Start.Y;
            l:=p.Stop.X-p.Start.X;
          end;

          s:=s+Format('M a b c d nfet l=%fu w=%fu;', [UnitToMeters(l)*1e6,UnitToMeters(w)*1e6])+LineEnding;
        end;
      end;
  end;

  ShowMessage(s);
end;

procedure TForm1.oglViewMakeCurrent(Sender: TObject; var Allow: boolean);
begin
  Allow:=true;

  Load_GL_version_1_2;

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
    Mouse:=(PosToWorld(Point(x,y))-MouseStart);
    oglView.Repaint;
  end;
end;

procedure TForm1.oglViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if IsDragging then
  begin
    Mouse:=(PosToWorld(Point(x,y))-MouseStart);

    Offset:=offset+Mouse;

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

  offset:=(offset-(cp-np));

  oglView.Repaint;
end;

procedure TForm1.AddFileItemClick(Sender: TObject);
begin
  if tvTech.Items.TopLvlItems[0].Selected then
    DoLoadStack
  else if tvTech.Items.TopLvlItems[0].Items[0].Selected then
    DoLoadGDS
  else if tvTech.Items.TopLvlItems[0].Items[1].Selected then
    DoLoadLEF
  else if tvTech.Items.TopLvlItems[0].Items[2].Selected then
    DoLoadLib
  else if tvTech.Items.TopLvlItems[0].Items[3].Selected then
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
      begin
        glTranslated(0,0,-50);
        glScaled(1/Scale,1/scale,1);
        glTranslated(Offset.x,offset.y,0);

        glTranslated(mouse.x,mouse.y,0);

        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        DrawCell(CurrentCell, Coord(0,0));

        glDisable(GL_BLEND);
      end;
    dmNetList:
      DrawNetlist;
  end;

  oglView.SwapBuffers;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePageIndex=0 then
    tvTechSelectionChanged(sender)
  else
  begin
    DrawMode:=dmNetList;
    oglView.Refresh;

    ResetZoom;
  end;
end;

procedure TForm1.RenderMenuClick(Sender: TObject);
begin
  TMenuItem(sender).Checked:=not TMenuItem(sender).Checked;
  oglView.Repaint;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
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

procedure TForm1.RegenCallLists;
var
  layer, i: longint;
  li: TLayerInfo;
begin
  for i:=0 to high(CallList) do
    glDeleteLists(CallList[i], 1);

  setlength(CallList, length(layers));
  for layer:=0 to High(layers) do
  begin
    CallList[layer]:=glGenLists(1);

    glNewList(CallList[layer],GL_COMPILE);

    li:=layers[layer];
    glColor4ub(Red(li.Color), green(li.Color), blue(li.Color), TrackBar1.Position);

    for i:=0 to Layout.Count-1 do
    begin
      glPushMatrix();
      glTranslated(Layout.Instance[i].Location.x,Layout.Instance[i].Location.y,0);

      DrawCell(Layout.Instance[i].Cell, Layout.Instance[i].Location, layer);

      glPopMatrix();
    end;

    glEndList();
  end;

  GotCallList:=true;
end;

procedure TForm1.DrawLists;
var
  layer: longint;
begin
  if length(CallList)<>length(layers) then exit;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glTranslated(0,0,-50);
  glScaled(1/Scale,1/scale,1);
  glTranslated(Offset.x,offset.y,0);
  glTranslated(mouse.x,mouse.y,0);

  glEnable(GL_BLEND);
  glBlendColor(1,1,1,TrackBar1.Position/255);
  glBlendFunc(GL_CONSTANT_ALPHA, GL_ONE_MINUS_CONSTANT_ALPHA);

  for layer:=0 to high(Layers) do
  begin
    if not layers[layer].Visible then continue;
    glCallList(CallList[layer]);
  end;

  glDisable(GL_BLEND);
end;

procedure TForm1.DrawCell(ACell: TCell; ALocation: TCoordinate; ALayer: longint);
var
  i, i2: longint;
  l: TLayer;
begin
  l:=layers[ALayer].Layer;

  glBegin(GL_TRIANGLE_STRIP);

  if RenderLayersMenu.Checked then
    for i:=0 to ACell.PolygonCount-1 do
      if ACell.Polygons[i].Layer=l then
        DrawPolyC(ACell.Polygons[i], ALayer);

  if RenderObsMenu.Checked then
    for i:=0 to ACell.ObstructionCount-1 do
      if ACell.Obstructions[i].Layer=l then
        DrawPolyC(ACell.Obstructions[i], ALayer);

  if RenderPinsMenu.Checked then
    for i:=0 to ACell.PinsCount-1 do
      for i2:=0 to high(ACell.Pins[i].Polygons) do
         if ACell.Pins[i].Polygons[i2].Layer=l then
           DrawPolyC(ACell.Pins[i].Polygons[i2], ALayer);

  glEnd();
end;

procedure TForm1.DrawCell(ACell: TCell; ALocation: TCoordinate);
var
  i, i2, layer: longint;
  li: TLayerInfo;
  l: TLayer;
begin
  for layer:=0 to high(Layers) do
  begin
    if not layers[layer].Visible then continue;

    li:=layers[layer];
    glColor4ub(Red(li.Color), green(li.Color), blue(li.Color), TrackBar1.Position);

    l:=layers[layer].Layer;

    if RenderLayersMenu.Checked then
      for i:=0 to ACell.PolygonCount-1 do
        if ACell.Polygons[i].Layer=l then
          DrawPoly(ACell.Polygons[i]);

    if RenderObsMenu.Checked then
      for i:=0 to ACell.ObstructionCount-1 do
        if ACell.Obstructions[i].Layer=l then
          DrawPoly(ACell.Obstructions[i]);

    if RenderPinsMenu.Checked then
      for i:=0 to ACell.PinsCount-1 do
        for i2:=0 to high(ACell.Pins[i].Polygons) do
           if ACell.Pins[i].Polygons[i2].Layer=l then
             DrawPoly(ACell.Pins[i].Polygons[i2]);
  end;
end;

procedure TForm1.DrawNetlist;
var
  i: longint;
begin
  if not GotCallList then
    RegenCallLists;
  DrawLists;
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
      MinMax(CurrentCell.Polygons[0].Poly, mi, ma);

      for i:=1 to CurrentCell.PolygonCount-1 do
      begin
        MinMax(CurrentCell.Polygons[i].Poly, ti, ta);

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
        MinMax(CurrentCell.Pins[i2].Polygons[0].Poly, mi, ma);
        Start:=1;
      end
      else
        Start:=0;

      for i:=start to high(CurrentCell.Pins[i2].Polygons) do
      begin
        MinMax(CurrentCell.Pins[i2].Polygons[i].Poly, ti, ta);

        MinMax(mi,ti,mi,ma);
        MinMax(mi,ta,mi,ma);
      end;
    end;

    if CurrentCell.ObstructionCount>0 then
    begin
      if not HasFirst then
      begin
        HasFirst:=true;
        MinMax(CurrentCell.Obstructions[0].Poly, mi, ma);
        Start:=1;
      end
      else
        Start:=0;

      for i:=start to CurrentCell.ObstructionCount-1 do
      begin
        MinMax(CurrentCell.Obstructions[i].Poly, ti, ta);

        MinMax(mi,ti,mi,ma);
        MinMax(mi,ta,mi,ma);
      end;
    end;

    dx:=ma.x-mi.x;
    dy:=ma.y-mi.y;

    Scale:=max(dx div oglView.ClientWidth,dy div oglView.ClientHeight);
    Offset:=Coord(0,0);
  end;
end;

procedure TForm1.DrawPolyC(const APoly: TPoly; ALayerIdx: longint);
begin
  glVertex3d(APoly.Poly.Points[0].X, APoly.Poly.Points[0].y, ALayerIdx);

  glVertex3d(APoly.Poly.Points[0].X, APoly.Poly.Points[0].y, ALayerIdx);
  glVertex3d(APoly.Poly.Points[1].X, APoly.Poly.Points[1].y, ALayerIdx);
  glVertex3d(APoly.Poly.Points[3].X, APoly.Poly.Points[3].y, ALayerIdx);
  glVertex3d(APoly.Poly.Points[2].X, APoly.Poly.Points[2].y, ALayerIdx);

  glVertex3d(APoly.Poly.Points[2].X, APoly.Poly.Points[2].y, ALayerIdx);
end;

procedure TForm1.DrawPoly(const APoly: TPoly);
var
  i, ALayerIdx: longint;
begin
  if APoly.Poly.Rect then
  begin
    glBegin(GL_TRIANGLE_STRIP);

    ALayerIdx:=GetLayerInfoIdx(APoly.Layer);
    glVertex3d(APoly.Poly.Points[0].X, APoly.Poly.Points[0].y, ALayerIdx);
    glVertex3d(APoly.Poly.Points[1].X, APoly.Poly.Points[1].y, ALayerIdx);
    glVertex3d(APoly.Poly.Points[3].X, APoly.Poly.Points[3].y, ALayerIdx);
    glVertex3d(APoly.Poly.Points[2].X, APoly.Poly.Points[2].y, ALayerIdx);

    glEnd();
  end
  else
  begin
    glBegin(GL_POLYGON);

    for i:=0 to high(APoly.Poly.Points) do
      glVertex3d(APoly.Poly.Points[i].X, APoly.Poly.Points[i].y, GetLayerInfoIdx(APoly.Layer));

    glEnd();
  end;
end;

procedure TForm1.DoLoadStack;
var
  FileName, map: String;
  doc: TXMLDocument;
  nodes: TDOMNodeList;
  i: longint;
  node: TDOMElement;
begin
  OpenDialog1.Filter:='Stack Files|*.xml';
  OpenDialog1.Title:='Select stack file';

  if OpenDialog1.Execute then
    FileName:=OpenDialog1.FileName
  else
    exit;

  ReadXMLFile(doc, OpenDialog1.FileName);

  nodes:=doc.GetElementsByTagName('gds2');
  for i:=0 to nodes.Count-1 do
  begin
    node:=TDOMElement(nodes[i]);

    map:='';
    if node.hasAttribute('map') then
      map:=string(node.GetAttribute('map'));

    filename:=string(node.TextContent);

    LoadGDS(FileName, map);

    tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[0], ExtractRelativepath(GetCurrentDir, FileName));
    tvTech.Items.TopLvlItems[0].items[0].Expand(true);
  end;
  nodes.free;

  nodes:=doc.GetElementsByTagName('lef');
  for i:=0 to nodes.Count-1 do
  begin
    node:=TDOMElement(nodes[i]);

    filename:=string(node.TextContent);

    LoadLEF(FileName);

    tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[1], ExtractRelativepath(GetCurrentDir, Filename));
    tvTech.Items.TopLvlItems[0].items[1].Expand(true);
  end;
  nodes.free;

  nodes:=doc.GetElementsByTagName('lib');
  for i:=0 to nodes.Count-1 do
  begin
    node:=TDOMElement(nodes[i]);

    filename:=string(node.TextContent);

    Loadlib(FileName);

    tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[2], ExtractRelativepath(GetCurrentDir, Filename));
    tvTech.Items.TopLvlItems[0].items[2].Expand(true);
  end;
  nodes.free;

  nodes:=doc.GetElementsByTagName('blif');
  for i:=0 to nodes.Count-1 do
  begin
    node:=TDOMElement(nodes[i]);

    filename:=string(node.TextContent);

    LoadBlif(FileName);
    LayoutRows;

    tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[3], ExtractRelativepath(GetCurrentDir, Filename));
    tvTech.Items.TopLvlItems[0].items[3].Expand(true);
  end;
  nodes.free;

  UpdateCells;

  ReorderLayers;
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

procedure TForm1.DoLoadLib;
var
  FileName: String;
begin
  OpenDialog1.Filter:='Liberty Files|*.lib';
  OpenDialog1.Title:='Select LIB file';

  if OpenDialog1.Execute then
    FileName:=OpenDialog1.FileName
  else
    exit;

  LoadLib(FileName);

  tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[2], ExtractRelativepath(GetCurrentDir, Filename));
  tvTech.Items.TopLvlItems[0].items[2].Expand(true);

  UpdateCells;
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

  LoadBlif(FileName);
  LayoutRows;

  tvTech.Items.AddChild(tvTech.Items.TopLvlItems[0].Items[3], ExtractRelativepath(GetCurrentDir, Filename));
  tvTech.Items.TopLvlItems[0].items[3].Expand(true);

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

