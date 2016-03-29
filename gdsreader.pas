unit gdsreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, strutils,
  math;

type
  TXY = record
    X,Y: double;
  end;

  TPolygon = class
  private
    fLayer: string;
    fCount: longint;
    fPoints: array of TXY;
    function GetPoint(AIndex: longint): TXY;
    procedure SetPoint(AIndex: longint; AValue: TXY);
  public
    constructor Create(APoints: longint; const ALayer: string);

    function MinPoint: TXY;
    function MaxPoint: TXY;

    property Layer: string read fLayer;

    property Count: longint read fCount;
    property Point[AIndex: longint]: TXY read GetPoint write SetPoint; default;
  end;

  TStructure = class
  private
    fName: String;
    fPolygons: TObjectList;
    function GetCount: longint;
    function GetPolygon(AIndex: longint): TPolygon;
  protected
    procedure AddPoly(APoly: TPolygon);
  public
    function MinPoint: TXY;
    function MaxPoint: TXY;

    constructor Create(const AName: string);
    destructor Destroy; override;

    property Name: string read fName;
    property Count: longint read GetCount;
    property Polygon[AIndex: longint]: TPolygon read GetPolygon; default;
  end;

  TGDS2Layer = record
    ID: longint;
    Name: string;
  end;

  TGDS2LayerMap = array of TGDS2Layer;

  TGDS2Reader = class
  private
    fUnits: double;
    fStructures: TObjectList;

    fLayers: TGDS2LayerMap;

    fCurrPos,fNextPos,fLeftData: int64;
    fCurrent,fCurrentData: byte;

    function GetCount: longint;
    function GetStructure(AIndex: longint): TStructure;
    function GetLayer(AIndex: longint): string;
    function GetLayerID(const AName: string): longint;

    procedure InitParse(ASt: TStream);
    procedure Consume(ASt: TStream; ARecord: byte);

    function ReadWord(ASt: TStream): word;
    function ReadDWord(ASt: TStream): longint;
    function ReadQWord(ASt: TStream): qword;
    function ReadReal8(ASt: TStream): double;

    procedure ParseElement(ASt: TStream; AStructure: TStructure);
    procedure Parse(ASt: TStream);

    procedure WriteRecord(AStream: TStream; ARecType, ADataType: byte; ALength: longint);
    procedure WriteWord(AStream: TStream; AData: smallint);
    procedure WriteDWord(AStream: TStream; AData: longint);
    procedure WriteZeros(AStream: TStream; ALength: longint);
    procedure WriteString(AStream: TStream; const AString: string);
    procedure WriteReal8(AStream: TStream; AValue: double);
  public
    procedure LoadFromStream(AStream: TStream; const ALayers: TGDS2LayerMap = nil);
    procedure SaveToStream(AStream: TStream);

    constructor Create();
    destructor Destroy; override;

    property Count: longint read GetCount;
    property Structure[AIndex: longint]: TStructure read GetStructure; default;

    property Units: double read fUnits write fUnits;
  end;

procedure LoadGDS(const AFilename, AMapFilename: string);
function LoadMap(const AFilename: string): TGDS2LayerMap;

implementation

uses
  cells;

function XY(AX, AY: double): TXY;
begin
  result.x:=ax;
  result.y:=ay;
end;

function Min(const A,B: TXY): TXY;
begin
  result.x:=math.min(a.x, b.x);
  result.y:=math.min(a.y, b.y);
end;

function Max(const A,B: TXY): TXY;
begin
  result.x:=math.max(a.x, b.x);
  result.y:=math.max(a.y, b.y);
end;

const
  GDS_DATA_None = 0;
  GDS_DATA_Bits = 1;
  GDS_DATA_16 =   2;
  GDS_DATA_32 = 3;
  GDS_DATA_REAL4 = 4;
  GDS_DATA_REAL8  = 5;
  GDS_DATA_STRING = 6;

  GDS_HEADER    = 0;
  GDS_BGNLIB    = 1;
  GDS_LIBNAME   = 2;
  GDS_UNITS     = 3;
  GDS_ENDLIB    = 4;
  GDS_BGNSTR    = 5;
  GDS_STRNAME   = 6;
  GDS_ENDSTR    = 7;
  GDS_BOUNDARY  = 8;
  GDS_PATH      = 9;
  GDS_SREF      = $A;
  GDS_AREF      = $B;
  GDS_TEST      = $C;
  GDS_LAYER     = $D;
  GDS_DATATYPE  = $E;
  GDS_WIDTH     = $F;
  GDS_XY        = $10;
  GDS_ENDEL     = $11;
  GDS_SNAME     = $12;
  GDS_COLROW    = $13;
  GDS_TEXTNODE  = $14;
  GDS_NODE      = $15;
  GDS_TEXTTYPE  = $16;
  GDS_PRESENTATION = $17;
  GDS_SPACING   = $18;
  GDS_STRING    = $19;
  GDS_STRANS    = $1A;
  GDS_MAG       = $1B;
  GDS_ANGLE     = $1C;
  GDS_USTRING   = $1E;
  GDS_REFLIBS   = $1F;
  GDS_FONTS     = $20;
  GDS_PATHTYPE  = $20;
  GDS_GENERATIONS = $22;
  GDS_ATTRTABLE = $23;
  GDS_STYPTABLE = $24;
  GDS_STRTYPE   = $25;
  GDS_ELFLAGS   = $26;
  GDS_ELKEY     = $27;
  GDS_LINKTYPE  = $28;
  GDS_LINKKEYS  = $29;
  GDS_NODETYPE  = $2A;
  GDS_PROPATTR  = $2B;
  GDS_PROPVALUE = $2C;
  GDS_BOX       = $2D;
  GDS_BOXTYPE   = $2E;
  GDS_PLEX      = $2F;
  GDS_BGNEXTN   = $30;
  GDS_ENDTEXTN  = $31;
  GDS_TAPENUM   = $32;
  GDS_TAPECODE  = $33;
  GDS_STRCLASS  = $34;
  GDS_RESERVED  = $35;
  GDS_FORMAT    = $36;
  GDS_MASK      = $37;
  GDS_ENDMASKS  = $38;
  GDS_LIBDIRSIZE = $39;
  GDS_STRFNAME  = $3A;
  GDS_LIBSECUR  = $3B;

procedure LoadGDS(const AFilename, AMapFilename: string);
var
  map: TGDS2LayerMap;
  gd: TGDS2Reader;
  s: TFileStream;
  str: TStructure;
  i, i2, i3: longint;
  c: TCell;
  poly: TPolygon;
  layer: TLayer;
  p: TPoly;
begin
  if (AMapFilename<>'') and
     FileExists(AMapFilename) then
    map:=LoadMap(AMapFilename)
  else
    map:=nil;

  gd:=TGDS2Reader.Create;
  try
    s:=TFileStream.Create(AFilename, fmOpenRead);
    try
      gd.LoadFromStream(s);

      for i:=0 to gd.Count-1 do
      begin
        str:=gd[i];

        if HasCell(str.Name) then continue;

        c:=TCell.Create(str.Name);

        for i2:=0 to str.Count-1 do
        begin
          poly:=str[i2];

          layer:=GetLayer(poly.Layer);

          p.Layer:=layer;
          setlength(p.Points, poly.Count);
          for i3:=0 to poly.Count-1 do
            p.points[i3]:=GetCoord(poly[i3].X, poly[i3].y);

          c.AddPoly(p);
        end;

        RegisterCell(c);
      end;
    finally
      s.free;
    end;
  finally
    gd.Free;
  end;
end;

function LoadMap(const AFilename: string): TGDS2LayerMap;
var
  st: TStringList;
  s, name: String;
  i, id: longint;
begin
  setlength(result,0);

  st:=TStringList.Create;
  try
    st.LoadFromFile(AFilename);

    for i:=0 to st.Count-1 do
    begin
      s:=trim(st[i]);
      if s='' then continue;
      if pos('#',s)=1 then continue;

      name:=Copy2SpaceDel(s); s:=trim(s);
      Copy2SpaceDel(s); s:=trim(s);
      id:=strtoint(Copy2SpaceDel(s));

      setlength(result, high(result)+2);
      result[high(result)].ID:=id;
      result[high(result)].Name:=name;
    end;
  finally
    st.free;
  end;
end;

function TStructure.GetCount: longint;
begin
  result:=fPolygons.Count;
end;

function TStructure.GetPolygon(AIndex: longint): TPolygon;
begin
  result:=TPolygon(fPolygons[AIndex]);
end;

procedure TStructure.AddPoly(APoly: TPolygon);
begin
  fPolygons.Add(APoly);
end;

function TStructure.MinPoint: TXY;
var
  i: longint;
begin
  result:=TPolygon(fPolygons[0]).MinPoint;
  for i:=1 to Count-1 do
    result:=min(result,Polygon[i].MinPoint);
end;

function TStructure.MaxPoint: TXY;
var
  i: longint;
begin
  result:=TPolygon(fPolygons[0]).MaxPoint;
  for i:=1 to Count-1 do
    result:=max(result,Polygon[i].MaxPoint);
end;

constructor TStructure.Create(const AName: string);
begin
  inherited Create;
  fName:=AName;
  fPolygons:=TObjectList.Create(true);
end;

destructor TStructure.Destroy;
begin
  fPolygons.Free;
  inherited Destroy;
end;

function TPolygon.GetPoint(AIndex: longint): TXY;
begin
  result:=fPoints[AIndex];
end;

procedure TPolygon.SetPoint(AIndex: longint; AValue: TXY);
begin
  fPoints[AIndex]:=AValue;
end;

constructor TPolygon.Create(APoints: longint; const ALayer: string);
begin
  inherited Create;
  fLayer:=ALayer;
  setlength(fPoints, APoints);
  fCount:=APoints
end;

function TPolygon.MinPoint: TXY;
var
  i: longint;
begin
  result:=fPoints[0];
  for i:=1 to high(fPoints) do
    result:=Min(result, fPoints[i]);
end;

function TPolygon.MaxPoint: TXY;
var
  i: longint;
begin
  result:=fPoints[0];
  for i:=1 to high(fPoints) do
    result:=Max(result,fPoints[i]);
end;

procedure TGDS2Reader.InitParse(ASt: TStream);
var
  l: Word;
begin
  fCurrPos:=ASt.Position;

  l:=beton(ASt.ReadWord);
  fCurrent:=ASt.ReadByte;
  fCurrentData:=ASt.ReadByte;

  fNextPos:=fCurrPos+l;
  fLeftData:=l-4;
end;

function TGDS2Reader.GetCount: longint;
begin
  result:=fStructures.Count;
end;

function TGDS2Reader.GetStructure(AIndex: longint): TStructure;
begin
  result:=TStructure(fStructures[AIndex]);
end;

function TGDS2Reader.GetLayer(AIndex: longint): string;
var
  i: longint;
begin
  for i:=0 to high(fLayers) do
    if fLayers[i].ID=AIndex then
      exit(fLayers[i].Name);

  result:=inttostr(AIndex);
end;

function TGDS2Reader.GetLayerID(const AName: string): longint;
begin
  result:=0;
end;

procedure TGDS2Reader.Consume(ASt: TStream; ARecord: byte);
var
  l: Word;
begin
  if ARecord<>fCurrent then
    writeln('Expected ',ARecord,' but was at ', fCurrent);

  ASt.Seek(fNextPos, soFromBeginning);
  fCurrPos:=aSt.Position;

  if ASt.Position>=ASt.Size then
    begin
      fCurrent:=255;
      exit;
    end;

  l:=beton(ASt.ReadWord);
  fCurrent:=ASt.ReadByte;
  fCurrentData:=ASt.ReadByte;

  fNextPos:=fCurrPos+l;
  fLeftData:=l-4;
end;

function TGDS2Reader.ReadWord(ASt: TStream): word;
begin
  result:=BEtoN(Ast.ReadWord);
end;

function TGDS2Reader.ReadDWord(ASt: TStream): longint;
begin
  result:=BEtoN(longint(ASt.ReadDWord));
end;

function TGDS2Reader.ReadQWord(ASt: TStream): qword;
begin
  result:=BEtoN(ASt.ReadQWord);
end;

function TGDS2Reader.ReadReal8(ASt: TStream): double;
var
  x: qword;
  s: Byte;
  sign, exponent: longint;
begin
  x:=NtoBE(ReadQWord(ASt));

  s:=(x and $FF);
  x:=x shr 8;
  x:=BEtoN(x) shr 8;

  result:=x/power(2, 56);

  exponent:=(s and $7F);
  if (s and $80)<>0 then
    result:=-result;

  result:=result * power(16,exponent-64);
end;

procedure TGDS2Reader.ParseElement(ASt: TStream; AStructure: TStructure);
var
  Layer: Word;
  i, pts: longint;
  x, y: longint;
  poly: TPolygon;
begin
  case fCurrent of
    GDS_BOUNDARY:
      begin
        Consume(ASt, GDS_BOUNDARY);
        if fCurrent=GDS_ELFLAGS then
          Consume(ASt, GDS_ELFLAGS);
        if fCurrent=GDS_PLEX then
          Consume(ASt, GDS_PLEX);

        Layer:=ReadWord(ASt);
        Consume(ASt, GDS_LAYER);

        Consume(ASt, GDS_DATATYPE);

        pts:=fLeftData div 8;

        poly:=TPolygon.Create(pts, GetLayer(layer));

        for i:=0 to pts-1 do
        begin
          x:=ReadDWord(ASt);
          y:=ReadDWord(ASt);

          poly.Point[i]:=XY(x*fUnits,y*fUnits);
        end;
        AStructure.AddPoly(poly);

        Consume(ASt, GDS_XY);
      end;
    GDS_BOX:
      begin
        Consume(ASt, GDS_BOX);
        if fCurrent=GDS_ELFLAGS then Consume(ASt, GDS_ELFLAGS);
        if fCurrent=GDS_PLEX then Consume(ASt, GDS_PLEX);

        Layer:=ReadWord(ASt);
        Consume(ASt, GDS_LAYER);

        Consume(ASt, GDS_BOXTYPE);

        Consume(ASt, GDS_XY);
      end;
    GDS_PATH:
      begin
        Consume(ASt, GDS_PATH);
        if fCurrent=GDS_ELFLAGS then Consume(ASt, GDS_ELFLAGS);
        if fCurrent=GDS_PLEX then Consume(ASt, GDS_PLEX);

        Layer:=ReadWord(ASt);
        Consume(ASt, GDS_LAYER);

        Consume(ASt, GDS_DATATYPE);

        if fCurrent=GDS_PATHTYPE then Consume(ASt, GDS_PATHTYPE);
        if fCurrent=GDS_WIDTH then Consume(ASt, GDS_WIDTH);
        if fCurrent=GDS_BGNEXTN then Consume(ASt, GDS_BGNEXTN);
        if fCurrent=GDS_ENDTEXTN then Consume(ASt, GDS_ENDTEXTN);

        Consume(ASt, GDS_XY);
      end;
    else
      begin
        while fCurrent<>GDS_ENDEL do
          consume(ASt, fCurrent);
        //writeln('Element: ', fCurrent);
      end;
  end;
  Consume(ASt, GDS_ENDEL);
end;

procedure TGDS2Reader.Parse(ASt: TStream);
var
  str: string;
  struct: TStructure;
begin
  InitParse(ASt);

  Consume(ASt, GDS_HEADER);
  Consume(ASt, GDS_BGNLIB);
  if fCurrent=GDS_LIBDIRSIZE then Consume(ASt, GDS_LIBDIRSIZE);
  if fCurrent=GDS_STRFNAME then Consume(ASt, GDS_STRFNAME);
  if fCurrent=GDS_LIBSECUR then Consume(ASt, GDS_LIBSECUR);

  Consume(ASt, GDS_LIBNAME);

  if fCurrent=GDS_REFLIBS then Consume(ASt, GDS_REFLIBS);
  if fCurrent=GDS_FONTS then Consume(ASt, GDS_FONTS);
  if fCurrent=GDS_ATTRTABLE then Consume(ASt, GDS_ATTRTABLE);
  if fCurrent=GDS_GENERATIONS then Consume(ASt, GDS_GENERATIONS);

  if fCurrent=GDS_FORMAT then
    begin
      Consume(ASt, GDS_FORMAT);
      if fCurrent=GDS_MASK then
        begin
          while fCurrent=GDS_MASK do
            Consume(ASt, GDS_MASK);
          Consume(ASt, GDS_ENDMASKS);
        end;
    end;

  ReadReal8(ASt);
  fUnits:=ReadReal8(ASt);
  Consume(ASt, GDS_UNITS);

  while fCurrent=GDS_BGNSTR do
    begin
      Consume(ASt, GDS_BGNSTR);

      setlength(str, fLeftData);
      ASt.Read(str[1], fLeftData);
      str:=TrimRight(str);

      struct:=TStructure.Create(str);
      Consume(ASt, GDS_STRNAME);
      if fCurrent=GDS_STRCLASS then Consume(ASt, GDS_STRCLASS);

      while fCurrent<>GDS_ENDSTR do
        ParseElement(ast, struct);

      fStructures.Add(struct);

      Consume(ASt, GDS_ENDSTR);
    end;

  Consume(ASt, GDS_ENDLIB);
end;

procedure TGDS2Reader.LoadFromStream(AStream: TStream; const ALayers: TGDS2LayerMap);
begin
  fStructures.Clear;
  fLayers:=Copy(ALayers);
  Parse(AStream);
end;

procedure TGDS2Reader.WriteRecord(AStream: TStream; ARecType, ADataType: byte; ALength: longint);
var
  l: word;
begin
  l:=(ALength+4+1) and $FFFE;

  AStream.WriteWord(NtoBE(l));
  AStream.WriteByte(ARecType);
  AStream.WriteByte(ADataType);
end;

procedure TGDS2Reader.WriteWord(AStream: TStream; AData: smallint);
begin
  AData:=NtoBE(AData);
  AStream.Write(AData, 2);
end;

procedure TGDS2Reader.WriteDWord(AStream: TStream; AData: longint);
begin
  AData:=NtoBE(AData);
  AStream.Write(AData, 4);
end;

procedure TGDS2Reader.WriteZeros(AStream: TStream; ALength: longint);
begin
  while alength>0 do
    begin
      AStream.WriteByte(0);
      dec(alength);
    end;
end;

procedure TGDS2Reader.WriteString(AStream: TStream; const AString: string);
begin
  AStream.Write(AString[1], length(AString));
  if odd(length(AString)) then
    AStream.WriteByte(0);
end;

procedure Encode(AVal: double; out AEncoded: qword);
var
  ex: Integer;
  aexp: byte;
begin
  if AVal<0 then
    begin
      AExp:=$80;
      AVal:=-AVal;
    end
  else
    AExp:=0;

  ex:=floor(logn(16, AVal))+1;
  AExp:=AExp or (ex+64) and $7F;

  AVal:=AVal*power(16, -ex);

  AEncoded:=((round(AVal*power(2,56))) and $FFFFFFFFFFFFFF) or (qword(aexp) shl 56);
end;

procedure TGDS2Reader.WriteReal8(AStream: TStream; AValue: double);
var
  ex: Integer;
  aexp: byte;
  AEncoded: qword;
begin
  if AValue<0 then
    begin
      AExp:=$80;
      AValue:=-AValue;
    end
  else
    AExp:=0;

  ex:=floor(logn(16, AValue))+1;
  AExp:=AExp or (ex+64) and $7F;

  AValue:=AValue*power(16, -ex);

  AEncoded:=((round(AValue*power(2,56))) and $FFFFFFFFFFFFFF) or (qword(aexp) shl 56);

  AStream.WriteQWord(NtoBE(AEncoded));
end;

procedure TGDS2Reader.SaveToStream(AStream: TStream);
var
  i, i2, i3: longint;
  struct: TStructure;
  poly: TPolygon;
  pt: TXY;
begin
  WriteRecord(AStream, GDS_HEADER, GDS_DATA_16, 2);
  WriteWord(AStream, 6);

  WriteRecord(AStream, GDS_BGNLIB, GDS_DATA_16, $18);
  WriteZeros(AStream, $18);
  //if fCurrent=GDS_LIBDIRSIZE then WriteRecord(AStream, GDS_LIBDIRSIZE);
  //if fCurrent=GDS_STRFNAME then WriteRecord(AStream, GDS_STRFNAME);
  //if fCurrent=GDS_LIBSECUR then WriteRecord(AStream, GDS_LIBSECUR);

  WriteRecord(AStream, GDS_LIBNAME, GDS_DATA_STRING, 2);
  WriteString(AStream, 'NA');

  {if fCurrent=GDS_REFLIBS then WriteRecord(AStream, GDS_REFLIBS);
  if fCurrent=GDS_FONTS then WriteRecord(AStream, GDS_FONTS);
  if fCurrent=GDS_ATTRTABLE then WriteRecord(AStream, GDS_ATTRTABLE);
  if fCurrent=GDS_GENERATIONS then WriteRecord(AStream, GDS_GENERATIONS);}

  {if fCurrent=GDS_FORMAT then
    begin
      WriteRecord(AStream, GDS_FORMAT);
      if fCurrent=GDS_MASK then
        begin
          while fCurrent=GDS_MASK do
            WriteRecord(AStream, GDS_MASK);
          WriteRecord(AStream, GDS_ENDMASKS);
        end;
    end;}

  WriteRecord(AStream, GDS_UNITS, GDS_DATA_REAL8, 16);
  WriteReal8(AStream, 1);
  WriteReal8(AStream, fUnits);

  for i:=0 to Count-1 do
    begin
      struct:=Structure[i];

      WriteRecord(AStream, GDS_BGNSTR, GDS_DATA_16, $18);
      WriteZeros(AStream, $18);

      WriteRecord(AStream, GDS_STRNAME, GDS_DATA_STRING, length(struct.name));
      WriteString(AStream, Struct.Name);

      //if fCurrent=GDS_STRCLASS then WriteRecord(AStream, GDS_STRCLASS);

      for i2:=0 to struct.Count-1 do
        begin
          poly:=struct.Polygon[i2];

          WriteRecord(AStream, GDS_BOUNDARY, GDS_DATA_None, 0);

          WriteRecord(AStream, GDS_LAYER, GDS_DATA_16, 2);
          WriteWord(AStream, GetLayerID(poly.Layer));

          WriteRecord(AStream, GDS_DATATYPE, GDS_DATA_16, 2);
          WriteWord(AStream, 0);

          WriteRecord(AStream, GDS_XY, GDS_DATA_32, poly.Count*8);

          for i3:=0 to poly.Count-1 do
          begin
            pt:=poly[i3];

            WriteDWord(AStream, round(pt.X/fUnits));
            WriteDWord(AStream, round(pt.Y/fUnits));
          end;

          WriteRecord(AStream, GDS_ENDEL, GDS_DATA_None, 0);
        end;

      WriteRecord(AStream, GDS_ENDSTR, GDS_DATA_None, 0);
    end;

  WriteRecord(AStream, GDS_ENDLIB, GDS_DATA_None, 0);
end;

constructor TGDS2Reader.Create;
begin
  inherited Create;
  fStructures:=TObjectList.Create(true);
end;

destructor TGDS2Reader.Destroy;
begin
  fStructures.Free;
  inherited Destroy;
end;

end.

