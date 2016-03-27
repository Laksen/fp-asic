unit gdsreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  math;

type
  TXY = record
    X,Y: double;
  end;

  TPolygon = class
  private
    fLayer,
    fCount: longint;
    fPoints: array of TXY;
    function GetPoint(AIndex: longint): TXY;
    procedure SetPoint(AIndex: longint; AValue: TXY);
  public
    constructor Create(APoints, ALayer: longint);

    function MinPoint: TXY;
    function MaxPoint: TXY;

    property Layer: longint read fLayer;

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

  { TGDS2Reader }

  TGDS2Reader = class
  private
    fUnits: double;
    fStructures: TObjectList;

    fCurrPos,fNextPos,fLeftData: int64;
    fCurrent,fCurrentData: byte;

    function GetCount: longint;
    function GetStructure(AIndex: longint): TStructure;
    procedure InitParse(ASt: TStream);
    procedure Consume(ASt: TStream; ARecord: byte);

    function ReadWord(ASt: TStream): word;
    function ReadDWord(ASt: TStream): longint;
    function ReadQWord(ASt: TStream): qword;
    function ReadReal8(ASt: TStream): double;

    procedure ParseElement(ASt: TStream; AStructure: TStructure);
    procedure Parse(ASt: TStream);
  public
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    constructor Create();
    destructor Destroy; override;

    property Count: longint read GetCount;
    property Structure[AIndex: longint]: TStructure read GetStructure; default;
  end;

implementation

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

{$packrecords 1}

type
  THeader = record
    case integer of
      0: (data: longword);
      1: (
        length: word;
        rectype,
        datatype: byte);
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

constructor TPolygon.Create(APoints, ALayer: longint);
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

        poly:=TPolygon.Create(pts, layer);

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

procedure TGDS2Reader.LoadFromStream(AStream: TStream);
begin
  fStructures.Clear;
  Parse(AStream);
end;

procedure TGDS2Reader.SaveToStream(AStream: TStream);
begin

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

