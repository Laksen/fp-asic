unit libparser;

interface

{$mode objfpc}{$H+}

uses
  classes, sysutils,
  libscanner, baseparser;

type
  TEntry = class;
  TEntries = array of TEntry;

  TValues = array of Variant;

  TEntry = class
  private
    fName: string;
  public
    function GetSingle(const AName: string): TEntry; virtual;
    function GetMultiple(const AName: string): TEntries; virtual;

    function GetValue: variant; virtual;
    function GetValues: TValues; virtual;

    constructor Create(const AName: string);

    property Name: string read fName;
  end;

  TValue = class(TEntry)
  private
    fValue: variant;
  public
    function GetValue: variant; override;

    constructor Create(const AName: string; const AValue: variant);
  end;

  TSubEntry = class(TEntry)
  private
    fEntries: TStringList;
    fValues: TValues;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    function GetSingle(const AName: string): TEntry; override;
    function GetMultiple(const AName: string): TEntries; override;

    function GetValues: TValues; override;

    procedure AddParam(const AValue: variant);
    procedure AddSub(AEntry: TEntry);
  end;

  TParser = class(TBaseParser)
  public
    function ENTRY: TEntry;
    function ENTRYCONTENT(const AName: string): TEntry;
    function SUBENTRY(const AName: string): TEntry;
    procedure SUBENTRYCONTENT(AEntry: TSubEntry);
    function Value: variant;
    procedure Name;
    function EXPRESSION: variant;
    function TERM: variant;
  end;

implementation

constructor TSubEntry.Create(const AName: string);
begin
  inherited Create(AName);
  fEntries:=TStringList.Create;
  fEntries.CaseSensitive:=true;
  fEntries.Sorted:=true;
  fEntries.Duplicates:=dupAccept;
  fEntries.OwnsObjects:=true;
end;

destructor TSubEntry.Destroy;
begin
  fEntries.Free;
  inherited Destroy;
end;

function TSubEntry.GetSingle(const AName: string): TEntry;
var
  res: TEntries;
begin
  res:=GetMultiple(AName);

  if length(res) < 1 then
    result:=nil
  else
    result:=res[0];
end;

function TSubEntry.GetMultiple(const AName: string): TEntries;
var
  i, idx, cnt: longint;
begin
  idx:=fEntries.IndexOf(AName);

  if idx<0 then
  begin
    setlength(result,0);
    exit;
  end;

  cnt:=0;
  for i:=idx to fEntries.Count-1 do
    if fEntries[i]=AName then
      inc(cnt)
    else
      break;

  SetLength(result, cnt);
  for i:=0 to cnt-1 do
    result[i]:=TEntry(fEntries.Objects[idx+i]);
end;

function TSubEntry.GetValues: TValues;
begin
  result:=fValues;
end;

procedure TSubEntry.AddParam(const AValue: variant);
begin
  setlength(fValues, high(fValues)+2);
  fValues[high(fValues)]:=AValue;
end;

procedure TSubEntry.AddSub(AEntry: TEntry);
begin
  fEntries.AddObject(AEntry.Name, AEntry);
end;

function TValue.GetValue: variant;
begin
  result:=fValue;
end;

constructor TValue.Create(const AName: string; const AValue: variant);
begin
  inherited Create(AName);
  fValue:=AValue;
end;

function TEntry.GetSingle(const AName: string): TEntry;
begin
  result:=nil;
end;

function TEntry.GetMultiple(const AName: string): TEntries;
begin
  setlength(result,0);
end;

function TEntry.GetValue: variant;
var
  res: TValues;
begin
  res:=GetValues;

  result:=nil;
  if length(res)>0 then
    result:=res[0];
end;

function TEntry.GetValues: TValues;
begin
  setlength(result,0);
end;

constructor TEntry.Create(const AName: string);
begin
  inherited Create;
  fName:=AName;
end;

function TParser.ENTRY: TEntry;
var
  nm: String;
begin
  nm:=Pr(tkIDENTIFIER);
  result:=ENTRYCONTENT(nm);
end;

function TParser.ENTRYCONTENT(const AName: string): TEntry;
begin
  if Token in [tkLParan] then
  begin
    result:=SUBENTRY(AName);
  end
  else
  begin
    result:=TValue.Create(AName, Value());
  end;
end;

function TParser.SUBENTRY(const AName: string): TEntry;
begin
  result:=TSubEntry.Create(AName);

  Pr(tkLParan);
  if Token in [tkIDENTIFIER, tkNUMBER, tkSTRING] then
  begin
    TSubEntry(result).AddParam(EXPRESSION());
    while Token in [tkComma] do
    begin
      Pr(tkComma);
      TSubEntry(result).AddParam(EXPRESSION());
    end;
  end;
  Pr(tkRParan);

  SUBENTRYCONTENT(TSubEntry(result));
end;

procedure TParser.SUBENTRYCONTENT(AEntry: TSubEntry);
begin
  if Token in [tkSemicolon] then
  begin
    Pr(tkSemicolon);
  end
  else if Token in [tkLBracket] then
  begin
    Pr(tkLBracket);

    while Token in [tkIDENTIFIER] do
      AEntry.AddSub(ENTRY());

    Pr(tkRBracket);
  end;
end;

function TParser.Value: variant;
begin
  Pr(tkColon);
  result:=EXPRESSION();
  Pr(tkSemicolon);
end;

procedure TParser.Name;
begin
  Pr(tkIDENTIFIER);
end;

function TParser.EXPRESSION: variant;
begin
  result:=TERM();
  while Token in [tkPipe] do
  begin
    Pr(tkPipe);
    result:=result+'|'+TERM();
  end;
end;

function TParser.TERM: variant;
begin
  if Token in [tkIDENTIFIER] then
  begin
    result:=Pr(tkIDENTIFIER);
  end
  else if Token in [tkSTRING] then
  begin
    result:=Pr(tkSTRING);
  end
  else
  begin
    result:=Pr(tkNUMBER);
  end;
end;

end.
