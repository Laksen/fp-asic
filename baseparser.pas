unit baseparser;

interface

uses
  sysutils, classes,
  libscanner;

type
  TBaseParser = class
  protected
    fScanner: TScanner;               
  public
    procedure Error(const str: string);
    procedure Expect(const ATokens: TTokenSet);

    function Pr(AToken: TToken): string;
    function Token: TToken;
    
    constructor Create(AStream: tstream);
    destructor Destroy; override;       
  end;

implementation

procedure TBaseParser.Error(const str: string);
begin
  raise exception.Create(str);
end;

procedure TBaseParser.Expect(const ATokens: TTokenSet);
var
  s: String;
  first: Boolean;
  r: TToken;
begin
  if not (fScanner.Token in ATokens) then
    begin
      s:='';
      first:=true;

      for r in ATokens do
        begin
          if first then s:=TokenNames[r] else s:=s+','+TokenNames[r];
          first:=false;
        end;

      raise Exception.Create('Parse error at line '+inttostr(fScanner.Line)+': '+inttostr(fScanner.Position)+': Expected any of ['+s+'] but found "' + TokenNames[fScanner.Token]+'"');
    end;
end;

function TBaseParser.Pr(AToken: TToken): string;
begin
  if AToken<>fScanner.Token then
    raise Exception.Create('Parse error at line '+inttostr(fScanner.Line)+': '+inttostr(fScanner.Position)+': Expected "'+TokenNames[AToken]+'" but found "' + TokenNames[fScanner.Token]+'"');

  result:=fScanner.TokenStr;

  fScanner.Next;
end;

function TBaseParser.Token: TToken;
begin
  result:=fScanner.Token;
end;

constructor TBaseParser.Create(AStream: tstream);
begin
  inherited Create;
  fScanner:=TScanner.Create(AStream);
end;

destructor TBaseParser.Destroy;
begin
  fScanner.Free;
  inherited Destroy;
end;

end.

