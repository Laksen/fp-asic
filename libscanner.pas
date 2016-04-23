unit libscanner;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
  TToken = (tkEof,
    tkIdentifier,
    tkPipe, tkLSquare, tkRSquare, tkLBracket, tkRBracket, tkDot, tkComma,
    tkLParan, tkRParan, tkColon, tkSemiColon,
    tkString, tkNumber);

  TTokenSet = set of TToken;

  EScannerException = class(Exception);

  TScanner = class
  private
    ch: char;
    fStream: TStream;
    fToken: TToken;
    fTokenStr: string;
    fLine, fPos: longint;

    fLookBack: char;
    lookback: boolean;

    function EOF: boolean;
    function GetChar: char;
    function FindMatch: longint;
  public
    procedure Next;

    constructor Create(AStream: TStream);

    property Token: TToken read fToken;
    property TokenStr: string read fTokenStr;

    property Line: longint read fLine;
    property Position: longint read fPos;
  end;

const
  TokenStrs: array[TToken] of string =
    ('',
    '', '|', '[', ']', '{', '}', '.', ',',
    '(', ')', ':', ';',
    '','');
  TokenNames: array[TToken] of string =
    ('',
    'Identifier', '|', '[', ']', '{', '}', '.', ',',
    '(', ')', ':', ';',
    'String','Number');

implementation

const
  BadChars = [' ', #0..#31];
  Letter   = ['a'..'z', 'A'..'Z'];
  Digit    = ['0'..'9'];
  HexDigit = ['A'..'F'];

function TScanner.EOF: boolean;
  begin
    Result := fStream.Position = fStream.Size;
  end;

function TScanner.GetChar: char;
  begin
    Result := #0;

    if lookback then
      begin
        Result := fLookback;
        lookback := False;
        exit;
      end;

    fStream.Read(Result, 1);
    Inc(fPos);

    Result := (Result);
  end;

function TScanner.FindMatch: longint;
  var
    tk: TToken;
  begin
    Result := 0;
    for tk := low(TToken) to high(TToken) do
      begin
        if pos(fTokenStr, TokenStrs[tk]) = 1 then
          begin
            fToken := tk;
            Inc(Result);
          end;
      end;
  end;

procedure TScanner.Next;
  var
    tk: TToken;
    hex: boolean;
    old: string;
    sl, sp, commentlevel: longint;

  procedure Trim;
    begin
      if ch in BadChars then
        begin
          while (ch in BadChars) and (not EOF) do
            begin
              if ch = #10 then
                begin
                  fpos := 0;
                  Inc(fline);
                end;
              ch := GetChar;
            end;
          if EOF then
            begin
              fToken := tkEof;
              exit;
            end;
        end;
    end;

  begin
    if EOF and (ch = #0) then
      begin
        fToken := tkEof;
        exit;
      end;

    Trim;

    commentlevel := 0;

    while (ch = '/') or (ch = '\') do
      begin
        if ch  = '\' then
          begin
            ch:=GetChar;
            trim;
          end
        else if ch = '/' then
          begin
            sp := Position;
            sl := Line;

            fToken := tkEof;
            fTokenStr := '/';

            ch := GetChar;
            if ch = '*' then
              begin
                Inc(commentlevel);
                while True do
                  begin
                    ch := GetChar;
                    trim;

                    if EOF and (ch = #0) then
                      raise EScannerException.CreateFmt('Unterminated comment found at line %d, position %d', [sl, sp]);

                    if ch = '/' then
                      begin
                        ch := GetChar;
                        if ch = '*' then
                          begin
                            Inc(commentlevel);
                          end;
                      end
                    else if ch = '*' then
                      begin
                        ch := GetChar;
                        if ch = '/' then
                          begin
                            Dec(commentlevel);
                            if commentlevel = 0 then
                              begin
                                ch := GetChar;
                                break;
                              end;
                          end;
                      end;
                  end;
              end
            else
              exit;
          end;
      end;

    Trim;

    if ch in Letter then
      begin
        fToken := tkIdentifier;
        fTokenStr := '';

        while ch in (letter + digit + ['_']) do
          begin
            fTokenStr := fTokenStr + ch;
            ch := GetChar;
          end;

        for tk := low(TToken) to high(TToken) do
          begin
            if fTokenStr = TokenStrs[tk] then
              begin
                fToken := tk;
                break;
              end;
          end;
      end
    else if ch in Digit then
      begin
        fTokenStr := ch;

        ch := GetChar;
        while ch in digit do
          begin
            fTokenStr := fTokenStr + ch;
            ch := GetChar;
          end;

        fToken := tkNumber;

        if ch = '.' then
          begin
            ch := GetChar;
            if ch = '.' then
              begin
                lookback := True;
                fLookBack := '.';
                exit;
              end
            else
              fTokenStr := fTokenStr + '.';

            while ch in Digit do
              begin
                fTokenStr := fTokenStr + ch;
                ch := GetChar;
              end;
            fTokenStr := fTokenStr + '0';

            if ch in ['E', 'D'] then
              begin
                fTokenStr := fTokenStr + 'E';
                ch := GetChar;

                if ch in ['+', '-'] then
                  begin
                    fTokenStr := fTokenStr + ch;
                    ch := GetChar;
                  end
                else
                  fTokenStr := fTokenStr + '+';

                while ch in Digit do
                  begin
                    fTokenStr := fTokenStr + ch;
                    ch := GetChar;
                  end;
              end;
          end;
      end
    else if ch = '"' then
      begin
        fToken := tkString;
        fTokenStr := '';

        ch := GetChar;
        while ch <> '"' do
          begin
            if EOF then
              raise Exception.Create('Unterminated string');
            fTokenStr := fTokenStr + ch;
            ch := GetChar;
          end;
        ch := GetChar;
      end
    else
      begin
        fTokenStr := ch;

        while True do
          begin
            if (FindMatch = 1) and
               (TokenStrs[fToken] = fTokenStr) then
              break;

            if EOF then
              begin
                fToken := tkEof;
                for tk := low(TToken) to high(TToken) do
                  begin
                    if fTokenStr = TokenStrs[tk] then
                      begin
                        fToken := tk;
                        break;
                      end;
                  end;
                break;
              end;
            ch := getchar;
            if ch in badchars then
              begin
                for tk := low(TToken) to high(TToken) do
                  begin
                    if fTokenStr = TokenStrs[tk] then
                      begin
                        fToken := tk;
                        break;
                      end;
                  end;
                break;
              end;
            old := fTokenStr;
            fTokenStr := fTokenStr + ch;
            if FindMatch = 0 then
              begin
                fTokenStr := old;
                for tk := low(TToken) to high(TToken) do
                  begin
                    if fTokenStr = TokenStrs[tk] then
                      begin
                        fToken := tk;
                        exit;
                      end;
                  end;
                raise Exception.CreateFmt('Expected token but got "%s"', [fTokenStr]);
              end;
          end;
        ch := getchar;
      end;
  end;

constructor TScanner.Create(AStream: TStream);
  begin
    inherited Create;
    lookback := False;
    fLine := 1;
    fPos := 1;
    fStream := AStream;
    Next;
  end;

end.
