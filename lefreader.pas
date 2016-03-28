unit lefreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLEFReader = class
  public
    procedure LoadFromStream(AStream: TStream);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure TLEFReader.LoadFromStream(AStream: TStream);
var
  st: TStringList;
  i: longint;
  s, name: string;

  procedure Next;
  begin
    repeat
      s:=trim(st[i]);
      inc(i);
    until pos('#', s)<>1;
  end;

begin
  st:=TStringList.Create;
  try
    st.LoadFromStream(AStream);

    i:=0;
    while i<st.count do
    begin
      if pos('#', s)=1 then continue;

      if pos('LAYER ', s)=1 then
      begin
        delete(s,1,6);
        name:=trim(s);

        writeln('Layer: ', name);

        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('VIA ', s)=1 then
      begin
        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('VIARULE ', s)=1 then
      begin
        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('SITE ', s)=1 then
      begin
        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('MACRO ', s)=1 then
      begin
        while pos('END ', s)<>1 do
        begin
          if pos('PORT ', s)=1 then
          begin
            while pos('END ', s)<>1 do
              Next;
            Next;
          end
          else if pos('PIN ', s)=1 then
          begin
            if pos('PORT ', s)=1 then
            begin
              while pos('END ', s)<>1 do
                Next;
              Next;
            end;

            while pos('END ', s)<>1 do
              Next;
            Next;
          end
          else if pos('OBS ', s)=1 then
          begin
            while pos('END ', s)<>1 do
              Next;
            Next;
          end
          else
            Next;
        end;
        Next;
      end
      else
        Next;
    end;
  finally
    st.Free;
  end;
end;

constructor TLEFReader.Create;
begin
  inherited Create;
end;

destructor TLEFReader.Destroy;
begin
  inherited Destroy;
end;

end.

