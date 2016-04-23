unit libparsetest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TLibTest= class(TTestCase)
  published
    procedure TestParser;
  end;

implementation

uses
  libparser;

procedure TLibTest.TestParser;
var
  fs: TFileStream;
  p: TParser;
  e: TEntry;
begin
  fs:=TFileStream.Create('../osu035/osu035_stdcells.lib', fmOpenRead);
  try
    p:=TParser.Create(fs);
    try
      e:=p.ENTRY;
      e.Free;
    finally
      p.Free;
    end;
  finally
    fs.Free;
  end;
end;

initialization
  RegisterTest(TLibTest);

end.

