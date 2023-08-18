program DfmParserExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DS.DfmParser in '..\src\DS.DfmParser.pas';

procedure WriteTreeLine(const AText: String; ADepth: Integer);
var
  Prefix: String;
  DepthIndex: Integer;
begin
  if ADepth > 0 then
  begin
    Prefix := '├─ ';
    for DepthIndex := 2 to ADepth do
      Prefix := '│  ' + Prefix;
  end else
    Prefix := '';
  Writeln(Prefix + AText);
end;

procedure WriteProperties(ADfmObject: TDfmObject; ADepth: Integer);
var
  DfmProperty: TDfmProperty;
begin
  for DfmProperty in ADfmObject.Properties do
    WriteTreeLine(DfmProperty.Name + ' = ' + DfmProperty.Value, ADepth);
end;

procedure IterateDfmObjects(ADfmObject: TDfmObject; ADepth: Integer = 0);
var
  ChildObject: TDfmObject;
begin
  WriteTreeLine(ADfmObject.Name + ' (' + ADfmObject.ClassName_ + ')', ADepth);
  for ChildObject in ADfmObject.Objects do
    IterateDfmObjects(ChildObject, ADepth + 1);
  WriteProperties(ADfmObject, ADepth + 1);
end;

var
  DfmFile: TDfmFile;
  ABuffer: String;
begin
  try
    DfmFile := TDfmFile.Create;
    try
      // Load file
      DfmFile.LoadFromFile('VCL-Example-Form/Main.dfm');

      // Print object tree
      IterateDfmObjects(DfmFile);
    finally
      DfmFile.Free;
    end;

    Writeln('');
    Writeln('Press enter to exit');
    Readln(ABuffer);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
