# Delphi DFM Parser

Parse and save Delphi DFM files by modifying the objects and their properties.

## Instalation

Just include the file `DS.DfmParser.pas` from the **src**-Directory.

```delphi
uses DS.DfmParser;
```

## Usage

```delphi
var
  DfmFile: TDfmFile;
  PanelObject: TDfmObject;
  PanelCaption: String;
  DfmText: String;
begin
  DfmFile := TDfmFile.Create;

  // Load file
  DfmFile.LoadFromFile('MyDfmFile.dfm');

  // Get object (visual element)
  DfmObject := DfmFile.GetObject('Panel1');

  Writeln(DfmObject.Name); // Panel1
  Writeln(DfmObject.ClassName_); // TPanel

  // Get properties
  PanelCaption := PanelObject.GetProperty('Caption');
  Writeln(PanelCaption); // 'Panel1'

  // Set property
  PanelObject.SetProperty('Caption', '''MyPanel''')

  // Delete property
  PanelObject.DeleteProperty('Caption');

  // Save file
  DfmFile.Save('MyModifiedDfmFile.dfm');

  // or get the context as string
  DfmText := DfmFile.GetDfm;
  Writeln(DfmText); // object Form1: TForm1...

  DfmFile.Free;
end;
```

## Notes

### Property values

The parser only parses the dfm but not it's content.
Strings for example are formatted like how one would write it in delphi (`'Caption1'`).

Values can also be multiline. Delphi generally indents the value lines, but it shouldn't make a difference if they aren't. After saving the Form in Delphi it should automatically fix the indention.

## Example

An example project can be found in the `example`-Directory.
