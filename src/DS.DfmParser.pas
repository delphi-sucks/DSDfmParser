unit DS.DfmParser;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  EDfmParseInvalidFormat = class(Exception);

  TDfmObjectType = (doObject, doInherited, doInline);

  TDfmProperty = class
  private
    FName: string;
    FValue: string;
  public
    constructor Create(const AName, AValue: string);
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  TDfmObject = class
  private
    FOwner: TDfmObject;
    FName: string;
    FClassName: string;
    FId: Integer;
    FBeginLine: Integer;
    FEndLine: Integer;
    FLevel: Integer;
    FProperties: TObjectList<TDfmProperty>;
    FObjects: TObjectList<TDfmObject>;
    FObjectType: TDfmObjectType;
    FClassSaved: string;
    FPropertiesSaved: TObjectList<TDfmProperty>;
    FObjectsSaved: TObjectList<TDfmObject>;
    function GetPropertyObject(const AName: string): TDfmProperty;
  public
    constructor Create(const AOwner: TDfmObject; const AName: string);
    destructor Destroy; override;

    function HasProperty(const AName: string): Boolean; overload;
    function HasProperty(const AName, AValue: string): Boolean; overload;
    function GetProperty(const AName: string): string;
    procedure SetProperty(const AName, AValue: string);
    procedure DeleteProperty(const AName: string);

    function HasObject(const AName: string): Boolean;
    function GetObject(const AName: string; ARecursive: Boolean = False): TDfmObject;
    procedure DeleteObject(const AName: string);

    property Owner: TDfmObject read FOwner;
    property Name: string read FName write FName;
    property ClassName_: string read FClassName write FClassName;
    property BeginLine: Integer read FBeginLine;
    property EndLine: Integer read FEndLine;
    property Level: Integer read FLevel;
    property Id: Integer read FId write FId;
    property ObjectType: TDfmObjectType read FObjectType write FObjectType;
    property Properties: TObjectList<TDfmProperty> read FProperties;
    property Objects: TObjectList<TDfmObject> read FObjects;
  end;

  TDfmFile = class(TDfmObject)
  private
    procedure Parse(const ADfmContent: string);
    function RenderDfm(DfmObject: TDfmObject): string;
  public
    constructor Create; overload;
    procedure Save(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromString(const ADfmContent: string);
    function GetDfm: string;
    function GetDfmPart(DfmObject: TDfmObject): string; overload;
    function GetDfmPart(StrDfmObject: string): string; overload;
  end;

implementation

uses
  System.IOUtils, System.RegularExpressions;

const
  CR = #13;
  LF = #10;
  CRLF = #13#10;

{ TDfmObject }

constructor TDfmObject.Create(const AOwner: TDfmObject; const AName: string);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FId := -1;
  if FOwner <> nil then
    FOwner.Objects.Add(Self);
  FProperties := TObjectList<TDfmProperty>.Create(True);
  FObjects := TObjectList<TDfmObject>.Create(True);
  FPropertiesSaved := TObjectList<TDfmProperty>.Create;
  FObjectsSaved := TObjectList<TDfmObject>.Create;
end;

procedure TDfmObject.DeleteObject(const AName: String);
var
  ObjectIndex: Integer;
begin
  for ObjectIndex := 0 to Objects.Count-1 do
    if SameText(Objects[ObjectIndex].Name, AName) then
    begin
      Objects.Delete(ObjectIndex);
      Break;
    end;
end;

procedure TDfmObject.DeleteProperty(const AName: string);
var
  PropertyIndex: Integer;
begin
  for PropertyIndex := 0 to Properties.Count - 1 do
    if SameText(Properties[PropertyIndex].Name, AName) then
    begin
      Properties.Delete(PropertyIndex);
      Break;
    end;
end;

destructor TDfmObject.Destroy;
begin
  FObjects.Free;
  FProperties.Free;
  FPropertiesSaved.Free;
  FObjectsSaved.Free;
  inherited;
end;

function TDfmObject.GetObject(const AName: string; ARecursive: Boolean): TDfmObject;

  function Search(const DfmObjectParent: TDfmObject; const Name: string): TDfmObject;
  var
    DfmChildObject: TDfmObject;
    DfmObject: TDfmObject;
  begin
    if SameText(DfmObjectParent.Name, Name) then
      Exit(DfmObjectParent);
    for DfmChildObject in DfmObjectParent.Objects do
    begin
      DfmObject := Search(DfmChildObject, Name);
      if DfmObject <> nil then
        Exit(DfmObject);
    end;
    Result := nil;
  end;

var
  DfmObject: TDfmObject;
begin
  if ARecursive then
    Result := Search(Self, AName)
  else
  begin
    for DfmObject in Objects do
      if SameText(DfmObject.Name, AName) then
        Exit(DfmObject);
    Result := nil;
  end;
end;

function TDfmObject.GetPropertyObject(const AName: string): TDfmProperty;
var
  DfmProperty: TDfmProperty;
begin
  for DfmProperty in Properties do
    if SameText(DfmProperty.Name, AName) then
      Exit(DfmProperty);
  Result := nil;
end;

function TDfmObject.GetProperty(const AName: string): string;
var
  DfmProperty: TDfmProperty;
begin
  DfmProperty := GetPropertyObject(AName);
  if DfmProperty = nil then
    Result := ''
  else
    Result := DfmProperty.Value;
end;

function TDfmObject.HasObject(const AName: string): Boolean;
begin
  Result := GetObject(AName) <> nil;
end;

function TDfmObject.HasProperty(const AName, AValue: string): Boolean;
var
  DfmProperty: TDfmProperty;
begin
  DfmProperty := GetPropertyObject(AName);
  Result := (DfmProperty <> nil) and (DfmProperty.Value = AValue);
end;

procedure TDfmObject.SetProperty(const AName, AValue: string);
var
  DfmProperty: TDfmProperty;
begin
  DfmProperty := GetPropertyObject(AName);
  if DfmProperty <> nil then
    DfmProperty.Value := AValue
  else
    Properties.Add(TDfmProperty.Create(AName, AValue));
end;

function TDfmObject.HasProperty(const AName: string): Boolean;
begin
  Result := GetPropertyObject(AName) <> nil;
end;

{ TDfmFile }

procedure TDfmFile.Parse(const ADfmContent: string);
var
  Lines: TStringList;
  LineNumber: Integer;
  LineCount: Integer;
  LineContent: string;
  Value: string;
  RegExObject: TRegEx;
  RegExProperty: TRegEx;
  Match: TMatch;

  DfmObject: TDfmObject;

  function GetLine(ATrim: Boolean = True): string;
  begin
    if ATrim then
      Result := Lines[LineNumber].Trim
    else
      Result := Lines[LineNumber].TrimRight([#13]);
    Inc(LineNumber);
  end;

var
  Depth: Integer;
  Name: string;
  ClassName: string;
  Id: Integer;
  Level: Integer;
begin
  if not ADfmContent.StartsWith('object', True)
  and not ADfmContent.StartsWith('inherited', True)
  and not ADfmContent.StartsWith('inline', True) then
    raise EDfmParseInvalidFormat.Create('Invalid dfm file!');

  Self.FName := EmptyStr;
  Self.FClassName := EmptyStr;
  Self.FId := 0;
  Self.FLevel := -1;
  Properties.Clear;
  Objects.Clear;
  DfmObject := nil;

  RegExObject := TRegEx.Create('^(\w+) (?:([\w\däöü_]+): )?([\w\d_]+)(?: \[(\d+)\])?$', [roIgnoreCase]);
  RegExProperty := TRegEx.Create('^([\w\d_\.]+) =(?:(?: (.*)$)|$)', [roIgnoreCase]);
  Lines := TStringList.Create;
  try
    Lines.Delimiter := LF;
    Lines.Text := ADfmContent;
    LineCount := Lines.Count;
    LineNumber := 0;
    Level := 0;

    while LineNumber < LineCount do
    begin
      LineContent := GetLine;

      // object
      Match := RegExObject.Match(LineContent);
      if Match.Success then
      begin
        Name := EmptyStr;
        ClassName := EmptyStr;
        Id := -1;
        if Match.Groups.Count = 5 then
        begin
          Name := Match.Groups[2].Value;
          ClassName := Match.Groups[3].Value;
          Id := Match.Groups[4].Value.ToInteger;
        end
        else if Match.Groups.Count = 4 then
        begin
          Name := Match.Groups[2].Value;
          ClassName := Match.Groups[3].Value;
        end
        else if Match.Groups.Count = 3 then
        begin
          Name := EmptyStr;
          ClassName := Match.Groups[2].Value;
        end;

        if DfmObject = nil then
        begin
          DfmObject := Self;
          DfmObject.FName := Name;
        end else
          DfmObject := TDfmObject.Create(DfmObject, Name);

        DfmObject.FClassName := ClassName;
        DfmObject.FId := Id;
        DfmObject.FBeginLine := LineNumber-1;
        DfmObject.FLevel := Level;

        Inc(Level);

        if Match.Groups[1].Value.ToLower = 'object' then
          DfmObject.ObjectType := doObject
        else if Match.Groups[1].Value.ToLower = 'inherited' then
          DfmObject.ObjectType := doInherited
        else if Match.Groups[1].Value.ToLower = 'inline' then
          DfmObject.ObjectType := doInline
        else
          Exit;

        DfmObject.FClassSaved := DfmObject.FClassName;
        Continue;
      end;

      // property
      Match := RegExProperty.Match(LineContent);
      if Match.Success then
      begin
        if DfmObject <> nil then
        begin
          if (Match.Groups.Count > 2)
          and Match.Groups[2].Success then
            Value := Match.Groups[2].Value
          else
            Value := EmptyStr;

          if Value = '(' then
          begin
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
            until Value.Substring(Value.Length - 3, 1) = ')';
            Value := Value.TrimRight;
          end
          else if Value = '{' then
          begin
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
            until Value.Substring(Value.Length - 3, 1) = '}';
            Value := Value.TrimRight;
          end
          else if Value = '<' then
          begin
            Depth := 1;
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
              if Value.Substring(Value.Length - 4, 2) <> '<>' then
              begin
                if Value.Substring(Value.Length - 3, 1) = '<' then
                  Inc(Depth)
                else if Value.Substring(Value.Length - 3, 1) = '>' then
                  Dec(Depth);
              end;
            until Depth = 0;
            Value := Value.TrimRight;
          end
          else if Value = EmptyStr then
          begin
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
            until Value.Substring(Value.Length - 3, 1) <> '+';
            Value := Value.TrimRight;
          end;

          DfmObject.SetProperty(Match.Groups[1].Value, Value);
        end
        else
          Exit;
        Continue;
      end;

      // end
      if LineContent = 'end' then
      begin
        if DfmObject <> nil then
        begin
          DfmObject.FEndLine := LineNumber-1;
          DfmObject := DfmObject.FOwner;
          Dec(Level);
        end;

        Continue;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TDfmFile.RenderDfm(DfmObject: TDfmObject): string;
var
  DFM: string;

  procedure RenderWhitespace(const ADepth: Integer);
  begin
    DFM := DFM + EmptyStr.PadLeft(ADepth * 2);
  end;

  procedure RenderProperty(const ADfmProperty: TDfmProperty; const ADepth: Integer);
  begin
    RenderWhitespace(ADepth);
    DFM := DFM + ADfmProperty.FName + ' = ' + ADfmProperty.FValue + CRLF;
  end;

  procedure RenderObject(const ADfmObject: TDfmObject; const ADepth: Integer);
  var
    objDfmProperty: TDfmProperty;
    objDfmObjectChild: TDfmObject;
  begin
    RenderWhitespace(ADepth);

    case ADfmObject.ObjectType of
      doObject:
        DFM := DFM + 'object ';
      doInherited:
        DFM := DFM + 'inherited ';
      doInline:
        DFM := DFM + 'inline ';
    end;
    if ADfmObject.Name <> EmptyStr then
      DFM := DFM + ADfmObject.Name + ': ';
    DFM := DFM + ADfmObject.ClassName_;
    if ADfmObject.Id >= 0 then
      DFM := DFM + ' [' + ADfmObject.Id.ToString + ']';
    DFM := DFM + CRLF;
    for objDfmProperty in ADfmObject.Properties do
      RenderProperty(objDfmProperty, ADepth + 1);
    for objDfmObjectChild in ADfmObject.Objects do
      RenderObject(objDfmObjectChild, ADepth + 1);
    RenderWhitespace(ADepth);
    DFM := DFM + 'end' + CRLF;
  end;

begin
  RenderObject(DfmObject, 0);
  Result := DFM;
end;

function TDfmFile.GetDfm: string;
begin
  Result := RenderDfm(Self);
end;

function TDfmFile.GetDfmPart(DfmObject: TDfmObject): string;
begin
  Result := RenderDfm(DfmObject);
end;

function TDfmFile.GetDfmPart(StrDfmObject: string): string;
begin
  Result := RenderDfm(Self.GetObject(StrDfmObject, True));
end;

constructor TDfmFile.Create;
begin
  inherited Create(nil, EmptyStr);
end;

procedure TDfmFile.Save(const AFileName: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := GetDfm;
    Lines.SaveToFile(AFileName);
  finally
    Lines.Free;
  end;
end;

procedure TDfmFile.LoadFromFile(const AFileName: string);
begin
  Parse(TFile.ReadAllText(AFileName));
end;

procedure TDfmFile.LoadFromString(const ADfmContent: string);
begin
  Parse(ADfmContent);
end;

{ TDfmProperty }

constructor TDfmProperty.Create(const AName, AValue: string);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
end;

end.
