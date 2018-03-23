unit DS.DfmParser;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  EDfmParseInvalidFormat = class(Exception);

  TDfmObject = class;

  TDfmProperty = class(TPersistent)
  strict private
    FOwner: TDfmObject;
    FName: String;
    FValue: String;
  public
    constructor Create(AOwner: TDfmObject; const AName, AValue: String);
    procedure Assign(Source: TPersistent); override;
    property Owner: TDfmObject read FOwner;
    property Name: String read FName write FName;
    property Value: String read FValue write FValue;
  end;

  TDfmObject = class(TObject)
  type

    TDfmObjectType = (doObject, doInherited, doInline);

    TDfmPropertyList = class(TObjectList<TDfmProperty>)
    type
      TNames = TObjectDictionary<String, TDfmProperty>;
    strict private
      FNames: TNames;
    protected
      procedure DoNotify(Sender: TObject; const Item: TDfmProperty; Action: TCollectionNotification);
    public
      constructor Create;
      destructor Destroy; override;
      function GetProperty(const AName: String): TDfmProperty;
      property Names: TNames read FNames;
    end;

    TDfmObjectList = class(TObjectList<TDfmObject>)
    type
      TNames = TObjectDictionary<String, TDfmObject>;
    strict private
      FNames: TNames;
    protected
      procedure DoNotify(Sender: TObject; const Item: TDfmObject; Action: TCollectionNotification);
    public
      constructor Create;
      destructor Destroy; override;
      function GetObject(const AName: String): TDfmObject;
      property Names: TNames read FNames;
    end;

  strict private
    FOwner: TDfmObject;
    FName: String;
    FClassName: String;
    FId: Integer;
    FProperties: TDfmPropertyList;
    FObjects: TDfmObjectList;
    FObjectType: TDfmObjectType;
  private
    FClassSaved: String;
    FPropertiesSaved: TObjectList<TDfmProperty>;
    FObjectsSaved: TObjectList<TDfmObject>;
  public
    constructor Create(const AOwner: TDfmObject; const AName: String);
    destructor Destroy; override;
    function HasProperty(const APropertyName: String): Boolean; overload;
    function HasProperty(const APropertyName, AValue: String): Boolean; overload;
    function GetProperty(const APropertyName: String): String;
    function HasObject(const AName: String): Boolean;
    function GetObject(const AName: String; const ARecursive: Boolean = False): TDfmObject;
    property Owner: TDfmObject read FOwner;
    property Name: String read FName write FName;
    property ClassName_: String read FClassName write FClassName;
    property Id: Integer read FId write FId;
    property ObjectType: TDfmObjectType read FObjectType write FObjectType;
    property Properties: TDfmPropertyList read FProperties;
    property Objects: TDfmObjectList read FObjects;
  end;

  TDfmFile = class(TDfmObject)
  strict private
    procedure Parse(const ADfmContent: String);
  public
    constructor Create; overload;
    procedure Save(const AFileName: String);
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromString(const ADfmContent: String);
    function GetDfm: String;
  end;

implementation

uses
  System.IOUtils, System.RegularExpressions;

const
  CR = #13;
  LF = #10;
  CRLF = #13#10;

{ TDfmObject }

constructor TDfmObject.Create(const AOwner: TDfmObject; const AName: String);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FId := -1;
  if FOwner <> nil then
  begin
    FOwner.Objects.Add(Self);
  end;
  FProperties := TDfmPropertyList.Create;
  FObjects := TDfmObjectList.Create;
  FPropertiesSaved := TObjectList<TDfmProperty>.Create;
  FObjectsSaved := TObjectList<TDfmObject>.Create;
end;

destructor TDfmObject.Destroy;
begin
  FreeAndNil(FObjects);
  FreeAndNil(FProperties);
  FreeAndNil(FPropertiesSaved);
  FreeAndNil(FObjectsSaved);
  inherited;
end;

function TDfmObject.GetObject(const AName: String; const ARecursive: Boolean): TDfmObject;

  function Search(const DfmObjectParent: TDfmObject; const Name: String): TDfmObject;
  var
    DfmChildObject: TDfmObject;
    DfmObject: TDfmObject;
  begin
    if DfmObjectParent.Name = Name then
    begin
      Result := DfmObjectParent;
      Exit;
    end;
    for DfmChildObject in DfmObjectParent.Objects do
    begin
      DfmObject := Search(DfmChildObject, Name);
      if DfmObject <> nil then
      begin
        Result := DfmObject;
        Exit;
      end;
    end;
    Result := nil;
  end;

var
  DfmObject: TDfmObject;
begin
  if ARecursive then
  begin
    Result := Search(Self, AName);
  end else
  begin
    for DfmObject in Objects do
    begin
      if DfmObject.Name.ToLower = AName.ToLower then
      begin
        Result := DfmObject;
        Exit;
      end;
    end;
    Result := nil;
  end;
end;

function TDfmObject.GetProperty(const APropertyName: String): String;
var
  DfmProperty: TDfmProperty;
begin
  DfmProperty := Properties.GetProperty(APropertyName);
  if DfmProperty = nil then
  begin
    Result := EmptyStr;
  end else
  begin
    Result := DfmProperty.Value;
  end;
end;

function TDfmObject.HasObject(const AName: String): Boolean;
var
  DfmObject: TDfmObject;
begin
  for DfmObject in Objects do
  begin
    if DfmObject.Name.ToLower = AName.ToLower then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TDfmObject.HasProperty(const APropertyName, AValue: String): Boolean;
var
  objDfmProperty: TDfmProperty;
begin
  Result := Properties.Names.TryGetValue(APropertyName.ToLower, objDfmProperty)
   and (objDfmProperty.Value = AValue);
end;

function TDfmObject.HasProperty(const APropertyName: String): Boolean;
begin
  Result := Properties.Names.ContainsKey(APropertyName.ToLower);
end;

{ TDfmFile }

procedure TDfmFile.Parse(const ADfmContent: String);
var
  Lines: TStringList;
  LineNumber: Integer;
  LineCount: Integer;
  LineContent: String;
  Value: String;
  RegExObject: TRegEx;
  RegExProperty: TRegEx;
  Match: TMatch;

  objDfmObject: TDfmObject;

  function GetLine(ATrim: Boolean = True): String;
  begin
    if ATrim then
    begin
      Result := Lines[LineNumber].Trim;
    end else
    begin
      Result := Lines[LineNumber].TrimRight([#13]);
    end;
    Inc(LineNumber);
  end;

var
  Depth: Integer;
  Name: String;
  ClassName: String;
  Id: Integer;
begin
  if not ADfmContent.StartsWith('object') then
  begin
    raise EDfmParseInvalidFormat.Create('Expected "object" at the beginning of the file');
  end;
  Self.Name := EmptyStr;
  Self.ClassName_ := EmptyStr;
  Self.Id := 0;
  Properties.Clear;
  Objects.Clear;
  objDfmObject := nil;

  RegExObject := TRegEx.Create('^(\w+) (?:([\w\däöü_]+): )?([\w\d_]+)(?: \[(\d+)\])?$', [roIgnoreCase]);
  RegExProperty := TRegEx.Create('^([\w\d_\.]+) =(?:(?: (.*)$)|$)', [roIgnoreCase]);
  Lines := TStringList.Create;
  try
    Lines.Delimiter := LF;
    Lines.Text := ADfmContent;
    LineCount := Lines.Count;
    LineNumber := 0;

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
        end else
        if Match.Groups.Count = 4 then
        begin
          Name := Match.Groups[2].Value;
          ClassName := Match.Groups[3].Value;
        end else
        if Match.Groups.Count = 3 then
        begin
          Name := EmptyStr;
          ClassName := Match.Groups[2].Value;
        end;

        if objDfmObject = nil then
        begin
          objDfmObject := Self;
          objDfmObject.Name := Name;
        end else
        begin
          objDfmObject := TDfmObject.Create(objDfmObject, Name);
        end;
        objDfmObject.ClassName_ := ClassName;
        objDfmObject.Id := Id;

        if Match.Groups[1].Value.ToLower = 'object' then
        begin
          objDfmObject.ObjectType := doObject;
        end else
        if Match.Groups[1].Value.ToLower = 'inherited' then
        begin
          objDfmObject.ObjectType := doInherited;
        end else
        if Match.Groups[1].Value.ToLower = 'inline' then
        begin
          objDfmObject.ObjectType := doInline;
        end else
        begin
          //prcLog(Self, 'Unknown DFM Object type "' + objMatch.Groups[1].Value + '"!', lpError);
          Exit;
        end;
        objDfmObject.FClassSaved := objDfmObject.ClassName_;
        Continue;
      end;

      // property
      Match := RegExProperty.Match(LineContent);
      if Match.Success then
      begin
        if objDfmObject <> nil then
        begin
          if (Match.Groups.Count > 2)
          and Match.Groups[2].Success then
          begin
            Value := Match.Groups[2].Value;
          end else
          begin
            Value := EmptyStr;
          end;
          if Value = '(' then
          begin
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
            until Value.Substring(Value.Length - 3, 1) = ')';
            Value := Value.TrimRight;
          end else
          if Value = '{' then
          begin
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
            until Value.Substring(Value.Length - 3, 1) = '}';
            Value := Value.TrimRight;
          end else
          if Value = '<' then
          begin
            Depth := 1;
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
              if Value.Substring(Value.Length - 4, 2) <> '<>' then
              begin
                if Value.Substring(Value.Length - 3, 1) = '<' then
                begin
                  Inc(Depth);
                end else
                if Value.Substring(Value.Length - 3, 1) = '>' then
                begin
                  Dec(Depth);
                end;
              end;
            until Depth = 0;
            Value := Value.TrimRight;
          end else
          if Value = EmptyStr then
          begin
            Value := Value + CRLF;
            repeat
              Value := Value + GetLine(False) + CRLF;
            until Value.Substring(Value.Length - 3, 1) <> '+';
            Value := Value.TrimRight;
          end;

          TDfmProperty.Create(objDfmObject, Match.Groups[1].Value, Value);
        end else
        begin
          //prcLog(Self, 'Can''t assign DFM-Property! No object available!', lpError);
          Exit;
        end;
        Continue;
      end;

      // end
      if LineContent = 'end' then
      begin
        if objDfmObject <> nil then
        begin
          objDfmObject := objDfmObject.Owner;
        end;
        Continue;
      end;
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

function TDfmFile.GetDfm: String;
var
  DFM: String;

  procedure RenderWhitespace(const ADepth: Integer);
  begin
    DFM := DFM + EmptyStr.PadLeft(ADepth * 2);
  end;

  procedure RenderProperty(const ADfmProperty: TDfmProperty; const ADepth: Integer);
  begin
    RenderWhitespace(ADepth);
    DFM := DFM + ADfmProperty.Name + ' = ' + ADfmProperty.Value + CRLF;
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
    begin
      DFM := DFM + ADfmObject.Name + ': ';
    end;
    DFM := DFM + ADfmObject.ClassName_;
    if ADfmObject.Id >= 0 then
    begin
      DFM := DFM + ' [' + ADfmObject.Id.ToString + ']';
    end;
    DFM := DFM + CRLF;
    for objDfmProperty in ADfmObject.Properties do
    begin
      RenderProperty(objDfmProperty, ADepth + 1);
    end;
    for objDfmObjectChild in ADfmObject.Objects do
    begin
      RenderObject(objDfmObjectChild, ADepth + 1);
    end;
    RenderWhitespace(ADepth);
    DFM := DFM + 'end' + CRLF;
  end;

begin
  RenderObject(Self, 0);
  Result := DFM;
end;

constructor TDfmFile.Create;
begin
  inherited Create(nil, EmptyStr);
end;

procedure TDfmFile.Save(const AFileName: String);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := GetDfm;
    Lines.SaveToFile(AFileName);
  finally
    FreeAndNil(Lines);
  end;
end;

procedure TDfmFile.LoadFromFile(const AFileName: String);
begin
  Parse(TFile.ReadAllText(AFileName));
end;

procedure TDfmFile.LoadFromString(const ADfmContent: String);
begin
  Parse(ADfmContent);
end;

{ TDfmProperty }

procedure TDfmProperty.Assign(Source: TPersistent);
var
  DfmProperty: TDfmProperty;
begin
  if Source is TDfmProperty then
  begin
    DfmProperty := Source as TDfmProperty;
    FOwner := DfmProperty.Owner;
    FName := DfmProperty.Name;
    FValue := DfmProperty.Value;
  end else
  begin
    inherited;
  end;
end;

constructor TDfmProperty.Create(AOwner: TDfmObject; const AName, AValue: String);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FValue := AValue;
  FOwner.Properties.Add(Self);
end;

{ TDfmObject.TDfmObjectList }

constructor TDfmObject.TDfmObjectList.Create;
begin
  inherited Create(True);
  FNames := TNames.Create;
  OnNotify := DoNotify;
end;

destructor TDfmObject.TDfmObjectList.Destroy;
begin
  FreeAndNil(FNames);
  inherited;
end;

function TDfmObject.TDfmObjectList.GetObject(const AName: String): TDfmObject;
begin
  if not FNames.TryGetValue(AName.ToLower, Result) then
  begin
    Result := nil;
  end;
end;

procedure TDfmObject.TDfmObjectList.DoNotify(Sender: TObject; const Item: TDfmObject; Action: TCollectionNotification);
begin
  if (FNames <> nil)
  and (Item.Name <> EmptyStr) then
  begin
    case Action of
      cnAdded:
        FNames.AddOrSetValue(Item.Name.ToLower, Item);
      cnRemoved, cnExtracted:
        FNames.Remove(Item.Name.ToLower);
    end;
  end;
end;

{ TDfmObject.TDfmPropertyList }

constructor TDfmObject.TDfmPropertyList.Create;
begin
  inherited Create(True);
  FNames := TNames.Create;
  OnNotify := DoNotify;
end;

destructor TDfmObject.TDfmPropertyList.Destroy;
begin
  FreeAndNil(FNames);
  inherited;
end;

function TDfmObject.TDfmPropertyList.GetProperty(const AName: String): TDfmProperty;
begin
  if not Names.TryGetValue(AName.ToLower, Result) then
  begin
    Result := nil;
  end;
end;

procedure TDfmObject.TDfmPropertyList.DoNotify(Sender: TObject; const Item: TDfmProperty; Action: TCollectionNotification);
begin
  if (Names <> nil)
  and (Item.Name <> EmptyStr) then
  begin
    case Action of
      cnAdded:
        Names.AddOrSetValue(Item.Name.ToLower, Item);
      cnRemoved, cnExtracted:
        Names.Remove(Item.Name.ToLower);
    end;
  end;
end;

end.
