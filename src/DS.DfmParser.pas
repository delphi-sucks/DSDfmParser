unit DS.DfmParser;

interface

uses
  System.Classes, System.Generics.Collections;

type
  TDfmObject = class;

  TDfmProperty = class(TPersistent)
  strict private
    FOwner: TDfmObject;
    FName: String;
    FValue: String;
  public
    constructor Create(Owner: TDfmObject; const Name, Value: String);
    procedure Assign(Source: TPersistent); override;
    property Owner: TDfmObject read FOwner;
    property Name: String read FName write FName;
    property Value: String read FValue write FValue;
  end;

  TDfmObject = class(TObject)
  type

    TDfmObjectType = (dotObject, dotInherited, dotInline);

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
      function GetProperty(const Name: String): TDfmProperty;
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
      function GetObject(const Name: String): TDfmObject;
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
    constructor Create(const Owner: TDfmObject; const Name: String);
    destructor Destroy; override;
    function HasProperty(const PropertyName: String): Boolean; overload;
    function HasProperty(const PropertyName, Value: String): Boolean; overload;
    function GetProperty(const PropertyName: String): String;
    function HasObject(const Name: String): Boolean;
    function GetObject(const Name: String; const Recursive: Boolean = False): TDfmObject;
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
    procedure Parse(const DfmContent: String);
  public
    constructor Create; overload;
    procedure Save(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromString(const DfmContent: String);
    function GetDfm: String;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.RegularExpressions;

const
  CR = #13;
  LF = #10;
  CRLF = #13#10;

{ TDfmObject }

constructor TDfmObject.Create(const Owner: TDfmObject; const Name: String);
begin
  inherited Create;
  FOwner := Owner;
  FName := Name;
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

function TDfmObject.GetObject(const Name: String; const Recursive: Boolean): TDfmObject;

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
  if Recursive then
  begin
    Result := Search(Self, Name);
  end else
  begin
    for DfmObject in Objects do
    begin
      if DfmObject.Name.ToLower = Name.ToLower then
      begin
        Result := DfmObject;
        Exit;
      end;
    end;
    Result := nil;
  end;
end;

function TDfmObject.GetProperty(const PropertyName: String): String;
var
  DfmProperty: TDfmProperty;
begin
  DfmProperty := Properties.GetProperty(PropertyName);
  if DfmProperty = nil then
  begin
    Result := EmptyStr;
  end else
  begin
    Result := DfmProperty.Value;
  end;
end;

function TDfmObject.HasObject(const Name: String): Boolean;
var
  DfmObject: TDfmObject;
begin
  for DfmObject in Objects do
  begin
    if DfmObject.Name.ToLower = Name.ToLower then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TDfmObject.HasProperty(const PropertyName, Value: String): Boolean;
var
  objDfmProperty: TDfmProperty;
begin
  Result := Properties.Names.TryGetValue(PropertyName.ToLower, objDfmProperty)
   and (objDfmProperty.Value = Value);
end;

function TDfmObject.HasProperty(const PropertyName: String): Boolean;
begin
  Result := Properties.Names.ContainsKey(PropertyName.ToLower);
end;

{ TDfmFile }

procedure TDfmFile.Parse(const DfmContent: String);
var
  Lines: TStringList;
  LineNumber: Integer;
  LineCount: Integer;
  LineContent: String;
  Value: String;
  RegEx_object: TRegEx;
  RegEx_property: TRegEx;
  Match: TMatch;

  objDfmObject: TDfmObject;

  function GetLine(_bTrim: Boolean = True): String;
  begin
    if _bTrim then
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
  objDfmObject := nil;

  RegEx_object := TRegEx.Create('^([a-z]+) (?:([a-z0-9_]+): )?([a-z0-9_]+)(?: \[([0-9]+)\])?$', [roIgnoreCase]);
  RegEx_property := TRegEx.Create('^([a-z0-9_\.]+) =(?:(?: (.*)$)|$)', [roIgnoreCase]);
  Lines := TStringList.Create;
  try
    Lines.Delimiter := LF;
    Lines.Text := DfmContent;
    LineCount := Lines.Count;
    LineNumber := 0;

    while LineNumber < LineCount do
    begin
      LineContent := GetLine;

      // object
      Match := RegEx_object.Match(LineContent);
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
          objDfmObject.ObjectType := dotObject;
        end else
        if Match.Groups[1].Value.ToLower = 'inherited' then
        begin
          objDfmObject.ObjectType := dotInherited;
        end else
        if Match.Groups[1].Value.ToLower = 'inline' then
        begin
          objDfmObject.ObjectType := dotInline;
        end else
        begin
          //prcLog(Self, 'Unknown DFM Object type "' + objMatch.Groups[1].Value + '"!', lpError);
          Exit;
        end;
        objDfmObject.FClassSaved := objDfmObject.ClassName_;
        Continue;
      end;

      // property
      Match := RegEx_property.Match(LineContent);
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
          end else
          begin
            Value := Value;
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

  procedure RenderWhitespace(const Depth: Integer);
  begin
    DFM := DFM + EmptyStr.PadLeft(Depth * 2);
  end;

  procedure RenderProperty(const DfmProperty: TDfmProperty; const Depth: Integer);
  begin
    RenderWhitespace(Depth);
    DFM := DFM + DfmProperty.Name + ' = ' + DfmProperty.Value + CRLF;
  end;

  procedure RenderObject(const _objDfmObject: TDfmObject; const _iDepth: Integer);
  var
    objDfmProperty:     TDfmProperty;
    objDfmObjectChild:  TDfmObject;
  begin
    RenderWhitespace(_iDepth);
    case _objDfmObject.ObjectType of
      dotObject:
        DFM := DFM + 'object ';
      dotInherited:
        DFM := DFM + 'inherited ';
      dotInline:
        DFM := DFM + 'inline ';
    end;
    if _objDfmObject.Name <> EmptyStr then
    begin
      DFM := DFM + _objDfmObject.Name + ': ';
    end;
    DFM := DFM + _objDfmObject.ClassName_;
    if _objDfmObject.Id >= 0 then
    begin
      DFM := DFM + ' [' + _objDfmObject.Id.ToString + ']';
    end;
    DFM := DFM + CRLF;
    for objDfmProperty in _objDfmObject.Properties do
    begin
      RenderProperty(objDfmProperty, _iDepth + 1);
    end;
    for objDfmObjectChild in _objDfmObject.Objects do
    begin
      RenderObject(objDfmObjectChild, _iDepth + 1);
    end;
    RenderWhitespace(_iDepth);
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

procedure TDfmFile.Save(const FileName: String);
var
  lstFile:  TStringList;
begin
  lstFile := TStringList.Create;
  try
    lstFile.Text := GetDfm;
    lstFile.SaveToFile(FileName);
  finally
    FreeAndNil(lstFile);
  end;
end;

procedure TDfmFile.LoadFromFile(const FileName: String);
begin
  Parse(TFile.ReadAllText(FileName));
end;

procedure TDfmFile.LoadFromString(const DfmContent: String);
begin
  Parse(DfmContent);
end;

{ TDfmProperty }

procedure TDfmProperty.Assign(Source: TPersistent);
var
  DfmProperty: TDfmProperty;
begin
  if Source is TDfmProperty then
  begin
    DfmProperty := (Source as TDfmProperty);
    FOwner := DfmProperty.Owner;
    FName := DfmProperty.Name;
    FValue := DfmProperty.Value;
  end else
  begin
    inherited;
  end;
end;

constructor TDfmProperty.Create(Owner: TDfmObject; const Name, Value: String);
begin
  inherited Create;
  FOwner := Owner;
  FName := Name;
  FValue := Value;
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

function TDfmObject.TDfmObjectList.GetObject(const Name: String): TDfmObject;
begin
  if not FNames.TryGetValue(Name.ToLower, Result) then
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

function TDfmObject.TDfmPropertyList.GetProperty(const Name: String): TDfmProperty;
begin
  if not Names.TryGetValue(Name.ToLower, Result) then
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
