unit DS.DfmParser.Test;

interface

uses
  DUnitX.TestFramework, DS.DfmParser;

type
  [TestFixture]
  DfmObjectTest = class
  private
    DfmFile: TDfmFile;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure LoadAndSaveWithoutChanging;

    [Test]
    [TestCase('Non existend property', 'NonExistend,,False')]
    [TestCase('By name', 'Caption,,True')]
    [TestCase('By name case insensitive', 'cApTIoN,,True')]
    [TestCase('By name and value', 'Caption,''Form1'',True')]
    [TestCase('By name case insensitive and value', 'CAPTION,''Form1'',True')]
    [TestCase('By name and value case sensitive', 'Caption,''form1'',False')]
    [TestCase('By name and wrong value', 'Caption,''Form2'',False')]
    [TestCase('Empty string', ',,False')]
    [TestCase('Empty string with value', ',''Form1'',False')]
    procedure HasProperty(const AName, AValue: String; AExpected: Boolean);

    [Test]
    [TestCase('Property exists', 'Caption,''Form1''')]
    [TestCase('Property exists case insensitive', 'CAPTION,''Form1''')]
    [TestCase('Property doesn''t exist', 'Captiooooon,')]
    [TestCase('Empty string', ',')]
    procedure GetProperty(const AName, AExpected: String);

    [Test]
    [TestCase('Set non existend property', 'NewCaption,''MyForm''')]
    [TestCase('Overwrite property', 'Caption,''MyForm''')]
    [TestCase('Overwrite property case insensitive', 'CAPTION,''MyForm''')]
    procedure SetProperty(const AName, AValue: String);

    [Test]
    [TestCase('Property exists', 'Caption')]
    [TestCase('Property exists case insensitive', 'CAPTION')]
    [TestCase('Property doesn''t exist', 'Captain')]
    [TestCase('Empty string', '')]
    procedure DeleteProperty(const AName: String);

    [Test]
    [TestCase('Object exists', 'Panel1,True')]
    [TestCase('Object exists case insensitive', 'pANeL1,True')]
    [TestCase('Object doesn''t exist', 'Panel99,False')]
    [TestCase('Empty string', ',False')]
    procedure HasObject(const AName: String; AExpected: Boolean);

    [Test]
    // Non recursive
    [TestCase('Object exists', 'Panel1,False,True')]
    [TestCase('Object exists case insensitive', 'pANeL1,False,True')]
    [TestCase('Object doesn''t exist', 'Panel99,False,False')]
    [TestCase('Empty string', ',False,False')]
    // Recursive
    [TestCase('Object exists within child', 'CheckBox1,True,True')]
    [TestCase('Object exists within child case insensitive', 'CHECKBOX1,True,True')]
    [TestCase('Object doesn''t exist resursive', 'Panel99,True,False')]
    [TestCase('Empty string recursive', ',True,False')]
    procedure GetObject(const AName: String; ARecursive: Boolean; AExpected: Boolean);

    [Test]
    [TestCase('Object exists', 'Panel1')]
    [TestCase('Object exists case insensitive', 'PANEL1')]
    [TestCase('Object doesn''t exist', 'Panel99')]
    [TestCase('Empty string', '')]
    procedure DeleteObject(const AName: String);
  end;

implementation

uses
  System.IOUtils;

const TEST_FILE = '../example/VCL-Example-Form/Main.dfm';

procedure DfmObjectTest.SetProperty(const AName, AValue: String);
begin
  DfmFile.SetProperty(AName, AValue);
  Assert.AreEqual(DfmFile.GetProperty(AName), AValue);
end;

procedure DfmObjectTest.Setup;
begin
  DfmFile := TDfmFile.Create;
  DfmFile.LoadFromFile(TEST_FILE)
end;

procedure DfmObjectTest.TearDown;
begin
  DfmFile.Free;
end;

procedure DfmObjectTest.DeleteObject(const AName: String);
begin
  DfmFile.DeleteObject(AName);
  Assert.IsFalse(DfmFile.HasObject(AName));
end;

procedure DfmObjectTest.DeleteProperty(const AName: String);
begin
  DfmFile.DeleteProperty(AName);
  Assert.IsFalse(DfmFile.HasProperty(AName));
end;

procedure DfmObjectTest.GetObject(const AName: String; ARecursive,
  AExpected: Boolean);
begin
  if AExpected then
    Assert.IsNotNull(DfmFile.GetObject(AName, ARecursive))
  else
    Assert.IsNull(DfmFile.GetObject(AName, ARecursive));
end;

procedure DfmObjectTest.GetProperty(const AName, AExpected: String);
begin
  Assert.AreEqual(AExpected, DfmFile.GetProperty(AName));
end;

procedure DfmObjectTest.HasObject(const AName: String; AExpected: Boolean);
begin
  Assert.AreEqual(AExpected, DfmFile.HasObject(AName));
end;

procedure DfmObjectTest.HasProperty(const AName, AValue: String;
  AExpected: Boolean);
begin
  if AValue = '' then
    Assert.AreEqual(AExpected, DfmFile.HasProperty(AName))
  else
    Assert.AreEqual(AExpected, DfmFile.HasProperty(AName, AValue));
end;

procedure DfmObjectTest.LoadAndSaveWithoutChanging;
var
  DfmText: String;
begin
  DfmText := TFile.ReadAllText(TEST_FILE);
  DfmFile.LoadFromString(DfmText);
  Assert.AreEqual(DfmText, DfmFile.GetDfm);
end;

initialization
  TDUnitX.RegisterTestFixture(DfmObjectTest);

end.
