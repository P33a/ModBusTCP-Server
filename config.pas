unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids,
  IniPropStorage, userparams;

type

  { TFConfig }

  TFConfig = class(TForm)
    IniPropStorage: TIniPropStorage;
    PageControl: TPageControl;
    SGPars: TStringGrid;
    TabParams: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SGParsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure SGParsPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure SGParsValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private

  public
    UserPars: TUserPars;
    curParEditRow: integer;
    EditRequiredObject: TObject;

    procedure UpdateParTable;

  end;

var
  FConfig: TFConfig;

implementation

{$R *.lfm}

{ TFConfig }

procedure TFConfig.FormCreate(Sender: TObject);
begin
  UserPars := TUserPars.Create;
  curParEditRow := -1;
  EditRequiredObject := TObject.Create;
end;

procedure TFConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SGPars.SaveToFile('grid.txt');
end;

procedure TFConfig.FormDestroy(Sender: TObject);
begin
  UserPars.Free;
  EditRequiredObject.Free;
end;

procedure TFConfig.FormShow(Sender: TObject);
begin
  if FileExists('grid.txt') then begin
    SGPars.LoadFromFile('grid.txt');
  end;
end;

procedure TFConfig.SGParsGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  curParEditRow := ARow;
end;

procedure TFConfig.SGParsPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  if (curParEditRow = aRow) and (aCol = 1)  then SGPars.Canvas.Brush.Color := clYellow;
end;

procedure TFConfig.SGParsValidateEntry(sender: TObject; aCol, aRow: Integer;
  const OldValue: string; var NewValue: String);
begin
  curParEditRow := -1;
  SGPars.Objects[0, arow] := EditRequiredObject;
end;

procedure TFConfig.UpdateParTable;
var parValue: single;
    i: integer;
    s: string;
begin
  for i := 0 to UserPars.List.Count - 1 do begin
    SGPars.Cells[0, i + 1] := UserPars.List[i].name;
    SGPars.Cells[1, i + 1] := format('%.4g',[UserPars.List[i].value]);

    s := SGPars.Cells[2, i + 1];
    if (s = '') then continue;

    ParValue := StrToFloatDef(s, 0);
    if ParValue = UserPars.List[i].value then continue;

    if i + 1 = curParEditRow then continue;

    if not assigned(SGPars.Objects[0, i + 1]) then continue;

    // OK, update this Par
    UserPars.List[i].value := ParValue;
    SGPars.Objects[0, i + 1] := nil;
  end;
end;

end.

