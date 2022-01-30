unit UConvert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, UCommon;

//-----------------------------------------------------------------------------
const cPresc = 6;   //Precisión de los valores numéricos.

//-----------------------------------------------------------------------------
type
  TFormConvert = class(TForm)
    EditBytes: TEdit;
    Label1: TLabel;
    EditKBytes: TEdit;
    Label2: TLabel;
    EditMBytes: TEdit;
    Label3: TLabel;
    EditGBytes: TEdit;
    Label4: TLabel;
    EditTBytes: TEdit;
    Label5: TLabel;
    ActionList1: TActionList;
    ActionClose: TAction;
    procedure ActionCloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditBytesChange(Sender: TObject);
    procedure EditKBytesChange(Sender: TObject);
    procedure EditMBytesChange(Sender: TObject);
    procedure EditGBytesChange(Sender: TObject);
    procedure EditTBytesChange(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    ValueByte: Double;
  public
    procedure ShowValues;
  end;

var
  FormConvert: TFormConvert;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------
// Inicia el formulario y sus datos.
//-----------------------------------------------------------------------------
procedure TFormConvert.FormCreate(Sender: TObject);
begin
ValueByte := 0;
ShowValues;
end;

//-----------------------------------------------------------------------------
// Cierra el formulario.
//-----------------------------------------------------------------------------
procedure TFormConvert.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Muestra los valores en los Edits.
//-----------------------------------------------------------------------------
procedure TFormConvert.ShowValues;
begin
if not EditBytes.Focused then
   EditBytes.Text := FloatToStrF(ValueByte, ffFixed, 16, cPresc);

if not EditKBytes.Focused then
   EditKBytes.Text := FloatToStrF(ValueByte / cKByte, ffFixed, 16, cPresc);

if not EditMBytes.Focused then
   EditMBytes.Text := FloatToStrF(ValueByte / cMByte, ffFixed, 16, cPresc);

if not EditGBytes.Focused then
   EditGBytes.Text := FloatToStrF(ValueByte / cGByte, ffFixed, 16, cPresc);

if not EditTBytes.Focused then
   EditTBytes.Text := FloatToStrF(ValueByte / cTByte, ffFixed, 16, cPresc);

EditBytes.Color := clWindow;
EditKBytes.Color := clWindow;
EditMBytes.Color := clWindow;
EditGBytes.Color := clWindow;
EditTBytes.Color := clWindow;
end;

//-----------------------------------------------------------------------------
// Convierte los datos introducidos.
//-----------------------------------------------------------------------------

procedure TFormConvert.EditBytesChange(Sender: TObject);
begin
if TEdit(Sender).Focused then
   if TEdit(Sender).Text <> '' then
      try
         ValueByte := StrToFloat(TEdit(Sender).Text);
         ShowValues;
      except
         TEdit(Sender).Color := clRed;
      end;
end;

procedure TFormConvert.EditKBytesChange(Sender: TObject);
begin
if TEdit(Sender).Focused then
   if TEdit(Sender).Text <> '' then
      try
         ValueByte := StrToFloat(TEdit(Sender).Text) * cKByte;
         ShowValues;
      except
         TEdit(Sender).Color := clRed;
      end;
end;

procedure TFormConvert.EditMBytesChange(Sender: TObject);
begin
if TEdit(Sender).Focused then
   if TEdit(Sender).Text <> '' then
      try
         ValueByte := StrToFloat(TEdit(Sender).Text) * cMByte;
         ShowValues;
      except
         TEdit(Sender).Color := clRed;
      end;
end;

procedure TFormConvert.EditGBytesChange(Sender: TObject);
begin
if TEdit(Sender).Focused then
   if TEdit(Sender).Text <> '' then
      try
         ValueByte := StrToFloat(TEdit(Sender).Text) * cGByte;
         ShowValues;
      except
         TEdit(Sender).Color := clRed;
      end;
end;

procedure TFormConvert.EditTBytesChange(Sender: TObject);
begin
if TEdit(Sender).Focused then
   if TEdit(Sender).Text <> '' then
      try
         ValueByte := StrToFloat(TEdit(Sender).Text) * cTByte;
         ShowValues;
      except
         TEdit(Sender).Color := clRed;
      end;
end;

//-----------------------------------------------------------------------------
// Limita los caracteres que se pueden introducir.
//-----------------------------------------------------------------------------
procedure TFormConvert.EditKeyPress(Sender: TObject; var Key: Char);
begin
if not (Key in ['0'..'9', #8, ',']) then Key := #0;
end;

procedure TFormConvert.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
