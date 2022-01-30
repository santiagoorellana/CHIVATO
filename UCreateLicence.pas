
///////////////////////////////////////////////////////////////////////////////
// Nombre: UCreateLicence
// Creado: 1/12/2016
// Objetivo: Implementa el formulario principal que genera las
//           licencias para los programas del paquete CHIVATO.
// Autor: Santiago Alejandro Orellana Pérez
// Notas:
// Las licencias generadas por esta aplicación controlan el uso de
// las aplicaciones que conforman el paquete CHIVATO. 
///////////////////////////////////////////////////////////////////////////////

unit UCreateLicence;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ToolWin, ImgList, Buttons,
  UAntiReversing, UlicenceBase1, Menus, ActnList, Spin;

//-----------------------------------------------------------------------------
type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    ActionClose: TAction;
    ActionSaveCode: TAction;
    ActionLoadHardID: TAction;
    SaveDialogTXT: TSaveDialog;
    OpenDialogTXT: TOpenDialog;
    GroupBoxHardID: TGroupBox;
    SpeedButtonOpenHardID: TSpeedButton;
    EditHardwareID: TEdit;
    GroupBoxActivationCode: TGroupBox;
    SpeedButton1: TSpeedButton;
    EditActivationCode: TEdit;
    ImageList1: TImageList;
    ActionPaste: TAction;
    ActionCopy: TAction;
    PopupMenu1: TPopupMenu;
    Hastaelfindeestaquicena1: TMenuItem;
    Hastaelfindeestems1: TMenuItem;
    Hastaelfindeestesemestre1: TMenuItem;
    Hastaelfindeesteao1: TMenuItem;
    GroupBoxApplication: TGroupBox;
    ComboBoxApplication: TComboBox;
    GroupBoxName: TGroupBox;
    EditName: TEdit;
    GroupBoxNotas: TGroupBox;
    MemoNotas: TMemo;
    GroupBoxAppVersion: TGroupBox;
    SpinEditAppVersion: TSpinEdit;
    procedure ActionCloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionLoadHardIDExecute(Sender: TObject);
    procedure EditHardwareIDKeyPress(Sender: TObject; var Key: Char);
    procedure ActionSaveCodeExecute(Sender: TObject);
    procedure EditHardwareIDChange(Sender: TObject);
    procedure SpinEditDaysChange(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ComboBoxApplicationChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure SpinEditAppVersionChange(Sender: TObject);
  private
    Licence: TLicence;
    function IsLicenceDataValid: Boolean;
    function FilterTextHexa(str: String): String;
    function StrReplace(StrInput, Esto, Por: String): String;
    function DateTimeToName(t: TDateTime): String;
    function BooleanToStr(b: Boolean): String;
  public
    procedure CreateCode;
  end;

var
  Form1: TForm1;

implementation

uses DateUtils;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Inicia la aplicación y sus componentes.
//-----------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
ComboBoxApplication.ItemIndex := - 1;
Licence := TLicence.Create;
Licence.Clear;
end;

//-----------------------------------------------------------------------------
// Cierra la aplicación.
//-----------------------------------------------------------------------------
procedure TForm1.ActionCloseExecute(Sender: TObject);
begin
Application.Terminate;
end;

//-----------------------------------------------------------------------------
// Carga el hardware ID desde un fichero de texto.
//-----------------------------------------------------------------------------
procedure TForm1.ActionLoadHardIDExecute(Sender: TObject);
var F: TStrings;
begin
if OpenDialogTXT.Execute then
   if FileExists(OpenDialogTXT.FileName) then
      begin
      F := TStringList.Create;
      F.LoadFromFile(OpenDialogTXT.FileName);
      EditHardwareID.Text := FilterTextHexa(F.Text);
      F.Free;
      end;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si los datos de la licencia son válidos.
//-----------------------------------------------------------------------------
function TForm1.IsLicenceDataValid: Boolean;
begin
Result := True;
if (Length(EditHardwareID.Text) <= 8) or
   (ComboBoxApplication.ItemIndex < 0) or
   (SpinEditAppVersion.Value = 0) or
   (EditName.Text = '') then
   Result := False;
end;

//-----------------------------------------------------------------------------
// Filtra el texto del Hardware ID.
// Deja pasar solo los caracteres hexadecimales.
//-----------------------------------------------------------------------------
procedure TForm1.EditHardwareIDKeyPress(Sender: TObject; var Key: Char);
begin
Key := UpperCase(Key)[1];
if not (Key in ['A'..'F', '0'..'9', #8]) then Key := #0;
end;

//-----------------------------------------------------------------------------
// Para reemplazar cadenas de texto.
// Reemplaza las ocurrencias de una substring dentro de una string.
//-----------------------------------------------------------------------------
function TForm1.StrReplace(StrInput, Esto, Por: String): String;
var aPos: Integer;
begin
aPos := Pos(Esto, StrInput);
Result := '';
while (aPos <> 0) do
      begin
      Result := Result + Copy(StrInput, 1, aPos-1) + Por;
      Delete(StrInput, 1, aPos + Length(Esto)-1);
      aPos := Pos(Esto, StrInput);
      end;
Result := Result + StrInput;
end;

//-----------------------------------------------------------------------------
// Convierte una fecha en una cadena para nombre de fichero.
//-----------------------------------------------------------------------------
function TForm1.DateTimeToName(t: TDateTime): String;
begin
Result := IntToStr(YearOf(t)) + '_';
Result := Result + IntToStr(MonthOf(t)) + '_';
Result := Result + IntToStr(DayOf(t)) + '_';
Result := Result + IntToStr(HourOf(t)) + '_';
Result := Result + IntToStr(MinuteOf(t)) + '_';
Result := Result + IntToStr(SecondOf(t)) + '_';
Result := Result + IntToStr(MilliSecondOf(t));
end;

//-----------------------------------------------------------------------------
// Convierte de booleano a cadena.
//-----------------------------------------------------------------------------
function TForm1.BooleanToStr(b: Boolean): String;
begin
if b then Result := 'SI' else Result := 'NO';
end;

//-----------------------------------------------------------------------------
// Crea la licencia y la guarda en un fichero.
//-----------------------------------------------------------------------------
procedure TForm1.ActionSaveCodeExecute(Sender: TObject);
const cTitle1 = '0S1STS8P;FL';   //'ERROR'
const cText1 = '=/6p?!At3fC!2qSv2e<pV!?dPs=fMbSsN!<f6m=!Cg:jRd;iDf6sTp<!PeYfD!UmRj0d5f6oEd0jHb0OP';    //'No se pudo crear el fichero de licencia.'
var FileWriter: TStrings;
    FName: String;
begin
if EditActivationCode.Text <> '' then
   begin
   FName := StrReplace(ComboBoxApplication.Text, Chr($20), '_');      //Agrega el nombre de la aplicación autorizada.
   FName := FName + '-' + StrReplace(EditName.Text, Chr($20), '_');   //Agrega el nombre del usuario autorizado.
   FName := FName + '_' + DateTimeToName(Licence.LicDateID);          //Agrega la fecha.
   SaveDialogTXT.FileName := FName;
   if SaveDialogTXT.Execute then
      try
         FileWriter := TStringList.Create;
         
         FileWriter.Add('Número de licencia: ' + DateTimeToStr(Licence.LicDateID));
         FileWriter.Add(GroupBoxName.Caption + ': ' + EditName.Text);
         FileWriter.Add('Fecha de creación: ' + DateTimeToStr(Licence.LicDateID));
         FileWriter.Add(GroupBoxHardID.Caption + ': ' + EditHardwareID.Text);
         FileWriter.Add(GroupBoxApplication.Caption + ': ' + ComboBoxApplication.Text);
         FileWriter.Add('');
         FileWriter.Add(GroupBoxActivationCode.Caption + ': ' + EditActivationCode.Text);
         FileWriter.Add('');
         FileWriter.Add(GroupBoxNotas.Caption + ': ' + MemoNotas.Text);

         FileWriter.SaveToFile(SaveDialogTXT.FileName);
      except
         Application.MessageBox(PChar(DecStr(cText1)),
                                PChar(DecStr(cTitle1)),
                                MB_ICONERROR
                                );
      end;
   end;
end;

//-----------------------------------------------------------------------------
// Deja solo los caracteres hexadecimales.
//-----------------------------------------------------------------------------
function TForm1.FilterTextHexa(str: String): String;
var n, len: Integer;
    s: String;
begin
Result := '';
len := Length(str);
s := UpperCase(str);
if len > 0 then
   for n := 1 to len do
       if (str[n] in ['A'..'F', '0'..'9']) then
          Result := Result + s[n];
end;

//-----------------------------------------------------------------------------
// Genera el código de activación de la licencia.
//-----------------------------------------------------------------------------
procedure TForm1.EditHardwareIDChange(Sender: TObject);
begin
CreateCode;
end;

procedure TForm1.SpinEditDaysChange(Sender: TObject);
begin
EditHardwareIDChange(Sender);
end;

procedure TForm1.ComboBoxApplicationChange(Sender: TObject);
begin
EditHardwareIDChange(Sender);
end;

procedure TForm1.EditNameChange(Sender: TObject);
begin
EditHardwareIDChange(Sender);
end;

procedure TForm1.SpinEditAppVersionChange(Sender: TObject);
begin
EditHardwareIDChange(Sender);
end;

//-----------------------------------------------------------------------------
// Crea el código de activación de la licencia.
//-----------------------------------------------------------------------------
procedure TForm1.CreateCode;
var s: String;
    Hour, Min, Sec, MilSec: Word;
begin
EditActivationCode.Text := '';
if IsLicenceDataValid then                                        //Si los parámetros de la licencia son válidos.
   begin
   Licence.LicHardIdKey := FilterTextHexa(EditHardwareID.Text);   //Obtiene la llave de encriptado.
   Licence.LicApp := ComboBoxApplication.ItemIndex + 1;           //Establece la aplicación permitida por la licencia.
   Licence.LicAppVersion := SpinEditAppVersion.Value;             //Versión de la aplicación.
   Licence.LicDateID := Now;                                      //Establece el número de la licencia.
   Licence.Encode;                                                //Crea el número de activación.
   EditActivationCode.Text := Licence.LicCode;                    //Muestra el número en la ventana.
   end;
end;

//-----------------------------------------------------------------------------
// Para pegar textos desde el portapapeles.
//-----------------------------------------------------------------------------
procedure TForm1.ActionPasteExecute(Sender: TObject);
begin
try
   with EditHardwareID do if Focused then PasteFromClipboard;
   with EditName do if Focused then PasteFromClipboard;
   with MemoNotas do if Focused then PasteFromClipboard;
except
   MessageBeep(MB_ICONMASK);
end;
end;

//-----------------------------------------------------------------------------
// Para copiar textos en el portapapeles.
//-----------------------------------------------------------------------------
procedure TForm1.ActionCopyExecute(Sender: TObject);
begin
try
   with EditHardwareID do if Focused then CopyToClipboard;
   with EditName do if Focused then CopyToClipboard;
   with MemoNotas do if Focused then CopyToClipboard;
   with EditActivationCode do if Focused then CopyToClipboard;
except
   MessageBeep(MB_ICONMASK);
end;
end;



end.


