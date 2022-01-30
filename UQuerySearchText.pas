
///////////////////////////////////////////////////////////////////////////////
// Nombre: UQuerySearchText
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 14/12/2016
// Objetivo: Permite abrir un diálogo para que el usuario pueda realizar
//           búsquedas de cadenas de texto en la tabla de datos. El diálogo
//           hereda de la clase TStayOnTopForm.
//-----------------------------------------------------------------------------
// Modo de uso:
//
// var Res: Boolean;        //Guardará el resultado de la opción del usuario.
// ...
// Res := InputQueryNewPassword(var Value: string);
//
///////////////////////////////////////////////////////////////////////////////

unit UQuerySearchText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UFormStayOnTop, UAntiReversing;

//-----------------------------------------------------------------------------
// Abre un diálogo para pedirle al usuario el password de acceso.
//-----------------------------------------------------------------------------
function InputQuerySearchText(var Text: String;               //Texto que se desea buscar.
                              var CaseSensitive: Boolean;     //Sensibilidad a la diferencia de mayúsculas.
                              var Exact: Boolean              //Exactitud de la comparación.
                              ): Boolean;                 //Devuelve TRUE si el usuario pulsa BUSCAR.

implementation

//-----------------------------------------------------------------------------
// Constantes que definen textos del mensaje de diálogo.
//-----------------------------------------------------------------------------
const SMsgDlgOK           = '=sKvMtUd=bDC<';    //'Buscar' codificado.
const SMsgDlgCancel       = 'LsHbXoKdYf:mLb<D3';  //'Cancelar' codificado.
const cMaxTextLength      = 200;
const cCaption            = '0P1VTT8D;BLS=!6U?FAY3UCC2';   //'BUSCAR TEXTO'
const cLabel1Text         = 'S;2f<yVu?pP!=qMbSsNb<!6c=vCt:dRb;sDU6';   //'Texto para buscar:'
const cLabel2Text         = '7/6j;g7fHsDf;o6dIjObOs3!3n2b4zQûQt@dIvBmObDt>!RzR!5n3j2o2û8tLd9vGm:b?tSE=';     //'Diferenciar mayúsculas y minúsculas.'
const cLabel3Text         = '0/MpYjPoOd8j6eVfPoHdTjLb<!3fJy;bTd6u;bFDX';   //'Coincidencia exacta.'

//-----------------------------------------------------------------------------
// Función auxiliar.
//-----------------------------------------------------------------------------
function GetAveCharSize(Canvas: TCanvas): TPoint;
var I: Integer;
    Buffer: array[0..51] of Char;
begin
for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
Result.X := Result.X div 52;
end;

//-----------------------------------------------------------------------------
// Abre un diálogo que permite al usuario buscar un texto en la tabla.
//-----------------------------------------------------------------------------
function InputQuerySearchText(var Text: String;               //Texto que se desea buscar.
                              var CaseSensitive: Boolean;     //Sensibilidad a la diferencia de mayúsculas.
                              var Exact: Boolean              //Exactitud de la comparación.
                              ): Boolean;                     //Devuelve TRUE si el usuario pulsa BUSCAR.
const cw = 175;
var
  Form: TStayOnTopForm;
  Label1: TLabel;
  Edit1: TEdit;
  Check1: TCheckBox;
  Check2: TCheckBox;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TStayOnTopForm.Create(Application);
  with Form do
       try
          Flash := True;
          Canvas.Font := Font;
          DialogUnits := GetAveCharSize(Canvas);
          BorderStyle := bsDialog;
          Caption := DecStr(cCaption);   //'Buscar texto'
          ClientWidth := MulDiv(cw + 15, DialogUnits.X, 4);
          Position := poScreenCenter;
          Font.Size := 12;
          Label1 := TLabel.Create(Form);
          with Label1 do
               begin
               Parent := Form;
               Caption := DecStr(cLabel1Text);
               Left := MulDiv(8, DialogUnits.X, 4);
               Top := MulDiv(8, DialogUnits.Y, 8);
               Constraints.MaxWidth := MulDiv(cw, DialogUnits.X, 4);
               WordWrap := True;
               end;
          Edit1 := TEdit.Create(Form);
          with Edit1 do
               begin
               Parent := Form;
               Left := Label1.Left;
               Top := Label1.Top + Label1.Height + 5;
               Width := MulDiv(cw, DialogUnits.X, 4);
               Text := '';
               PasswordChar := chr(0);
               MaxLength := cMaxTextLength;
               SelectAll;
               Font.Size := 14;
               Font.Style := [];
               end;
          Check1 := TCheckBox.Create(Form);
          with Check1 do
               begin
               Parent := Form;
               Caption := DecStr(cLabel2Text);
               Left := Label1.Left;
               Top := Edit1.Top + Edit1.Height + 15;
               Width := MulDiv(cw, DialogUnits.X, 4);
               Font.Size := 10;
               Checked := CaseSensitive;
               end;
          Check2 := TCheckBox.Create(Form);
          with Check2 do
               begin
               Parent := Form;
               Caption := DecStr(cLabel3Text);
               Left := Label1.Left;
               Top := Check1.Top + Check1.Height + 15;
               Width := MulDiv(cw, DialogUnits.X, 4);
               Font.Size := 10;
               Checked := Exact;
               end;
          ButtonTop := Check2.Top + Check2.Height + 15;
          ButtonWidth := MulDiv(55, DialogUnits.X, 4);
          ButtonHeight := MulDiv(20, DialogUnits.Y, 8);
          with TButton.Create(Form) do
               begin
               Parent := Form;
               Caption := DecStr(SMsgDlgOK);
               ModalResult := mrOk;
               Default := True;
               SetBounds(MulDiv(30, DialogUnits.X, 4),
                         ButtonTop,
                         ButtonWidth,
                         ButtonHeight
                         );
               end;
          with TButton.Create(Form) do
               begin
               Parent := Form;
               Caption := DecStr(SMsgDlgCancel);
               ModalResult := mrCancel;
               Cancel := True;
               SetBounds(MulDiv(105, DialogUnits.X, 4),
                         Check2.Top + Check2.Height + 15,
                         ButtonWidth,
                         ButtonHeight
                         );
               Form.ClientHeight := Top + Height + 13;
               end;
          FormStyle := fsStayOnTop;
          if ShowModal = mrOk then
             begin
             Text := Edit1.Text;
             CaseSensitive := Check1.Checked;
             Exact := Check2.Checked;
             Result := True;
             end;
          Form.Free;
       except
          MessageBeep(MB_ICONERROR);
       end;
end;

end.
