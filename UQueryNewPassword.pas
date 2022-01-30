
///////////////////////////////////////////////////////////////////////////////
// Nombre: UQueryNewPassword
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 10/08/2016
// Objetivo: Permite abrir un diálogo para que el usuario pueda cambiar
//           la password de acceso. El diálogo hereda de la clase
//           TStayOnTopForm.
//-----------------------------------------------------------------------------
// Modo de uso:
//
// var Res: Boolean;        //Guardará el resultado de la opción del usuario.
// ...
// Res := InputQueryNewPassword(var Value: string);
//
///////////////////////////////////////////////////////////////////////////////

unit UQueryNewPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UFormStayOnTop, UAntiReversing;

//-----------------------------------------------------------------------------
// Abre un diálogo para pedirle al usuario el password de acceso.
//-----------------------------------------------------------------------------
function InputQueryNewPassword(var Old: String;       //Password actual.
                               var New1: String;      //Password nueva.
                               var New2: String       //Repetición de la password nueva.
                               ): Boolean;            //Devuelve TRUE si el usuario pulsa ENTRAR.

implementation

//-----------------------------------------------------------------------------
// Constantes que definen textos del mensaje de diálogo.
//-----------------------------------------------------------------------------
const SMsgDlgOK           = '0s1bTn8c;jLb=D6';    //'Cambiar' codificado.
const SMsgDlgCancel       = 'LsHbXoKdYf:mLb<D3';  //'Cancelar' codificado.
const cPasswordChar       = '?';
const cMaxPasswordLength  = 50;
const cCaption            = 'FEXB7N6C;J7BHSD!;Q6BITOTOX3P3S2D4';   //'CAMBIAR PASSWORD'
const cLabel1Text         = '=;Cb:tRt;xDp6sTe<!PbYdDuUvRb0m5Q6';   //'Password actual:'
const cLabel2Text         = 'E;0bHt0tPxKpPsMeG!8oLvHfXwKbYQ:';     //'Password nueva:'
const cLabel3Text         = 'L;<f3qPfDuUjEdHjXôLo>!0eMfY!PqOb8t6tVxPpHsTeL!<o3vJf;wTb6S;';   //'Repetición de password nueva:'

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
// Abre un diálogo que permite al usuario cambiar el password de acceso.
//-----------------------------------------------------------------------------
function InputQueryNewPassword(var Old: String; var New1: String; var New2: String): Boolean;
var
  Form: TStayOnTopForm;
  Label1: TLabel;
  Label2: TLabel;
  Label3: TLabel;
  Edit1: TEdit;
  Edit2: TEdit;
  Edit3: TEdit;
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
          Caption := DecStr(cCaption);   //'Cambiar password'
          ClientWidth := MulDiv(180, DialogUnits.X, 4);
          Position := poScreenCenter;
          Font.Size := 12;
          Label1 := TLabel.Create(Form);
          with Label1 do
               begin
               Parent := Form;
               Caption := DecStr(cLabel1Text);  //'Password actual:'
               Left := MulDiv(8, DialogUnits.X, 4);
               Top := MulDiv(8, DialogUnits.Y, 8);
               Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
               WordWrap := True;
               end;
          Edit1 := TEdit.Create(Form);
          with Edit1 do
               begin
               Parent := Form;
               Left := Label1.Left;
               Top := Label1.Top + Label1.Height + 5;
               Width := MulDiv(164, DialogUnits.X, 4);
               Text := '';
               PasswordChar := cPasswordChar;
               MaxLength := cMaxPasswordLength;
               SelectAll;
               Font.Size := 14;
               Font.Style := [fsBold];
               end;
          Label2 := TLabel.Create(Form);
          with Label2 do
               begin
               Parent := Form;
               Caption := DecStr(cLabel2Text);   //'Password nueva:'
               Left := Label1.Left;
               Top := Edit1.Top + Edit1.Height + 15;
               Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
               WordWrap := True;
               end;
          Edit2 := TEdit.Create(Form);
          with Edit2 do
               begin
               Parent := Form;
               Left := Label1.Left;
               Top := Label2.Top + Label2.Height + 5;
               Width := MulDiv(164, DialogUnits.X, 4);
               Text := '';
               PasswordChar := cPasswordChar;
               MaxLength := cMaxPasswordLength;
               SelectAll;
               Font.Size := 14;
               Font.Style := [fsBold];
               end;
          Label3 := TLabel.Create(Form);
          with Label3 do
               begin
               Parent := Form;
               Caption := DecStr(cLabel3Text);   //'Repetición de password nueva:'
               Left := Label1.Left;
               Top := Edit2.Top + Edit2.Height + 15;
               Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
               WordWrap := True;
               end;
          Edit3 := TEdit.Create(Form);
          with Edit3 do
               begin
               Parent := Form;
               Left := Label1.Left;
               Top := Label3.Top + Label3.Height + 5;
               Width := MulDiv(164, DialogUnits.X, 4);
               Text := '';
               PasswordChar := cPasswordChar;
               MaxLength := cMaxPasswordLength;
               SelectAll;
               Font.Size := 14;
               Font.Style := [fsBold];
               end;
          ButtonTop := Edit3.Top + Edit3.Height + 15;
          ButtonWidth := MulDiv(55, DialogUnits.X, 4);
          ButtonHeight := MulDiv(20, DialogUnits.Y, 8);
          with TButton.Create(Form) do
               begin
               Parent := Form;
               Caption := DecStr(SMsgDlgOK);
               ModalResult := mrOk;
               Default := True;
               SetBounds(MulDiv(25, DialogUnits.X, 4),
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
               SetBounds(MulDiv(100, DialogUnits.X, 4),
                         Edit3.Top + Edit3.Height + 15,
                         ButtonWidth,
                         ButtonHeight
                         );
               Form.ClientHeight := Top + Height + 13;
               end;
          FormStyle := fsStayOnTop;
          if ShowModal = mrOk then
             begin
             Old := Edit1.Text;
             New1 := Edit2.Text;
             New2 := Edit3.Text;
             Result := True;
             end;
          Form.Free;
       except
          MessageBeep(MB_ICONERROR);
       end;
end;

end.
