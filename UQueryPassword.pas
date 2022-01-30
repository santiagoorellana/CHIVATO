
///////////////////////////////////////////////////////////////////////////////
// Nombre: UQueryPassword
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 23/07/2016
// Objetivo: Permite abrir un diálogo para pedirle al usuario una
//           password. El diálogo hereda de la clase TStayOnTopForm.
//-----------------------------------------------------------------------------
// Modo de uso:
//
// var Res: Boolean;        //Guardará el resultado de la opción del usuario.
// ...
// Res := InputQueryPassword(var Value: string);
// 
///////////////////////////////////////////////////////////////////////////////

unit UQueryPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UFormStayOnTop, UAntiReversing;

//-----------------------------------------------------------------------------
// Abre un diálogo para pedirle al usuario el password de acceso.
//-----------------------------------------------------------------------------
function InputQueryPassword(var Value: string     //Variable que recibe el texto insertado por el usuario.
                            ): Boolean;           //Devuelve TRUE si el usuario pulsa ENTRAR.

implementation

//-----------------------------------------------------------------------------
// Constantes que definen textos del mensaje de diálogo.
//-----------------------------------------------------------------------------
const SMsgDlgOK           = '0sPoKuPsMbGF8';      //'Entrar' codificado.
const SMsgDlgCancel       = 'LsHbXoKdYf:mLb<D3';  //'Cancelar' codificado.
const cPasswordChar       = '?';
const cMaxPasswordLength  = 50;
const cTitle              = '0P1PTO8U;SLP=M6!?EAF3!CB2DSD2F<TVD?';                       //'CONTROL DE ACCESO'
const cMessage            = 'P/=oMeSuNs<p6e=vC{:dRb;!Dm6bT!<qPbYtDtUxRp0s5e6/E/0JH';     //'Introduzca la password...'

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
// Abre un diálogo para pedirle al usuario el password de acceso.
//-----------------------------------------------------------------------------
function InputQueryPassword(var Value: string): Boolean;
var
  Form: TStayOnTopForm;
  Prompt: TLabel;
  Edit: TEdit;
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
          Caption := DecStr(cTitle);
          ClientWidth := MulDiv(180, DialogUnits.X, 4);
          Position := poScreenCenter;
          Prompt := TLabel.Create(Form);
          Font.Size := 12;
          with Prompt do
               begin
               Parent := Form;
               Caption := DecStr(cMessage);
               Left := MulDiv(8, DialogUnits.X, 4);
               Top := MulDiv(8, DialogUnits.Y, 8);
               Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
               WordWrap := True;
               end;
          Edit := TEdit.Create(Form);
          with Edit do
               begin
               Parent := Form;
               Left := Prompt.Left;
               Top := Prompt.Top + Prompt.Height + 5;
               Width := MulDiv(164, DialogUnits.X, 4);
               Text := Value;
               PasswordChar := cPasswordChar;
               MaxLength := cMaxPasswordLength;
               SelectAll;
               Font.Size := 14;
               Font.Style := [fsBold];
               end;
          ButtonTop := Edit.Top + Edit.Height + 15;
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
                         Edit.Top + Edit.Height + 15,
                         ButtonWidth,
                         ButtonHeight
                         );
               Form.ClientHeight := Top + Height + 13;
               end;
          FormStyle := fsStayOnTop;
          if ShowModal = mrOk then
             begin
             Value := Edit.Text;
             Result := True;
             end;
          Form.Free;
       except
          MessageBeep(MB_ICONERROR);
       end;
end;

end.
