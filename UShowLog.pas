
///////////////////////////////////////////////////////////////////////////////
// Nombre: UShowLog
// Autor: Santiago A. Orellana P�rez (Chago)
// Creado: 17/12/2016
// Objetivo: Permite abrir un di�logo para mostrar al usuario el fichero log.
//-----------------------------------------------------------------------------
// Modo de uso:
//
// Res := OutputShowLogFile(TextoDelFichero);
//
///////////////////////////////////////////////////////////////////////////////

unit UShowLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UFormStayOnTop, UAntiReversing;

//-----------------------------------------------------------------------------
// Abre un di�logo para mostrarle al usuario el texto de un fichero log.
//-----------------------------------------------------------------------------
procedure OutputShowLogFile(pText: string);

implementation

//-----------------------------------------------------------------------------
// Constantes que definen textos del mensaje de di�logo.
//-----------------------------------------------------------------------------
const cTitle              = '0H1JTD8I;FLS=P6!?MAP3GC';            //'FICHERO LOG'

//-----------------------------------------------------------------------------
// Abre un di�logo para mostrarle al usuario el identificador del hardware.
//-----------------------------------------------------------------------------
procedure OutputShowLogFile(pText: string);
var Form: TStayOnTopForm;
    Memo: TMemo;
    Init: TDateTime;
begin
  Init := Now;
  Form := TStayOnTopForm.Create(Application);
  with Form do
       try
          Flash := True;
          Canvas.Font := Font;
          BorderStyle := bsSizeable;
          Caption := DecStr(cTitle);
          ClientWidth := 400;
          ClientHeight := 300;
          Position := poScreenCenter;
          Font.Size := 12;
          Memo := TMemo.Create(Form);
          with Memo do
               begin
               Parent := Form;
               Align := alClient;
               Text := pText;
               Font.Size := 8;
               Font.Style := [];
               WordWrap := False;
               ScrollBars := ssBoth;
               end;
          FormStyle := fsStayOnTop;
          ShowModal;
          Form.Free;
       except
          MessageBeep(MB_ICONERROR);
       end;
end;

end.
