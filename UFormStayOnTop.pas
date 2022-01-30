
///////////////////////////////////////////////////////////////////////////////
// Nombre: UFormStayOnTop
// Autor: Santiago A. Orellana P�rez (Chago)
// Creado: 23/07/2016
// Objetivo: Brinda una clase de formulario que sirve de base para los
//           di�logos StayOnTop que se muestran en la aplicaci�n.
///////////////////////////////////////////////////////////////////////////////


unit UFormStayOnTop;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

//-----------------------------------------------------------------------------
// Esta clase implementa un formulario del cual se pueden heredar
// sus caracter�sticas, como StayOnTop, Centrado en pantalla, etc.
//-----------------------------------------------------------------------------
type
  TStayOnTopForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure PosChanging(var m: TWMWINDOWPOSCHANGED); message WM_WindowPosChanging;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
  public
    Flash: Boolean;
  end;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------
// Establece el estilo StayOnTop para el formulario.
//-----------------------------------------------------------------------------
procedure TStayOnTopForm.FormCreate(Sender: TObject);
begin
FormStyle := fsStayOnTop;       //Establece el StayOnTop de la form.
BorderIcons := [biSystemMenu];  //Quita botones de barra de t�tulos.
end;

//-----------------------------------------------------------------------------
// Procesa los mensajes de cambio de posici�n y fuerza el StayOnTop.
//-----------------------------------------------------------------------------
procedure TStayOnTopForm.PosChanging(var m: TWMWINDOWPOSCHANGED);
begin
m.WindowPos.hwndInsertAfter := HWND_TOP;
end;

//-----------------------------------------------------------------------------
// Evita que minimizen el formulario. 
//-----------------------------------------------------------------------------
procedure TStayOnTopForm.WMSysCommand(var Msg: TWMSysCommand);
begin
if (Msg.CmdType = SC_MINIMIZE) then
   Msg.Result:=0
else
   DefaultHandler(Msg);
end;

end.
