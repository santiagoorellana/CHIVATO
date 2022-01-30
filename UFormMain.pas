
///////////////////////////////////////////////////////////////////////////////
// Nombre: UFormMain
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 23/07/2016
// Objetivo: Implementa el formulario principal de la aplicación.
///////////////////////////////////////////////////////////////////////////////

unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UQueryPassword, ExtCtrls, UStoreMonitor, UStoreDataControl,
  UFormInterface, UAccessControl, UAntiReversing, ImgList,
  UCommon, ShellApi, SHFolder;

//-----------------------------------------------------------------------------
// Algunas constantes de cadena (encriptadas con algoritmo muy sencillo)
//-----------------------------------------------------------------------------
const cHotKey = $70;                                  //Este es el código de la tecla F1 que representa al programa CHIVATO.
const cLoaderFileName1 = 'ldr.exe';

//-----------------------------------------------------------------------------
// Clase que implementa el formulario principal.
//-----------------------------------------------------------------------------
type
  TMainForm = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    IdleCount: Integer;

    procedure OnHotKey(var Msg : TWMHotKey); message WM_HotKey;
    procedure OnEndSession(var Msg: TWMQueryEndSession); message WM_QUERYENDSESSION;

    procedure CreateDataControl;
    procedure CreateStoreMonitor;
    procedure CreateAccessControl;

    function Exist(SemaphoreName: String): Boolean;
    
    procedure Idle(Sender: TObject; var Done: Boolean);
  public
    DataControl: TDataControl;
    StoreMonitor: TStoreMonitor;
    AccessControl: TAccessControl;

    DataDirectory: String;
    AppDirectory: String;
    LoadedPassword: Boolean;

    procedure HideInShel;
    procedure ShowInShel;
    procedure EndMonitor;

    //Eventos del monitor de dispositivos de almacenamiento.
    procedure OnConect(store: Integer);
    procedure OnDisconect(store: Integer);
    procedure OnRegister(store: TStoreInfo);
    procedure OnUnregister(store: TStoreInfo);
    procedure OnEndSearch;
  end;

//-----------------------------------------------------------------------------
// Estas son variables globales en esta UNIT.
//-----------------------------------------------------------------------------
var
  MainForm: TMainForm;
  FormInterface: TFormInterface;


implementation

uses Math, DateUtils;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Inicia el formulario y la aplicación en general.
//-----------------------------------------------------------------------------
procedure TMainForm.FormCreate(Sender: TObject);
var s: AnsiString;
begin
IdleCount := 0;

//Obtiene los directorios base para el trabajo de la aplicación.
AppDirectory := NormaliceDirectory(ExtractFilePath(Application.ExeName));         //Obtiene el directorio del fichero ejecutable.
DataDirectory := NormaliceDirectory(GetDataDirectory(CSIDL_LOCAL_APPDATA));       //Obtiene el directorio de datos de las aplicaciones del usuario actual.

HideInShel;               //Oculta el formulario principal de la aplicación.
CreateAccessControl;      //Crea el controlador de accesos.
CreateDataControl;        //Crea el controlador de datos.
CreateStoreMonitor;       //Crea el monitor de dispositivos de almacenamiento.

//Registra la Hot Key que sirve de entrada para el usuario administrador.
s := 'Ctrl_Alt_Shift_' + IntToHex(cHotKey, 2);
RegisterHotKey(Handle,                                //Registra una HotKey para el formulario.
               GlobalAddAtom(^s),                     //Obtiene un identificador para el hotkey.
               MOD_ALT or MOD_SHIFT or MOD_CONTROL,   //Establece la combinación de teclas.
               cHotKey                                //Establece la tecla de activación.
               );

Application.OnIdle := Idle;                           //Establece un manejador de eventos para la aplicación.
end;

//-----------------------------------------------------------------------------
// Oculta el formulario principal, quitándolo de la barra de tareas y del
// comando Alt-tab. Solo se hace visible en el administrador de tareas.
//-----------------------------------------------------------------------------
procedure TMainForm.HideInShel;
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
// Muestra el formulario principal, mostrándolo en la barra de tareas y en
// el comando Alt-tab. También es visible en el administrador de tareas.
//-----------------------------------------------------------------------------
procedure TMainForm.ShowInShel;
begin
BorderStyle := bsSizeable;               //Mantener este valor para que responda a las hotkey globales.
Left := 0;                               //Pegamos el formulario a la esquina
Top := 0;                                //superior izquierda de la pantalla.
Width := 0;                              //Le asignamos un tamaño bastante pequeño.
Height := 0;                             //
Visible := False;                        //Hacemos que el formulario no sea visible.
Application.Title := DecStr(cAppTitle);  //Le ponemos título a la ventana.
Application.ShowMainForm := True;        //Mostramos el formulario.

//Lo mostramos en la barra de tareas y el comando Alt + Tab.
ShowWindow(Application.Handle, SW_SHOW);
SetWindowLong(Application.Handle,                                  //Handle de la ventana.
              GWL_EXSTYLE,                                         //Valor que se desea configurar.
              GetWindowLong(Application.Handle, GWL_EXSTYLE) or    //Establece el estilo actual y
              not WS_EX_TOOLWINDOW and WS_EX_APPWINDOW);           //esto...
end;

//-----------------------------------------------------------------------------
// Crea el controlador de acceso.
//-----------------------------------------------------------------------------
procedure TMainForm.CreateAccessControl;
begin
try
   LoadedPassword := False;
   AccessControl := TAccessControl.Create;           //Crea el objeto de control de acceso...
   AccessControl.Directory := DataDirectory;         //Establece el directorio base de trabajo del objeto.
   LoadedPassword := AccessControl.LoadPassword;     //Intenta cargar la password.
except
   Application.Terminate;
end;
end;

//-----------------------------------------------------------------------------
// Crea dinámicamente el controlador de datos de la aplicación.
// Si la base de datos existe, la carga. De lo contrario la crea,
// la guarda en un fichero y luego la carga para agregarle datos.
//
// Nota: Por cada usuario de Windows se crea una base de datos.
//-----------------------------------------------------------------------------
procedure TMainForm.CreateDataControl;
var DatFile: String;
begin
try
   DataControl := TDataControl.Create;                                //Crea el controlador de datos.
   DatFile := DataDirectory + CreateNameFor(cFileInit, cFDataBase);   //Crea el nombre de la base de datos.
   if not FileExists(DatFile) then                                    //Si la base de datos no existe...
      DataControl.CreateDB(DatFile);                                  //Crea la base de datos en un fichero.
   DataControl.OpenDB(DatFile);                                       //Carga la base de datos desde el fichero.
except
   Application.Terminate;
end;
end;

//-----------------------------------------------------------------------------
// Crea dinámicamente el monitor de dispositivos de almacenamiento.
//-----------------------------------------------------------------------------
procedure TMainForm.CreateStoreMonitor;
begin
try
   StoreMonitor := TStoreMonitor.Create;
   StoreMonitor.OnConect := OnConect;
   StoreMonitor.OnDisconect := OnDisconect;
   StoreMonitor.OnRegister := OnRegister;
   StoreMonitor.OnUnregister := OnUnregister;
   StoreMonitor.OnEndSearch := OnEndSearch;
   StoreMonitor.Resume;
except
   Application.Terminate;
end;
end;

//-----------------------------------------------------------------------------
// Indica que se produjo la conexión de un dispositivo.
//-----------------------------------------------------------------------------
procedure TMainForm.OnConect(store: Integer);
begin
//....
end;

//-----------------------------------------------------------------------------
// Indica la desconexión de un dispositivo.
//-----------------------------------------------------------------------------
procedure TMainForm.OnDisconect(store: Integer);
begin
//....
end;

//-----------------------------------------------------------------------------
// Indica que se han registrado los datos de un dispositivo.
//-----------------------------------------------------------------------------
procedure TMainForm.OnRegister(store: TStoreInfo);
begin
//if StoreMonitor.ReportDevices then
//Se copian ficheros promocionales a la memoria......
end;

//-----------------------------------------------------------------------------
// Indica que se han eliminado los datos de un dispositivo ya desconectado.
//
// Aquí se comprueba el estado de la licencia. Si no es válida o si la versión
// de la aplicación autorizada difiere de la versión actual de CHIVATO, solo
// permite que se puedan agregar unos pocos registros de dispositivos.
//-----------------------------------------------------------------------------
procedure TMainForm.OnUnregister(store: TStoreInfo);
begin
try
   DataControl.AddData(store.sLeter,
                       store.sName,
                       StoreMonitor.TypeToString(store.sType),
                       IntToHex(store.sSerial, 8),
                       store.sCapacity,
                       store.sDateTimeConect,
                       store.sTimeConectionMin,
                       store.sInfoInitial,
                       store.sInfoMinimal,
                       store.sInfoFinal
                       );
except
end;
end;

//-----------------------------------------------------------------------------
// Indica que se ha terminado un ciclo de búsqueda.
//-----------------------------------------------------------------------------
procedure TMainForm.OnEndSearch;
begin
if not StoreMonitor.ReportDevices then    //Si esta es la primera búsqueda...
   begin
   StoreMonitor.IgnorePresentDevices;     //Ignora los dispositivos que ya están conectados.
   StoreMonitor.ReportDevices := True;
   end;
 end;

//-----------------------------------------------------------------------------
// Procesa el evento de la HotKey.
// Si la conbinación de teclas es Ctrl+Alt+Shift+P se abre un diálogo que
// pide al usuario la password de acceso. Si el usuario inserta la password
// correcta, se abre el formulario de interface que permite interactuar con
// la aplicación.
//-----------------------------------------------------------------------------
procedure TMainForm.OnHotKey(var Msg: TWMHotKey);
var Password: String;
    rIQP: Boolean;
begin
if Assigned(FormInterface) then                           //Si el formulario está creado y es
   if FormInterface.Visible then Exit;                    //visible, entonces no hace nada.
LoadedPassword := AccessControl.LoadPassword;             //Intenta cargar la password.
if not LoadedPassword then                                //Si no hay password cargada
   Application.MessageBox(PChar('No se ha podido cargar la password original.'),          //muestra un mensaje al usuario
                          PChar('ALERTA'),
                          MB_ICONASTERISK
                          );
rIQP := InputQueryPassword(Password);                     //Muestra el diálogo de password.
if rIQP then                                              //Si el diálogo se ejecutó normalmente...
   if AccessControl.ValidatePassword(Password) then       //Si la password es válida...
      begin
      FormInterface := TFormInterface.Create(Self);       //Crea el formulario de interface.
      FormInterface.ShowModal;                            //Muestra el formulario de interface.
      end;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el semáforo especificado existe.
// Intenta crear un semáforo con el nombre especificado. Si el sistema
// impide la creación del semáforo y devuelve ERROR_ALREADY_EXISTS,
// entonces se asume que el semáforo existe. De lo contrario se crea
// el semáforo y se elimina inmediatamente antes de salir de la función.
//-----------------------------------------------------------------------------
function TMainForm.Exist(SemaphoreName: String): Boolean;
var Semaphore: THandle;
begin
try
   Semaphore := CreateSemaphore(nil, 0, 1, PChar(CreateNameFor(cFileInit, DecStr(SemaphoreName))));
except
   Semaphore := 0;
end;   
result := (Semaphore <> 0) and (GetLastError = ERROR_ALREADY_EXISTS);
if Semaphore <> 0 then CloseHandle(Semaphore);
end;

//-----------------------------------------------------------------------------
// Finaliza la ejecución del monitoreo hasta el próximo reinicio.
//-----------------------------------------------------------------------------
procedure TMainForm.EndMonitor;
begin
//Guarda los datos de los dispositivos que aún están
//conectados y termina el monitoreo de dispositivos.
MainForm.StoreMonitor.Terminate;            //Ordena terminar el subproceso y
MainForm.StoreMonitor.WaitFor;              //espera a que se termine.

//Termina las ejecuciones de los cargadores y el programa.
KillTask(cLoaderFileName1);                 //Termina el cargador.
Application.Terminate;                      //Cierra la aplicación.
end;

//-----------------------------------------------------------------------------
// Detecta el cierre de windows y realiza operaciones antes de cerrar.
//-----------------------------------------------------------------------------
procedure TMainForm.OnEndSession(var Msg: TWMQueryEndSession);
begin
EndMonitor;               //Termina el monitoreo.
Msg.result := 0;          //Permite el cierre de windows.
Application.Terminate;    //Termina la ejecución del programa.
end;

//-----------------------------------------------------------------------------
// Se ejecuta mientras la ventana no está siendo utilizada.
// Verifica que se esté ejecutando el cargador.
// Muestra mensajes de retroalimentación al usuario.
//-----------------------------------------------------------------------------
procedure TMainForm.Idle(Sender: TObject; var Done: Boolean);
var LoaderFile1: String;
begin
Done := True;

//Aquí se comprueba que el programa cargador esté funcionando.
//Si no está funcionando, se ejecuta inmediatamente.
try
   if IdleCount = 0 then                                                    //Si pasa por el cero...
      begin
      LoaderFile1 := AppDirectory + cLoaderFileName1;                       //Crea el nombre del primer fichero.
      if (not Exist(cSemaphoreLoader)) and FileExists(LoaderFile1) then     //Si "ldr.exe" existe pero no se está ejecutando...
         ShellExecute(Handle, nil,PChar(LoaderFile1),'','',SW_SHOWNORMAL);  //Ejecuta el fichero "ldr.exe".
      end;
   IdleCount := (IdleCount + 1) and $FFFF;                                  //Incrementa el contador.
except
end;
end;

end.




