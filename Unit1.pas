unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UAntiReversing, UCommon, UNamedPipe;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FPipe: TPipeServer;
    FSubKey: String;
  public
    procedure ProcessData(Inp: String; var Outp: String);
    function ProcessCommand(ValueName: String; Value: String): String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var NamePipe: String;
begin
//Crea un nombre de subllave para los datos del servicio.
FSubKey := DecStr('0]1TTp8g;uLx=b6s?fA]3') +      //'\Software\'
           DecStr(cCompanny) + '\' +
           DecStr(cAppTitle) + '\' +
           DecStr(cAppVersion);

//Crea un nombre para el objeto de comunicación PIPE.
NamePipe := CreateNameFor(DecStr(cPSecurePIPE));

//Crea el subproceso que atiende las peticiones de los clientes.
try
   FPipe := TPipeServer.CreatePipeServer('', NamePipe, False);  //Crea el PIPE.
   FPipe.OnProcessData := ProcessData;                          //Asigna el evento que procesa los datos.
   FPipe.StartUpServer;                                         //Inicial el pipe.
   FPipe.Resume;                                                //Inicia el subproceso del PIPE.
except
   //Error en el servidor, no se pudo crear el PIPE.
end;
end;

//-----------------------------------------------------------------------------
// Procesa los mensajes de los clientes.
//
// Parámetros:
// Inp = Cadena con el mensaje enviado por el cliente.
//       El mensaje está codificado y hay que extraerlo.
// Outp = Cadena donde se colca el mensaje que se le envía al cliente.
//        Esta cadena también está codificada y hay que extraerla.
//-----------------------------------------------------------------------------
procedure TForm1.ProcessData(Inp: String; var Outp: String);
var ValueName, Value: String;
begin
DecProtectMsg(Inp,            //Extrae el mensaje protegido...
              ValueName,      //el nombre del parámetro y
              Value           //el valor del parámetro.
              );
Outp := EncStr(ProcessCommand(ValueName, Value));   //Procesa el comando
end;

//-----------------------------------------------------------------------------
// Procesa los comandos enviados por el cliente.
// Lo que hace es gestionar internamente las escrituras y lecturas
// de llaves en el registro de Windows y otras operaciones de fichero.
//
// Entrdas:
// ValueName = Nombre del parámetro.
// Value = Valor que se le asigna al parámetro. Si se pasa una cadena
//         vacía, la función lee el parámetro y devuelve su valor.
//
// Salida:
// Si "Value" contiene texto, lo intenta guardar como valor del parámetro.
//    Si tiene éxito, la función retorna el valor guardado.
// Si "Value" es cadena vacía, intenta leer el valor del parámetro.
//    Si tiene éxito, la función retorna el valor leido.
// En ambos casos, si ocurre algún error, o el parámetro
// no existe la función devuelve una cadena vacía.
//-----------------------------------------------------------------------------
function TForm1.ProcessCommand(ValueName: String; Value: String): String;
begin
Result := '';                                               //Por defecto devuelve cadena vacía.
if ValueName = '' then Exit;
if Value <> '' then                                         //Si se tiene que escribir un valor en el registro...
   begin
   if ParamToRegistry(HKEY_LOCAL_MACHINE,                   //Llave raiz donde se aloja la subllave.
                      FSubKey,                              //Subllave específica.
                      CreateNameFor(DecStr(ValueName)),     //Nombre del valor de la subllave.
                      Value                                 //Valor que se le asigna.
                      ) then Result := Value;               //Devuelve el mismo valor.
   end
else                                                        //Si se debe leer un valor...
   begin
   Result := ParamFromRegistry(HKEY_LOCAL_MACHINE,                //Llave raiz donde se aloja la subllave.
                               FSubKey,                           //Subllave específica.
                               CreateNameFor(DecStr(ValueName))   //Nombre del valor de la subllave.
                               );

   end;
end;

end.
 