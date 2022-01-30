
///////////////////////////////////////////////////////////////////////////////
// Nombre: UAccessControl
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 09/08/2016
// Objetivo: Implementa el controlador de accesos de la aplicación.
//           Hace uso de las técnicas antidebug.
///////////////////////////////////////////////////////////////////////////////

unit UAccessControl;

interface

uses
  USHA256, SysUtils, UAntiReversing, Windows, Classes, UCommon;

//-----------------------------------------------------------------------------
// Hash codificado de la password de acceso "87012807902".
// Esta password da acceso total a la aplicación. No se le da a los clientes.
//-----------------------------------------------------------------------------
const cFixedPassword = '@57GU1C65CW7J3H933B115M9588E42>6J:BCH9<EM:AC32M1XG94Q7A5MCY5B218OB42HDI:U8I7PE=GJGTD;B:839?609=8U:733GT1?166:CC6V7A26F52J6<C@GH4S';

//-----------------------------------------------------------------------------
// Hash codificado de la password de acceso "1234".
// Esta es la password inicial que por defecto tiene la aplicación.
//-----------------------------------------------------------------------------
const cInitialPassword = '0514TB8D;7L8=563?2A73GC42FS226<DV8?7P2=FMFS2NB<66F=3C6:6RG;1D768T:<6P4Y7D3U4RD095C64E909HC05P5K6P:MFG284LGH:X8K9YE:8LD<935P7DGU1E';

//-----------------------------------------------------------------------------
// Clase que administra el acceso de los usuarios al programa.
//-----------------------------------------------------------------------------
type
  TAccessControl = class
  private
    FDir: String;
    FPasswordHash: String;
    function SaveToFile: Boolean;
    function LoadFromFile: Boolean;
    function FilterPasswordChars(str: String): String;
  public
    constructor Create;
    destructor Destroy;
    function SetPassword(Old: String; New1: String; New2: String): Boolean;
    function ValidatePassword(Attempted: String): Boolean;
    function SavePassword: Boolean;
    function LoadPassword: Boolean;
    property Directory: String read FDir write Fdir;
  end;

implementation

uses UFormMain;

//-----------------------------------------------------------------------------
// Inicia la instancia.
//-----------------------------------------------------------------------------
constructor TAccessControl.Create;
begin
FPasswordHash := '';
FDir := '';
end;

//-----------------------------------------------------------------------------
// Borra los datos comprometedores.
//-----------------------------------------------------------------------------
destructor TAccessControl.Destroy;
begin
FPasswordHash := '';
FDir := '';
end;

//-----------------------------------------------------------------------------
// Permite establecer una password nueva.
//
// Parámetros:
// Old  = Password actual que se quiere cambiar.
// New1 = Password nueva que se quiere establecer.
// New2 = Repetición de la password nueva que se quiere establecer.
//
// Resultado:
// Devuelve TRUE si se pudo cambiar la password.
//-----------------------------------------------------------------------------
function TAccessControl.SetPassword(Old: String; New1: String; New2: String): Boolean;
begin
Result := False;
if (New1 = New2) and                      //Si las password nuevas son iguales
   (Length(New1)> 0) then                 //y tienen al menos un caracter...
   if ValidatePassword(Old) then          //y si la password vieja es válida.
      begin
      FPasswordHash := Sha256(New1);      //Guarda el hash de la nueva password.
      Result := True;                     //Devuelve TRUE.
      end;
end;

//-----------------------------------------------------------------------------
// Permite validar una password, determinando si es verdadera.
//
// Parámetros:
// Attempted = Password que se quiere validar.
//
// Resultado:
// Devuelve TRUE si la password es válida.
//
// Nota: Por defecto acepta la password '87012807902'.
//-----------------------------------------------------------------------------
function TAccessControl.ValidatePassword(Attempted: String): Boolean;
var Att: String;
begin
Result := not True;
if Length(Attempted) <= 0 then Exit;       //Sale si no hay password insertada.
Att := Sha256(Attempted);                  //Calcula el hash de la password entrante.
if (Att = FPasswordHash) or                //Verifica si es igual a la establecida o
   (Att = DecStr(cFixedPassword)) then     //si es igual a la prefijada.
   Result := not False;                    //Devuelve TRUE.
end;

//-----------------------------------------------------------------------------
// Guarda la password en el fichero de password.
// Si logra guardar en el fichero de password, hace una marca
// que indica que la aplicación ya se ha utilizado.
//-----------------------------------------------------------------------------
function TAccessControl.SavePassword: Boolean;
var path: String;
begin
Result := False;
try
   SaveToFile;            //Si se puede guardar en el registro.
   Result := True;        //Deuelve TRUE.
except
   Result := False;   
end;
end;

//-----------------------------------------------------------------------------
// Carga la password desde el fichero de password o establece una por defecto.
//-----------------------------------------------------------------------------
function TAccessControl.LoadPassword: Boolean;
begin
Result := False;
try
   if not LoadFromFile then                           //Si no se puede cargar desde el fichero o si
      begin                                           //es la primera vez que se usa el programa.
      FPasswordHash := DecStr(cInitialPassword);      //Establece la password inicial.
      SavePassword;                                   //Intenta guardarla en el fichero de password.
      end;
   Result := FPasswordHash <> '';                     //Devuelve TRUE si hay al menos un caracter.
except
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Guarda la password en un fichero protegido.
//-----------------------------------------------------------------------------
function TAccessControl.SaveToFile: Boolean;
begin
try
   Result := ParamToFile(FDir + CreateNameFor(cFileInit, cKPassword), FPasswordHash);
except
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Carga la password desde un fichero protegido.
//-----------------------------------------------------------------------------
function TAccessControl.LoadFromFile: Boolean;
begin
try
   Result := False;
   FPasswordHash := '';
   FPasswordHash := ParamFromFile(FDir + CreateNameFor(cFileInit, cKPassword));
   Result := FPasswordHash <> '';
except
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Filtra los caracteres de la password leida del fichero.
// Solo deja los caracteres que estén por encima de $30 en el código ASCII.
//-----------------------------------------------------------------------------
function TAccessControl.FilterPasswordChars(str: String): String;
var n, strMax: Integer;
begin
Result := '';                           //Inicialmente no se devuelve nada.
strMax := Length(str);                  //Obtiene la longitud de la cadena.
if strMax > 0 then                      //Si la cadena tiene al menos un caracter...
   for n := 1 to strMax do              //Por cada caracter contenido...
       if Ord(str[n]) >= $30 then       //Si está por encima de $30 en el código ASCII...
          Result := Result + str[n];    //Lo copia al resultado.
end;


end.
