
///////////////////////////////////////////////////////////////////////////////
// Nombre: UDateID
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 03/10/2016
// Objetivo: Generar un ID a partir de la fecha actual del Sistema Operativo.
//-----------------------------------------------------------------------------
// Modo de uso:
//
// var id: TDateID;                   //Crea una variable para generador de ID.
// ...
// id := TDateID.Create;              //Crea una instancia del generador de ID.
// id.GenerateID;                     //Genera el ID y se guarda en el objeto.
// ...
// Edit1.Text := id.GetIDStringHex;   //Obtiene el ID y lo muestra en un Edit.
//
///////////////////////////////////////////////////////////////////////////////

unit UDateID;

interface

uses Windows, USHA256, UAntiReversing;

//-----------------------------------------------------------------------------
// Tipo de dato que almacena el HASH del ID de fecha.
//-----------------------------------------------------------------------------

const cDateIDArrayMax = 31;         //Para un arreglo de 32 Bytes.
const cShortDateIDArrayMax = 7;     //Para un arreglo de 8 Bytes.

type TDateIDArray = array[0..cDateIDArrayMax] of Byte;             //256 bits
type TShortDateIDArray = array[0..cShortDateIDArrayMax] of Byte;   //64 bits

//-----------------------------------------------------------------------------
// Esta es la clase que implementa al generador de ID de fecha.
//-----------------------------------------------------------------------------
type
  TDateID = class
  private
    FDateIDRaw: String;

    procedure ClearData;
    function InternalGetIDStringHex: String;
  public
    constructor Create;
    destructor Destroy;
    function GenerateID: Boolean;
    function GetIDArray: TDateIDArray;
    function GetShortIDArray: TShortDateIDArray;
    function GetIDStringHex: String;
    function GetShortIDStringHex: String;
  end;

implementation

uses SysUtils, DateUtils;

//-----------------------------------------------------------------------------
// Inicia el objeto y prepara las variables.
//-----------------------------------------------------------------------------
constructor TDateID.Create;
begin
ClearData;
end;

//-----------------------------------------------------------------------------
// Borra los datos utilizados.
//-----------------------------------------------------------------------------
destructor TDateID.Destroy;
begin
ClearData;
end;

//-----------------------------------------------------------------------------
// Crea el ID de fecha.
//-----------------------------------------------------------------------------
function TDateID.GenerateID: Boolean;
begin
Result := False;
try
   FDateIDRaw := InternalGetIDStringHex;
finally
   Result := ReturnTrue(DateTimeToStr(Now), 4, 256);     //Devuelve TRUE.
end;
end;

//-----------------------------------------------------------------------------
// Devuelve el ID de fecha en un erreglo de 32 Bytes.
//-----------------------------------------------------------------------------
function TDateID.GetIDArray: TDateIDArray;
var Hash: TSHA256;
begin
Hash := TSha256.Create(nil);      //Crea el objeto HASH.
Hash.Init;                        //Inicializa el hash.
Hash.UpdateStr(FDateIDRaw);       //Genera el hash del ID crudo.
Hash.Final(Result);               //Salva el HASH del DateID en Digest.
Hash.Burn;                        //Elimina los datos de la memoria.
Hash.Free;                        //Destruye el objeto HASH.
end;

//-----------------------------------------------------------------------------
// Devuelve el ID de fecha corto en un erreglo de 8 Bytes.
//-----------------------------------------------------------------------------
function TDateID.GetShortIDArray: TShortDateIDArray;
var ID32: TDateIDArray;
    n: Integer;
begin
ID32 := GetIDArray;
for n := 0 to cShortDateIDArrayMax do
    Result[n] := ID32[n] xor ID32[n + 8] xor ID32[n + 16] xor ID32[n + 24];
end;

//-----------------------------------------------------------------------------
// Devuelve el hash del ID de fecha en una cadena de texto en exadecimal.
//-----------------------------------------------------------------------------
function TDateID.GetIDStringHex: String;
var hida: TDateIDArray;
    n: Integer;
begin
Result := '';                                     //Vacía la cadena inicialmente.
hida := GetIDArray;                               //Obtiene el id de fecha en un arreglo.
for n := 0 to cDateIDArrayMax do                  //Devuelve el arreglo como una cadena
    Result := Result + IntToHex(hida[n], 2);      //de texto en hexadecimal.
end;

//-----------------------------------------------------------------------------
// Devuelve el ID de fecha corto en una cadena de texto en exadecimal.
//-----------------------------------------------------------------------------
function TDateID.GetShortIDStringHex: String;
var hida: TShortDateIDArray;
    n: Integer;
begin
Result := '';                                     //Vacía la cadena inicialmente.
hida := GetShortIDArray;                          //Obtiene el hasdware id en un arreglo de 8 bits.
for n := 0 to cShortDateIDArrayMax do             //Devuelve el arreglo como una cadena
    Result := Result + IntToHex(hida[n], 2);      //de texto en hexadecimal.
end;

//-----------------------------------------------------------------------------
// Devuelve el ID de fecha crudo en una cadena de texto en exadecimal.
//-----------------------------------------------------------------------------
function TDateID.InternalGetIDStringHex: String;
var Date: TDateTime;
begin
Result := '';                                     //Inicia la cadena.
Date := Now;                                      //Obtiene la fecha actual.
Result := Result + IntToHex(YearOf(Date), 4) +    //Obtiene el año de la fecha actual.
          Result + IntToHex(MonthOf(Date), 4);    //Obtiene el mes de la fecha actual.
end;


//-----------------------------------------------------------------------------
// Borra el contenido de las variables internas.
//-----------------------------------------------------------------------------
procedure TDateID.ClearData;
begin
FDateIDRaw := '';
end;

end.

