
///////////////////////////////////////////////////////////////////////////////
// Nombre: UDataControl
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 23/07/2016
// Objetivo: Implementa el almacenamiento, filtrado y control de los datos.
///////////////////////////////////////////////////////////////////////////////

unit UStoreDataControl;

interface

uses
  SysUtils, Types, Classes, Variants, DB, DBClient, UAntiReversing,
  Math, Windows, DateUtils, UCommon, IniFiles;

//-----------------------------------------------------------------------------
// Campos que conforman la tabla de datos.
//-----------------------------------------------------------------------------
const fNumber = 'Número';               //Número identificador del evento.
const fUnit = 'Unidad';                 //Letra asignada por el sistema operativo.
const fName = 'Nombre';                 //Nombre del dispositivo.
const fType = 'Tipo';                   //Tipo de dispositivo.
const fSerial = 'Serial';               //Número de serie del dispositivo.
const fCapacityGB = 'Capacidad GB';     //Capacidad del dispositivo en GB.
const fConection = 'Conectado';         //Fecha y hora de la conexión.
const fDurationMin = 'Minutos';         //Duración de la conexión en minutos.
const fInitialGB = 'Inicio GB';         //Cantidad de información inicial contenida en el dispositivo.
const fMinimalGB = 'Minimo GB';         //Cantidad de información mínima que hubo en el dispositivo.
const fFinalGB = 'Final GB';            //Cantidad de información que finalmente obtiene el dispositivo.
const fCopyedGB = 'Copiados GB';        //Cantidad de información copiada hacia el dispositivo.

//-----------------------------------------------------------------------------
// Guarda información estadística sobre los dispositivos registrados en la DB.
//-----------------------------------------------------------------------------

type TArrayDays = Array[1..24]of Integer;
type TArrayHoursOfDays = Array[1..7]of TArrayDays;

type TStatistics = record
                   //Almacenan las cantidades totales.
                   CountTotalAllDevices: Integer;                 //Todos los tipos de dispositivos.
                   CountTotalRemovable: Integer;                  //Dispositivos extraibles (FLASH, External HDD).
                   CountTotalFixed: Integer;                      //Dispositivos internos (HDD).
                   CountTotalRemote: Integer;                     //Dispositivos remotos.
                   CountTotalCDROM: Integer;                      //CD-ROM, DVD-ROM, CD-Rewrite, DVD-Rewrite
                   CountTotalRAMDisk: Integer;                    //Discos virtuales.
                   CountTotalUnknow: Integer;                     //Dispositivo desconocido.

                   //Almacenan las cantidades de dispositivos
                   //hacia los cuales se copió información.
                   CountCopyedAllDevices: Integer;                //Todos los tipos de dispositivos.
                   CountCopyedRemovable: Integer;                 //Dispositivos extraibles (FLASH, External HDD).
                   CountCopyedFixed: Integer;                     //Dispositivos internos (HDD).
                   CountCopyedRemote: Integer;                    //Dispositivos remotos.
                   CountCopyedCDROM: Integer;                     //CD-ROM, DVD-ROM, CD-Rewrite, DVD-Rewrite
                   CountCopyedRAMDisk: Integer;                   //Discos virtuales.
                   CountCopyedUnknow: Integer;                    //Dispositivo desconocido.

                   //Cuenta cuanto se copió hacia
                   //cada tipo de dispositivo.
                   TotalCopyedAllDevices: Double;                //Todos los tipos de dispositivos.
                   TotalCopyedRemovable: Double;                 //Dispositivos extraibles (FLASH, External HDD).
                   TotalCopyedFixed: Double;                     //Dispositivos internos (HDD).
                   TotalCopyedRemote: Double;                    //Dispositivos remotos.
                   TotalCopyedCDROM: Double;                     //CD-ROM, DVD-ROM, CD-Rewrite, DVD-Rewrite
                   TotalCopyedRAMDisk: Double;                   //Discos virtuales.
                   TotalCopyedUnknow: Double;                    //Dispositivo desconocido.

                   //Para los acumulados por fecha y hora.
                   Hours: TArrayDays;                            //Un contador por cada hora del día.
                   Days: TArrayDays;                             //Un contador por cada día de la semana. Empieza por el domingo (Dom, Lun, Mart, Mier, Jue, Vier, Sab)
                   HoursPerDays: TArrayHoursOfDays;              //Un contador para cada hora, por cada día de la semana.
                   end;

//-----------------------------------------------------------------------------
// Tipos de eventos que genera este objeto.
//-----------------------------------------------------------------------------
type TSDCNotifyEventP = procedure(n: Integer; t: Integer) of object;
type TSDCNotifyLog = procedure(msg: String) of object;             

//-----------------------------------------------------------------------------
// Clase que implementa el controlador de datos.
//-----------------------------------------------------------------------------
type
  TDataControl = class 
    ClientDataSet: TClientDataSet;
  private
    FOpened: Boolean;
    FFileName: String;
    FLock: Boolean;
    FContinue: Boolean;

    //Para el filtrado de los datos.
    FFilterOfDate: String;
    FFilterOfCapcity: String;
    FFilterOfCopy: String;

    FFilterOfDateInitial: TDateTime;
    FFilterOfDateFinal: TDateTime;
    FFilterOfCapacityOp: String;
    FFilterOfCapacityGB: Integer;
    FFilterOfCopyOp: String;
    FFilterOfCopyGB: Integer;

    FSearchText: String;      //Cadena de texto que se busca.
    FCaseSensitive: Boolean;  //Sensibilidad a mayúsculas.
    FExact: Boolean;          //Exactitud.

    FOnProcess: TSDCNotifyEventP;
    FOnLog: TSDCNotifyLog;

    procedure AddField(pName: String; pType: TFieldType; pSize: Integer);
    function DesignTable: Boolean;
    function SaveTable(FileName: String): Boolean;
    function GetFileName: String;
    function SearchInString(Str1: String; SearchText: String; Sensitive: Boolean; Exact: Boolean):Boolean;
    procedure OnEventLog(msg: String);                //Lanza un evento OnLog.
  public
    constructor Create;
    destructor Destroy;
    function CreateDB(FileName: String): Boolean;
    function OpenDB(FileName: String): Boolean;
    function AddData(pUnit: String;               //Letra asignada por el sistema operativo.
                     pName: String;               //Nombre del dispositivo.
                     pType: String;               //Tipo de dispositivo.
                     pSerial: String;             //Número de serie del dispositivo.
                     pCapacity: Double;           //Capacidad del dispositivo en GB.
                     pConection: TDateTime;       //Fecha y hora de la conexión.
                     pDurationMin: Integer;       //Duración de la conexión en minutos.
                     pInitial: Double;            //Cantidad de información inicial contenida en el dispositivo.
                     pMinimal: Double;            //Cantidad de información mínima que hubo en el dispositivo.
                     pFinal: Double               //Cantidad de información que finalmente obtiene el dispositivo.
                     ): Boolean;
    function SaveToFile(Name: String; FType: TDataPacketFormat): Boolean;         //Guarda la base de datos completa en un fichero.
    function RestoreFromFile(Name: String; FType: TDataPacketFormat): Boolean;    //Sustituye la base de datos actual por otra que se carga desde un fichero.
    function ExportToTXT(Name: String): Boolean;                                  //Exporta el filtrado hacia un fichero TXT.
    function ExportToCSV(Name: String): Boolean;                                  //Exporta el filtrado hacia un fichero CSV.
    function ExportToINI(Name: String): Boolean;                                  //Exporta el filtrado hacia un fichero INI.
    function ExportToHTML(Name: String): Boolean;                                 //Exporta el filtrado hacia un fichero HTML.
    function ExportToXML(Name: String): Boolean;                                  //Exporta el filtrado hacia un fichero XML.
    function CloseDB: Boolean;
    function ClearDB: Boolean;                          //Borra todos los datos de la tabla.

    procedure CreateFilterOfDate(Initial: TDateTime;    //Fecha y hora inicial del rango.
                                 Final: TDateTime;      //Fecha y hora final del rango.
                                 Active: Boolean        //Determina la activación del filtro.
                                 );
    procedure CreateFilterOfCapacity(Op: String;        //Operador de la comparación.
                                     GB: Integer;       //Valor de comparación.
                                     Active: Boolean    //Determina la activación del filtro.
                                     );
    procedure CreateFilterOfCopy(Op: String;            //Operador de la comparación.
                                 GB: Integer;           //Valor de comparación.
                                 Active: Boolean        //Determina la activación del filtro.
                                 );
    procedure ApplyFilters;                             //Aplica todos los filtros creados.
    function GetDevicesTotal: Integer;
    function GetStatistic(var Statistics: TStatistics): Boolean;

    function SearchText(Text: String; CaseSensitive: Boolean; Exact: Boolean; var Fill: Integer): Boolean;
    function SearchNext(First: Boolean; var Fill: Integer): Boolean;
    procedure Traverse(IntervalMls: Integer; First: Boolean);

    property Opened: Boolean read FOpened;        //Indica si un fichero está abierto.
    property FileName: String read GetFileName;   //Devuelve el nombre de la base de datos activa.
    property Lock: Boolean read FLock;            //Indica si la base de datos está bloqueada.
    property Continue: Boolean read FContinue write FContinue;
    property OnProcess: TSDCNotifyEventP read FOnProcess write FOnProcess;
    property OnLog: TSDCNotifyLog read FOnLog write FOnLog;                          //Se dispara cuando se produce una información.
  end;

implementation

//-----------------------------------------------------------------------------
// Inicia las variables y datos del controlador.
//-----------------------------------------------------------------------------
constructor TDataControl.Create;
begin
try
   ClientDataSet := TClientDataSet.Create(nil);    //Crea el Dataset.
except
   OnEventLog('Error: TDataControl.Create/TClientDataSet');
end;
FOpened := False;                               //Indica que no se ha abierto ninguna DB.
FFileName := '';                                //Ningún fichero de DB abierto.

FFilterOfDate := '';
FFilterOfCapcity := '';
FFilterOfCopy := '';

FLock := False;
FContinue := False;
FSearchText := '';     //No hay ningún texto que buscar.
end;

//-----------------------------------------------------------------------------
// Libera las variables y datos del controlador.
//-----------------------------------------------------------------------------
destructor TDataControl.Destroy;
begin
if Assigned(ClientDataSet) then ClientDataSet.Free;
FFileName := '';
FFilterOfDate := '';
FFilterOfCapcity := '';
FFilterOfCopy := '';
end;

//-----------------------------------------------------------------------------
// Crea una tabla y la guarda en el fichero especificado, sin abrirla.
//-----------------------------------------------------------------------------
function TDataControl.CreateDB(FileName: String): Boolean;
begin
try
   Result := False;                        //Por defecto devuelve FALSE.
   if DesignTable then                     //Si se puede diseñar la tabla...
      if SaveTable(FileName) then          //Crea la tabla y si se pudo crear...
         Result := True;                   //Devuelve TRUE indicando éxito.
except
   OnEventLog('Error: TDataControl.CreateDB');
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Carga y abre una tabla desde un fichero de disco.
//-----------------------------------------------------------------------------
function TDataControl.OpenDB(FileName: String): Boolean;
begin
Result := False;                                //Por defecto devuelve FALSE.
If FileExists(FileName) then                    //Si el fichero existe...
   try
      ClientDataSet.Active := False;            //Desactiva...
      ClientDataSet.FileName := FileName;       //Indica la ruta y...
      ClientDataSet.Active := True;             //Activa nuevamente.
      Result := True;                           //Indica el éxito.
      FOpened := True;
      FFileName := EncStr(FileName);            //Guarda el nombre del fichero abierto.
   except
      OnEventLog('Error: TDataControl.OpenDB');
      Result := False;
   end;
end;

//-----------------------------------------------------------------------------
// Agrega un registro de datos a la tabla.
// Al terminar la ejecución de esta función, la base de datos queda
// abierta para que el resto del programa pueda seguir empleándola.
// Si la base de datos estaba cerrada, la abre. Internamente, antes
// de abandonar la función, se cierra la base de datos para forzar
// la escritura de los datos y luego se abre nuevamente.
//-----------------------------------------------------------------------------
function TDataControl.AddData(pUnit: String;             //Letra asignada por el sistema operativo.
                              pName: String;             //Nombre del dispositivo.
                              pType: String;             //Tipo de dispositivo.
                              pSerial: String;           //Número de serie del dispositivo.
                              pCapacity: Double;         //Capacidad del dispositivo en GB.
                              pConection: TDateTime;     //Fecha y hora de la conexión.
                              pDurationMin: Integer;     //Duración de la conexión en minutos.
                              pInitial: Double;          //Cantidad de información inicial contenida en el dispositivo.
                              pMinimal: Double;          //Cantidad de información mínima que hubo en el dispositivo.
                              pFinal: Double             //Cantidad de información que finalmente obtiene el dispositivo.
                              ): Boolean;
begin
Result := False;                //Por defecto devuelve FALSE.
if FLock then Exit;
if not FOpened then Exit;       //Sale si no se ha abierto un fichero de DB.
try
   with ClientDataSet do         //Comienza a agregar los
        begin                    //valores de cada campo.
        if not Active then Open; //Abre la base de datos.
        Append;                  //Agrega un registro al final de la tabla.

        FieldByName(fUnit).AsString := pUnit;
        FieldByName(fName).AsString := pName;
        FieldByName(fType).AsString := pType;
        FieldByName(fSerial).AsString := pSerial;
        FieldByName(fCapacityGB).AsFloat := RoundTo(pCapacity / cGByte, -4);
        FieldByName(fConection).AsDateTime := pConection;
        FieldByName(fDurationMin).AsInteger := pDurationMin;
        FieldByName(fInitialGB).AsFloat := RoundTo(pInitial / cGByte, -4);
        FieldByName(fMinimalGB).AsFloat := RoundTo(pMinimal / cGByte, -4);
        FieldByName(fFinalGB).AsFloat := RoundTo(pFinal / cGByte, -4);
        if pFinal - pMinimal > 0 then
           FieldByName(fCopyedGB).AsFloat := RoundTo((pFinal - pMinimal) / cGByte, -4)
        else
           FieldByName(fCopyedGB).AsFloat := 0;
        Post;                    //Guarda el registro y sus valores.
        Close;                   //Cuerra la base de datos para que se guarde el fichero.
        Open;                    //Abre la base de datos para continuar trabajando con ella.
        end;
   Result := True;               //Indica el éxito.
except                          //Si no ocurrió algún error...
   OnEventLog('Error: TDataControl.AddData');
   Result := False;              //Indica el error.
end;
end;

//-----------------------------------------------------------------------------
// Borra todos los datos de la tabla.
//-----------------------------------------------------------------------------
function TDataControl.ClearDB: Boolean;
var count, i: Integer;
begin
Result := False;                            //Por defecto devuelve FALSE.
if not FOpened then Exit;                   //Sale si no se ha abierto un fichero de DB.
try
   ClientDataSet.Filter := '';              //Quita el filtro de datos...
   ClientDataSet.Edit;                      //Activa la edición en el DataSet.
   count := ClientDataSet.RecordCount;      //Cuenta la cantidad de filas.
   if count > 0 then                        //Si hay filas...
      begin
      FContinue := True;                    //Activa el inicio del ciclo de búsqueda.
      ClientDataSet.First;                  //Se mueve al primer registro.
      i := 0;
      while FContinue do                    //Mientras esté activado el ciclo...
            begin
            Inc(i);
            ClientDataSet.Delete;           //Borra el registro actualmente seleccionado.

            if Assigned(FOnProcess) then    //Dispara un evento que informa del
               FOnProcess(i, count);        //estado del proceso.

            ClientDataSet.First;            //Se coloca en la primera posición.
            if ClientDataSet.Eof then       //Si es el final...
               begin
               Result := True;              //Devuelve TRUE y
               Break;                       //sale del ciclo.
               end;
            end;
      end;
except
   OnEventLog('Error: TDataControl.ClearDB');
   Result := False;                         //Indica el éxito.
end;
end;

//-----------------------------------------------------------------------------
// Cierra el fichero de la DB que contiene la tabla.
//-----------------------------------------------------------------------------
function TDataControl.CloseDB: Boolean;
begin
Result := False;
if FLock then Exit;
if not FOpened then Exit;       //Sale si no se ha abierto un fichero de DB.
try
   ClientDataSet.FileName := '';
   Result := True;
   FOpened := False;
   FFileName := '';
except
   OnEventLog('Error: TDataControl.CloseDB');
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Crea un filtro de fecha y hora para los datos.
//
// Parámetros:
// Initial = Fecha y hora inicial del rango.
// Final = Fecha y hora final del rango.
//-----------------------------------------------------------------------------
procedure TDataControl.CreateFilterOfDate(Initial: TDateTime; Final: TDateTime; Active: Boolean);
begin
FFilterOfDateInitial := Initial;    //Guarda el operador.
FFilterOfDateFinal := Final;        //Guarda el valor.

FFilterOfDate := '([' + fConection + ']>=''' + DateTimeToStr(Initial) + ''') and' +
                 '([' + fConection + ']<=''' + DateTimeToStr(Final)+ ''')';
if not Active then FFilterOfDate := '';
end;

//-----------------------------------------------------------------------------
// Crea un filtro de capacidad en GB para los datos.
//
// Parámetros:
// Op = Tipo de operador de comparación (>, <, >=, <=, =, <>).
// GB = Valor e comparación en Giga Bytes.
//-----------------------------------------------------------------------------
procedure TDataControl.CreateFilterOfCapacity(Op: String; GB: Integer; Active: Boolean);
begin
FFilterOfCapacityOp := Op;    //Guarda el operador.
FFilterOfCapacityGB := GB;    //Guarda el valor.
if Op <> '' then
   FFilterOfCapcity := '([' + fCapacityGB + ']' + Op + IntToStr(GB)+ ')';
if not Active then FFilterOfCapcity := '';
end;

//-----------------------------------------------------------------------------
// Crea un filtro de GB copiados para los datos.
//
// Parámetros:
// Op = Tipo de operador de comparación (>, <, >=, <=, =, <>).
// GB = Valor e comparación en Giga Bytes.
//-----------------------------------------------------------------------------
procedure TDataControl.CreateFilterOfCopy(Op: String; GB: Integer; Active: Boolean);
begin
FFilterOfCopyOp := Op;        //Guarda el operador.
FFilterOfCopyGB := GB;        //Guarda el valor.
if Op <> '' then
   FFilterOfCopy := '([' + fCopyedGB + ']' + Op + IntToStr(GB)+ ')';
if not Active then FFilterOfCopy := '';
end;

//-----------------------------------------------------------------------------
// Aplica todos los filtros creados.
//-----------------------------------------------------------------------------
procedure TDataControl.ApplyFilters;
var fil: String;
begin
if FLock then Exit;
fil := '';

//Agrega el filtro por fecha.
if FFilterOfDate <> '' then fil := FFilterOfDate;

//Agrega el filtro por capacidad.
if FFilterOfCapcity <> '' then
   if fil <> '' then
      fil := fil + 'and' + FFilterOfCapcity
   else
      fil := FFilterOfCapcity;

//Agrega el filtro por cantidad de copia.
if FFilterOfCopy <> '' then
   if fil <> '' then
      fil := fil + 'and' + FFilterOfCopy
   else
      fil := FFilterOfCopy;

ClientDataSet.Filter := fil;
ClientDataSet.Filtered := True;
end;

//-----------------------------------------------------------------------------
// Agrega campos a la tabla.
//-----------------------------------------------------------------------------
procedure TDataControl.AddField(pName: String; pType: TFieldType; pSize: Integer);
var FieldDef: TFieldDef;
begin
try
   ClientDataSet.Active := False;                    //Deasactiva el DataSet.
   FieldDef := ClientDataSet.FieldDefs.AddFieldDef;
   if Assigned(FieldDef) then
      begin
      FieldDef.Name := pName;                        //Establece el nombre.
      FieldDef.DataType := pType;                    //Establece el tipo de dato.
      FieldDef.Size := pSize;                        //Establece el tamaño del campo.
      end;
   ClientDataSet.CreateDataSet;                      //Crea el campo.
   ClientDataSet.Active := True;                     //Activa nuevamente el DataSet.
except
   OnEventLog('Error: TDataControl.AddField');
end;
end;

//-----------------------------------------------------------------------------
// Diseña una tabla para guardar los datos.
//-----------------------------------------------------------------------------
function TDataControl.DesignTable: Boolean;
begin
try
   AddField(fNumber, ftAutoInc, 0);         //Número identificador del evento.
   AddField(fUnit, ftString, 1);            //Letra asignada por el sistema operativo.
   AddField(fName, ftString, 20);           //Nombre del dispositivo.
   AddField(fType, ftString, 20);           //Tipo de dispositivo.
   AddField(fSerial, ftString, 20);         //Número de serie del dispositivo.
   AddField(fCapacityGB, ftFloat, 0);       //Capacidad del dispositivo en GB.

   AddField(fConection, ftDateTime, 0);     //Fecha y hora de la conexión.
   AddField(fDurationMin, ftInteger, 0);    //Duración de la conexión en minutos.
   AddField(fCopyedGB, ftFloat, 0);         //Cantidad de información copiada hacia el dispositivo.
   AddField(fInitialGB, ftFloat, 0);        //Cantidad de información inicial contenida en el dispositivo.
   AddField(fMinimalGB, ftFloat, 0);        //Cantidad de información mínima que hubo en el dispositivo.
   AddField(fFinalGB, ftFloat, 0);          //Cantidad de información que finalmente obtiene el dispositivo.

   Result := True;                          //Devuelve TRUE si todo salió OK.
except
   OnEventLog('Error: TDataControl.DesignTable');
   Result := False;                         //Devuelve FALSE ocurrió un error.
end;
end;

//-----------------------------------------------------------------------------
// Guarda la tabla actual del DataSet.
//-----------------------------------------------------------------------------
function TDataControl.SaveTable(FileName: String): Boolean;
begin
Result := False;                                      //Por defecto devuelve FALSE.
If not FileExists(FileName) then                      //Si el fichero no existe,
   try
      Clientdataset.SaveToFile(FileName, dfBinary);   //Guarda la tabla en un fichero.
      Result := True;                                 //Devuelve TRUE.
   except
      OnEventLog('Error: TDataControl.SaveTable');
      Result := False;                                //Devuelve FALSE indicando el error.
   end;
end;

//-----------------------------------------------------------------------------
// Devuelve el nombre del fichero de la DB.
//-----------------------------------------------------------------------------
function TDataControl.GetFileName: String;
begin
Result := DecStr(FFileName);
end;

//-----------------------------------------------------------------------------
// Guarda la base de datos (la tabla) en un fichero.
//-----------------------------------------------------------------------------
function TDataControl.SaveToFile(Name: String; FType: TDataPacketFormat): Boolean;
begin
try
   Clientdataset.SaveToFile(Name, FType);   //Guarda la tabla en un fichero.
   Result := True;                          //Devuelve TRUE.
except
   OnEventLog('Error: TDataControl.SaveToFile');
   Result := False                          //Devuelve FALSE indicando el error.
end;
end;

//-----------------------------------------------------------------------------
// Restaura la base de datos (la tabla) desde una salva en un fichero.
//-----------------------------------------------------------------------------
function TDataControl.RestoreFromFile(Name: String; FType: TDataPacketFormat): Boolean;
begin
try
   FLock := True;                                            //Bloquea el acceso a los datos.
   CopyFile(PChar(Name), PChar(DecStr(FFileName)), False);   //Sustituye el fichero actual de la base de datos.
   OpenDB(DecStr(FFileName));
   Result := True;                                           //Devuelve TRUE.
except
   OnEventLog('Error: TDataControl.RestoreFromFile');
   Result := False;                                          //Devuelve FALSE indicando el error.
end;
FLock := False;                                              //Habilita el acceso a los datos.
end;

//-----------------------------------------------------------------------------
// Devuelve la cantidad de registros actual en el DataSet.
// Este resultado es afectado por el filtro que esté aplicado en el momento.
//-----------------------------------------------------------------------------
function TDataControl.GetDevicesTotal: Integer;
begin
Result := 0;
if FLock then Exit;
try
   Result := ClientDataSet.RecordCount;
except
   Result := 0;
end;
end;

//-----------------------------------------------------------------------------
// Devuelve algunos datos destadísticos de los dispositivos.
// Esta función es lenta y debe ser llamada cuando existan
// pocos registros, o para un rango corto de registros.
//-----------------------------------------------------------------------------
function TDataControl.GetStatistic(var Statistics: TStatistics): Boolean;
var Count, n, m, d, i, j, c: Integer;
    ad: TArrayDays;
    v1: Double;
    v2: TDateTime;
    v3, v4, ext: String;
    f: TStrings;
begin
Result := False;
if FLock then Exit;
try
   count := ClientDataSet.RecordCount;
except
   Count := 0;
end;
if count > 0 then
   begin
   //Inicia los contadores y acumuladores.
   Statistics.CountTotalAllDevices := 0;
   Statistics.CountTotalRemovable := 0;
   Statistics.CountTotalFixed := 0;
   Statistics.CountTotalRemote := 0;
   Statistics.CountTotalCDROM := 0;
   Statistics.CountTotalRAMDisk := 0;
   Statistics.CountTotalUnknow := 0;

   Statistics.CountCopyedAllDevices := 0;
   Statistics.CountCopyedRemovable := 0;
   Statistics.CountCopyedFixed := 0;
   Statistics.CountCopyedRemote := 0;
   Statistics.CountCopyedCDROM := 0;
   Statistics.CountCopyedRAMDisk := 0;
   Statistics.CountCopyedUnknow := 0;

   Statistics.TotalCopyedAllDevices := 0;
   Statistics.TotalCopyedRemovable := 0;
   Statistics.TotalCopyedFixed := 0;
   Statistics.TotalCopyedRemote := 0;
   Statistics.TotalCopyedCDROM := 0;
   Statistics.TotalCopyedRAMDisk := 0;
   Statistics.TotalCopyedUnknow := 0;

   for n := 1 to 24 do Statistics.Hours[n] := 0;
   for n := 1 to 7 do Statistics.Days[n] := 0;
   for n := 1 to 7 do
       for m := 1 to 24 do
           Statistics.HoursPerDays[n, m] := 0;

   //Prepara el ciclo de búsqueda y cálculo.
   try
      FContinue := True;                                               //Activa el inicio del ciclo de búsqueda.
      ClientDataSet.First;                                             //Se mueve al primer registro.
      i := 0;
      while FContinue do                                               //Mientras esté activado el ciclo...
            begin
            Inc(i);

            //Obtiene datos del dispositivo.
            v1 := ClientDataSet.FieldByName(fCopyedGB).AsFloat;        //Obtiene los GBytes copiados.
            v2 := ClientDataSet.FieldByName(fConection).AsDateTime;    //Obtiene el tiempo de la conexión.
            v3 := ClientDataSet.FieldByName(fType).AsString;           //Obtiene el tipo de dispositivo.

            //Contabiliza los totales de cualquier tipo de dispositivo.
            Inc(Statistics.CountTotalAllDevices);
            if v1 > 0 then Inc(Statistics.CountCopyedAllDevices);
            with Statistics do TotalCopyedAllDevices := TotalCopyedAllDevices + v1;

            //Contabiliza el tipo de dispositivo.
            if AnsiCompareText(v3, 'Extraible') = 0 then                 
               begin
               Inc(Statistics.CountTotalRemovable);
               if v1 > 0 then Inc(Statistics.CountCopyedRemovable);
               with Statistics do TotalCopyedRemovable := TotalCopyedRemovable + v1;
               end
            else
               if AnsiCompareText(v3, 'Fijo') = 0 then                        
                  begin
                  Inc(Statistics.CountTotalFixed);
                  if v1 > 0 then Inc(Statistics.CountCopyedFixed);
                  with Statistics do TotalCopyedFixed := TotalCopyedFixed + v1;
                  end
               else
                  if AnsiCompareText(v3, 'Unidad de red') = 0 then   
                     begin
                     Inc(Statistics.CountTotalRemote);
                     if v1 > 0 then Inc(Statistics.CountCopyedRemote);
                     with Statistics do TotalCopyedRemote := TotalCopyedRemote + v1;
                     end
                  else
                     if AnsiCompareText(v3, 'CD-ROM') = 0 then
                        begin
                        Inc(Statistics.CountTotalCDROM);
                        if v1 > 0 then Inc(Statistics.CountCopyedCDROM);
                        with Statistics do TotalCopyedCDROM := TotalCopyedCDROM + v1;
                        end
                     else
                        if AnsiCompareText(v3, 'Disco-RAM') = 0 then        
                           begin
                           Inc(Statistics.CountTotalRAMDisk);
                           if v1 > 0 then Inc(Statistics.CountCopyedRAMDisk);
                           with Statistics do TotalCopyedRAMDisk := TotalCopyedRAMDisk + v1;
                           end
                        else
                           begin                                          //Dispositivos desconocidos.
                           Inc(Statistics.CountTotalUnknow);
                           if v1 > 0 then Inc(Statistics.CountCopyedUnknow);
                           with Statistics do TotalCopyedUnknow := TotalCopyedUnknow + v1;
                           end;

            //Llena los acumulados de horas y días.
            Inc(Statistics.Days[DayOfWeek(v2)]);
            Inc(Statistics.Hours[HourOf(v2) + 1]);
            Inc(Statistics.HoursPerDays[DayOfWeek(v2), HourOf(v2) + 1]);

            //Dispara un evento que informa del estado del proceso
            if Assigned(FOnProcess) then FOnProcess(i, count);

            //Termina la operación o continúa si aún quedan registros.
            ClientDataSet.Next;
            if ClientDataSet.Eof then
               begin
               Result := True;
               Break;
               end;
            end;
   except
      OnEventLog('Error: TDataControl.GetStatistic');
      Result := False;
   end;

   //Pone los datos del domingo en la última posición del arreglo.
   d := Statistics.Days[1];
   for n := 1 to 6 do Statistics.Days[n] := Statistics.Days[n + 1];
   Statistics.Days[7] := d;

   ad := Statistics.HoursPerDays[1];
   for n := 1 to 6 do Statistics.HoursPerDays[n] := Statistics.HoursPerDays[n + 1];
   Statistics.HoursPerDays[7] := ad;
   end;
end;

//-----------------------------------------------------------------------------
// Exporta los registros de la base de datos a un fichero tipo TXT.
//-----------------------------------------------------------------------------
function TDataControl.ExportToTXT(Name: String): Boolean;
var Count, i: Integer;
    vNumber, vDuration: Integer;
    vCapacityGB, vInitialGB, vMinimalGB, vFinalGB, vCopyedGB: Double;
    vUnit, vName, vType, vSerial, vFiles: String;
    vConection: TDateTime;
    f: TStrings;
begin
Result := False;
if FLock then Exit;
count := ClientDataSet.RecordCount;
if count > 0 then
   try
      //Prepara el ciclo de búsqueda y escritura.
      FContinue := True;                                               //Activa el inicio del ciclo de búsqueda.
      ClientDataSet.First;                                             //Se mueve al primer registro.
      f := TStringList.Create;                                         //Crea un objeto para guardar el fichero.

      //Agrega el nombre del programa que hizo la exportación.
      f.Add('APLICACIÓN: ' + DecStr(cAppTitle));
      f.Add('VERSIÓN: ' + IntToStr(cAppVersion)+'.'+IntToStr(cAppSubVersion));

      //Agrega la fecha de la exportacion.
      f.Add('FECHA_HORA: ' + DateTimeToStr(Now));

      //Agrega el usuario de windows.
      f.Add('USUARIO: ' + GetWindowsUser);
      f.Add('');

      //Muestra los filtros establecidos.
      f.Add('FILTROS');
      if FFilterOfDate <> '' then
         f.Add(DecStr('7!6b;o7hHpD!;e6fI!OuOj3f3n2q4pQ;QS@') + DateToStr(FFilterOfDateInitial) + '-' + DateToStr(FFilterOfDateFinal));   //'Rango de tiempo: '
      if FFilterOfCapcity <> '' then
         f.Add(DecStr('I!BbOqDb>dRjRe5b3e2;2D8') + FFilterOfCapacityOp + ' ' + ToXByte(FFilterOfCapacityGB));   //'Capacidad: '
      if FFilterOfCopy <> '' then
         f.Add(DecStr('L!9oGg:p?sSn=b=dKjMôUo=!Dd<pPqOjGbNeAbF;IJT') + FFilterOfCopyOp + ' ' + ToXByte(FFilterOfCopyGB));   //'Información copiada: '
      f.Add('');

      f.Add('DATOS');
      i := 0;
      while FContinue do                                               //Mientras esté activado el ciclo...
            begin
            Inc(i);

            //Obtiene datos del dispositivo.
            vNumber := ClientDataSet.FieldByName(fNumber).AsInteger;
            vUnit := ClientDataSet.FieldByName(fUnit).AsString;
            vName := ClientDataSet.FieldByName(fName).AsString;
            vType := ClientDataSet.FieldByName(fType).AsString;
            vSerial := ClientDataSet.FieldByName(fSerial).AsString;
            vConection := ClientDataSet.FieldByName(fConection).AsDateTime;
            vDuration := ClientDataSet.FieldByName(fDurationMin).AsInteger;
            vCapacityGB := ClientDataSet.FieldByName(fCapacityGB).AsFloat;
            vCopyedGB := ClientDataSet.FieldByName(fCopyedGB).AsFloat;
            vInitialGB := ClientDataSet.FieldByName(fInitialGB).AsFloat;
            vMinimalGB := ClientDataSet.FieldByName(fMinimalGB).AsFloat;
            vFinalGB := ClientDataSet.FieldByName(fFinalGB).AsFloat;

            //Aquí es donde se escribe el fichero de salida.
            f.Add('----------------------------------------');
            f.Add(DecStr('P!=ûMnSfNs<p6!=eCf:!Rf;wDf6oTu<pP;YOD') + IntToStr(vNumber));                                         //'Número de evento: '
            f.Add(DecStr('U!Ro0j5e6bEe0!Hb0tPjKhPoMbGe8bL;HVX') + UpperCase(vUnit) + ':\');                                     //'Unidad asignada: '
            f.Add(DecStr('K!Yp:nLc<s3fP!DeUfEmH!XeLj>t0qMpYtPjOu8j6wVpP;HOT') + vName);                                         //'Nombre del dispositivo: '
            f.Add(DecStr('L!<j3qJp;!Te6f;!FeXj7t6q;p7tHjDu;j6wIpO;OU3') + vType);                                               //'Tipo de dispositivo: '
            f.Add(DecStr('3!2û4nQfQs@pI!BeOfD!>tRfRs5j3f2;2O8') + vSerial);                                                     //'Número de serie: '
            f.Add(DecStr('L!9bGq:b?dSj=e=bKeM!Ue=fD!<bPmOnGbNdAfFoIbTnLj4fCo@u4p2;GDL') + ToXByte(vCapacityGB));                //'Capacidad de almacenamiento: '
            f.Add(DecStr('3!VoSjLdVj;pM!NeDfK!6m9bW!?d5pYo<f2y4jTôGoM;4JL') + DateTimeToStr(vConection));                       //'Inicio de la conexión: '
            f.Add(DecStr('0!=vUsFb=dBjUô>o>!De3fS!Om7bV!Dd5pUo@f8yQjHô4oJ;VEF') + ToXTime(vDuration));                          //'Duración de la conexión: '
            f.Add(DecStr('=!Bo9g7pLsYnJbMdFj4ôOo6!9dTpMqBjYb6e1b7!0b?m@!?e3jCtEqCpLtRjGu:j?wHpT;IJ:') + ToXByte(vCopyedGB));    //'Información copiada al dispositivo: '
            f.Add(DecStr('M!GpTo?u?fJoPjCe8pM!SbOmU!7jPo;j<dMjCpX!7e0f9!Um=bT!PdXp;o@fSy0j:ô2oE;AD0') + ToXByte(vInitialGB));   //'Contenido al inicio de la conexión: '
            f.Add(DecStr('6!Mp=o@uGf:o6j<e3pM!=nQîKoGjUn0p:!UrLv1f6!6mAmAf6h0ôF!PbG!9uTf2oDf3s?;KD8') + ToXByte(vMinimalGB));   //'Contenido mínimo que llegó a tener: '
            f.Add(DecStr('Q!EpAo1uYf6o1j2e4pQ!Xb4m8!PgYjDo1bHmQ!VeEfK!YmPbN!UdYpNoPf0y?j=ô3oP;:DS') + ToXByte(vFinalGB));       //'Contenido al final de la conexión: '
            f.Add(DecStr('0;1jTd8i;fLs=p6t?!Ad3pCq2jSb2e<pVt?GP'));       //'Ficheros copiados:'
            f.Add(vFiles);

            //Dispara un evento que informa del estado del proceso
            if Assigned(FOnProcess) then FOnProcess(i, Count);

            //Termina la operación o continúa si aún quedan registros.
            ClientDataSet.Next;
            if ClientDataSet.Eof then
               begin
               f.SaveToFile(Name);     //Guarda el fichero.
               Result := True;         //Indica el éxito de la operación.
               Break;                  //Sale del ciclo y de la función.
               end;
            end;
   except
      OnEventLog('Error: TDataControl.ExportToTXT');
      Result := False;
   end;
end;

//-----------------------------------------------------------------------------
// Exporta los registros de la base de datos a un fichero tipo CSV.
//-----------------------------------------------------------------------------
function TDataControl.ExportToCSV(Name: String): Boolean;
const cSep = ';';
const cStrLim = '"';
var Count, i: Integer;
    vNumber, vDuration: Integer;
    vCapacityGB, vInitialGB, vMinimalGB, vFinalGB, vCopyedGB: Double;
    vUnit, vName, vType, vSerial, vFiles: String;
    vConection: TDateTime;
    f: TStrings;
    s: String;
begin
Result := False;
if FLock then Exit;
count := ClientDataSet.RecordCount;
if count > 0 then
   try
      //Prepara el ciclo de búsqueda y escritura.
      FContinue := True;                                               //Activa el inicio del ciclo de búsqueda.
      ClientDataSet.First;                                             //Se mueve al primer registro.
      f := TStringList.Create;                                         //Crea un objeto para guardar el fichero.

      //Agrega el nombre del programa que hizo la exportación.
      f.Add('APLICACIÓN' + cSep + DecStr(cAppTitle));
      f.Add('VERSIÓN' + cSep + IntToStr(cAppVersion)+'.'+IntToStr(cAppSubVersion));

      //Agrega la fecha de la exportacion.
      f.Add('FECHA' + cSep + DateTimeToStr(Now));

      //Agrega el usuario de windows.
      f.Add('USUARIO' + cSep + GetWindowsUser);
      f.Add(cSep + cSep);

      f.Add('FILTROS');
      //Muestra los filtros establecidos.
      if FFilterOfDate <> '' then
         begin
         f.Add('Desde' + cSep + DateToStr(FFilterOfDateInitial));
         f.Add('Hasta' + cSep + DateToStr(FFilterOfDateFinal));
         end;
      if FFilterOfCapcity <> '' then
         f.Add('Capacidad' + cSep + FFilterOfCapacityOp + ' ' + ToXByte(FFilterOfCapacityGB));
      if FFilterOfCopy <> '' then
         f.Add('Información copiada' + cSep + FFilterOfCopyOp + ' ' + ToXByte(FFilterOfCopyGB));
      f.Add(cSep + cSep);

      f.Add('DATOS');
      s := '';
      s := s + cStrLim + fNumber + cStrLim + cSep;
      s := s + cStrLim + fUnit + cStrLim + cSep;
      s := s + cStrLim + fName + cStrLim + cSep;
      s := s + cStrLim + fType + cStrLim + cSep;
      s := s + cStrLim + fSerial + cStrLim + cSep;
      s := s + cStrLim + fCapacityGB + cStrLim + cSep;
      s := s + cStrLim + fConection + cStrLim + cSep;
      s := s + cStrLim + fDurationMin + cStrLim + cSep;
      s := s + cStrLim + fCopyedGB + cStrLim + cSep;
      s := s + cStrLim + fInitialGB + cStrLim + cSep;
      s := s + cStrLim + fMinimalGB + cStrLim + cSep;
      s := s + cStrLim + fFinalGB + cStrLim;
      f.Add(s);

      i := 0;
      while FContinue do                                               //Mientras esté activado el ciclo...
            begin
            Inc(i);

            //Obtiene datos del dispositivo.
            vNumber := ClientDataSet.FieldByName(fNumber).AsInteger;
            vUnit := ClientDataSet.FieldByName(fUnit).AsString;
            vName := ClientDataSet.FieldByName(fName).AsString;
            vType := ClientDataSet.FieldByName(fType).AsString;
            vSerial := ClientDataSet.FieldByName(fSerial).AsString;
            vConection := ClientDataSet.FieldByName(fConection).AsDateTime;
            vDuration := ClientDataSet.FieldByName(fDurationMin).AsInteger;
            vCapacityGB := ClientDataSet.FieldByName(fCapacityGB).AsFloat;
            vCopyedGB := ClientDataSet.FieldByName(fCopyedGB).AsFloat;
            vInitialGB := ClientDataSet.FieldByName(fInitialGB).AsFloat;
            vMinimalGB := ClientDataSet.FieldByName(fMinimalGB).AsFloat;
            vFinalGB := ClientDataSet.FieldByName(fFinalGB).AsFloat;

            //Aquí es donde se escribe el fichero de salida.
            s := IntToStr(vNumber) + cSep;
            s := s + cStrLim + UpperCase(vUnit) + cStrLim + cSep;
            s := s + cStrLim + vName + cStrLim + cSep;
            s := s + cStrLim + vType + cStrLim + cSep;
            s := s + cStrLim + vSerial + cStrLim + cSep;
            s := s + FloatToStrF(vCapacityGB, ffFixed, 16, 2) + cSep;
            s := s + DateTimeToStr(vConection) + cSep;
            s := s + IntToStr(vDuration) + cSep;
            s := s + FloatToStrF(vCopyedGB, ffFixed, 16, 2) + cSep;
            s := s + FloatToStrF(vInitialGB, ffFixed, 16, 2) + cSep;
            s := s + FloatToStrF(vMinimalGB, ffFixed, 16, 2) + cSep;
            s := s + FloatToStrF(vFinalGB, ffFixed, 16, 2) + cSep;
            s := s + cStrLim + vFiles + cStrLim;
            f.Add(s);

            //Dispara un evento que informa del estado del proceso
            if Assigned(FOnProcess) then FOnProcess(i, Count);

            //Termina la operación o continúa si aún quedan registros.
            ClientDataSet.Next;
            if ClientDataSet.Eof then
               begin
               f.SaveToFile(Name);     //Guarda el fichero.
               Result := True;         //Indica el éxito de la operación.
               Break;                  //Sale del ciclo y de la función.
               end;
            end;
   except
      OnEventLog('Error: TDataControl.ExportToCSV');
      Result := False;
   end;
end;

//-----------------------------------------------------------------------------
// Exporta los registros de la base de datos a un fichero tipo INI.
//-----------------------------------------------------------------------------
function TDataControl.ExportToINI(Name: String): Boolean;
var Count, i, m: Integer;
    INI: TIniFile;

    //Valores de los campos.
    vNumber, vDuration: Integer;
    vCapacityGB, vInitialGB, vMinimalGB, vFinalGB, vCopyedGB: Double;
    vUnit, vName, vType, vSerial, vFiles: String;
    vConection: TDateTime;

    //Nombres de los campos.
    nNumber, nDuration, nCapacityGB, nInitialGB: String;
    nMinimalGB, nFinalGB, nCopyedGB, nUnit: String;
    nName, nType, nSerial, nFiles, nConection: String;

    fl: TStrings;
    s: String;
begin
Result := False;
if FLock then Exit;
count := ClientDataSet.RecordCount;
if count > 0 then
   try
      //Prepara el ciclo de búsqueda y escritura.
      FContinue := True;                                       //Activa el inicio del ciclo de búsqueda.
      ClientDataSet.First;                                     //Se mueve al primer registro.
      INI := TIniFile.Create(Name);                            //Crea un objeto para guardar el fichero.

      s := 'GENERAL';
      INI.WriteString(s, 'APLICACION', DecStr(cAppTitle));     //Nombre del programa.
      INI.WriteString(s, 'VERSION', IntToStr(cAppVersion)+'.'+IntToStr(cAppSubVersion));   //Versión del programa.
      INI.WriteDateTime(s, 'FECHA_HORA', Now);                 //Fecha y hora de la exportación.
      INI.WriteString(s, 'USUARIO', GetWindowsUser);           //Usuario de windows.

      //Muestra los filtros establecidos.
      if FFilterOfDate <> '' then
         begin
         s := 'FILTROS';
         INI.WriteDateTime(s, 'DESDE', FFilterOfDateInitial);
         INI.WriteDateTime(s, 'HASTA', FFilterOfDateFinal);
         end;
      if FFilterOfCapcity <> '' then
         INI.WriteString(s, 'CAPACIDAD', FFilterOfCapacityOp + ' ' + ToXByte(FFilterOfCapacityGB));
      if FFilterOfCopy <> '' then
         INI.WriteString(s, 'INFO_COPIADA', FFilterOfCopyOp + ' ' + ToXByte(FFilterOfCopyGB));

      //Crea los nombres de los campos.
      nNumber     := UpperCase(FilterXML(fNumber));
      nUnit       := UpperCase(FilterXML(fUnit));
      nName       := UpperCase(FilterXML(fName));
      nType       := UpperCase(FilterXML(fType));
      nSerial     := UpperCase(FilterXML(fSerial));
      nCapacityGB := UpperCase(FilterXML(fCapacityGB));
      nConection  := UpperCase(FilterXML(fConection));
      nDuration   := UpperCase(FilterXML(fDurationMin));
      nCopyedGB   := UpperCase(FilterXML(fCopyedGB));
      nInitialGB  := UpperCase(FilterXML(fInitialGB));
      nMinimalGB  := UpperCase(FilterXML(fMinimalGB));
      nFinalGB    := UpperCase(FilterXML(fFinalGB));

      INI.WriteInteger('DATOS', 'TOTAL_DATOS', GetDevicesTotal);

      i := 0;
      while FContinue do                                               //Mientras esté activado el ciclo...
            begin
            Inc(i);

            //Obtiene datos del dispositivo.
            vNumber     := ClientDataSet.FieldByName(fNumber).AsInteger;
            vUnit       := ClientDataSet.FieldByName(fUnit).AsString;
            vName       := ClientDataSet.FieldByName(fName).AsString;
            vType       := ClientDataSet.FieldByName(fType).AsString;
            vSerial     := ClientDataSet.FieldByName(fSerial).AsString;
            vConection  := ClientDataSet.FieldByName(fConection).AsDateTime;
            vDuration   := ClientDataSet.FieldByName(fDurationMin).AsInteger;
            vCapacityGB := ClientDataSet.FieldByName(fCapacityGB).AsFloat;
            vCopyedGB   := ClientDataSet.FieldByName(fCopyedGB).AsFloat;
            vInitialGB  := ClientDataSet.FieldByName(fInitialGB).AsFloat;
            vMinimalGB  := ClientDataSet.FieldByName(fMinimalGB).AsFloat;
            vFinalGB    := ClientDataSet.FieldByName(fFinalGB).AsFloat;

            //Aquí es donde se escriben los datos.
            s := 'DATO_' + IntToStr(i);
            INI.WriteInteger(s, nNumber, vNumber);
            INI.WriteString(s, nUnit, vUnit);
            INI.WriteString(s, nName, vName);
            INI.WriteString(s, nType, vType);
            INI.WriteString(s, nSerial, vSerial);
            INI.WriteFloat(s, nCapacityGB, vCapacityGB);
            INI.WriteDateTime(s, nConection, vConection);
            INI.WriteInteger(s, nDuration, vDuration);
            INI.WriteFloat(s, nCopyedGB, vCopyedGB);
            INI.WriteFloat(s, nInitialGB, vInitialGB);
            INI.WriteFloat(s, nMinimalGB, vMinimalGB);
            INI.WriteFloat(s, nFinalGB, vFinalGB);

            fl := TStringList.Create;                  //Crea una lista temporal de cadenas y...
            fl.Text := vFiles;                         //le asigna la lista de ficheros.
            INI.WriteFloat(s, 'TOTAL_' + nFiles, fl.Count);
            if fl.Count > 0 then                       //Si hay algún fichero en la lista...
               for m := 0 to fl.Count - 1 do
                   INI.WriteString(s, 'FICHERO_' + IntToStr(m+1), fl[m]);
            fl.Free;

            //Dispara un evento que informa del estado del proceso
            if Assigned(FOnProcess) then FOnProcess(i, Count);

            //Termina la operación o continúa si aún quedan registros.
            ClientDataSet.Next;
            if ClientDataSet.Eof then
               begin
               Result := True;         //Indica el éxito de la operación.
               Break;                  //Sale del ciclo y de la función.
               end;
            end;
   except
      OnEventLog('Error: TDataControl.ExportToINI');
      Result := False;
   end;
end;

//-----------------------------------------------------------------------------
// Exporta los registros de la base de datos a un fichero tipo HTML.
//-----------------------------------------------------------------------------
function TDataControl.ExportToHTML(Name: String): Boolean;
var Count, i, m: Integer;
    vNumber, vDuration: Integer;
    vCapacityGB, vInitialGB, vMinimalGB, vFinalGB, vCopyedGB: Double;
    vUnit, vName, vType, vSerial, vFiles: String;
    vConection: TDateTime;
    f, fl: TStrings;
begin
Result := False;
if FLock then Exit;
count := ClientDataSet.RecordCount;
if count > 0 then
   try
      //Prepara el ciclo de búsqueda y escritura.
      FContinue := True;                                               //Activa el inicio del ciclo de búsqueda.
      ClientDataSet.First;                                             //Se mueve al primer registro.
      f := TStringList.Create;                                         //Crea un objeto para guardar el fichero.

      f.Add('<HTML>');
      f.Add('<HEAD><TITLE>REGISTROS</TITLE></HEAD>');
      f.Add('<BODY bgcolor = DDDDFF>');

      //Agrega el nombre del programa que hizo la exportación.
      f.Add('<B>APLICACIÓN: </B>' + DecStr(cAppTitle) + '<BR>');
      f.Add('<B>VERSIÓN: </B>' + IntToStr(cAppVersion)+'.'+IntToStr(cAppSubVersion) + '<BR>');

      //Agrega la fecha de la exportación.
      f.Add('<B>FECHA Y HORA: </B>' + DateTimeToStr(Now) + '<BR>');

      //Agrega el usuario de windows.
      f.Add('<B>USUARIO: </B>' + GetWindowsUser + '<BR><BR>');

      //Muestra los filtros establecidos.
      f.Add('<B>FILTROS</B><BR>');
      if FFilterOfDate <> '' then
         f.Add('<B>' + DecStr('7!6b;o7hHpD!;e6fI!OuOj3f3n2q4pQ;QS@') + '</B>' + DateToStr(FFilterOfDateInitial) + '-' + DateToStr(FFilterOfDateFinal) + '<BR>');   //'Rango de tiempo: '
      if FFilterOfCapcity <> '' then
         f.Add('<B>' + DecStr('I!BbOqDb>dRjRe5b3e2;2D8') + '</B>' + FFilterOfCapacityOp + ' ' + ToXByte(FFilterOfCapacityGB) + '<BR>');   //'Capacidad: '
      if FFilterOfCopy <> '' then
         f.Add('<B>' + DecStr('L!9oGg:p?sSn=b=dKjMôUo=!Dd<pPqOjGbNeAbF;IJT') + '</B>' + FFilterOfCopyOp + ' ' + ToXByte(FFilterOfCopyGB) + '<BR>');   //'Información copiada: '
      f.Add('<BR>');

      f.Add('<B>ENCABEZADOS</B><BR>');
      f.Add('<B>' + fNumber + ': </B>' + DecStr('3/PûDnUfEsHpX!Le>f0!MfYwPfOo8u6pV!P)HsTfLh<j3tJu;sTp6*;OF') + '<BR>');                      //'Número de evento (registro).'
      f.Add('<B>' + fUnit + ': </B>' + DecStr('R/;oDj6eTb<eP!YbDtUjRh0o5b6eEb0!Hb0mP!KePjMtGq8pLtHjXuKjYw:pLV<') + '<BR>');                  //'Unidad asignada al dispositivo.'
      f.Add('<B>' + fName + ': </B>' + DecStr('X/7p6n;c7sHfD!;e6fImO!Oe3j3t2q4pQtQj@uIjBwOpDO>') + '<BR>');                                  //'Nombre del dispositivo.'
      f.Add('<B>' + fType + ': </B>' + DecStr('R/Rj5q3p2!2e8fL!9eGj:t?qSp=t=jKuMjUw=pDU<') + '<BR>');                                        //'Tipo de dispositivo.'
      f.Add('<B>' + fSerial + ': </B>' + DecStr('P/OûGnNfAsFpI!TeLf4!Ct@f4s2jGfL!3eVfSmL!Ve;jMtNqDpKt6j9uWj?w5pYO<') + '<BR>');              //'Número de serie del dispositivo.'
      f.Add('<B>' + fCapacityGB + ': </B>' + DecStr('Q/TbBq0b9dHjUe2bCeW!TeTfN!Yb8m6nKbRd7f0oRbGnCjBf6oQuTp4!I)QH9jCh@bE!McPz=u1fIt<*2DC') + '<BR>');                                        //'Capacidad de almacenamiento (Giga bytes).'
      f.Add('<B>' + fConection + ': </B>' + DecStr('9/3fKdSi>b<!CzV!NiAp3sQb:!1e>fU!TjBo:jAd?jKpB!JeOfR!CmFbI!<d:pAoNfKy7jXô1o=GU') + '<BR>');                                               //'Fecha y hora de inicio de la conexión.'
      f.Add('<B>' + fDurationMin + ': </B>' + DecStr('C/EvTsLbAdHj6ôXoC!6e=fI!RmDb5!Cd6p=oKfRySj6ô7oO!;f>o:!1nUjLo3vKu0pItHEP') + '<BR>');                                                   //'Duración de la conexión en minutos.'
      f.Add('<B>' + fCopyedGB + ': </B>' + DecStr('8/UbRoJuDjPeOb8e?!VeFfT!Nj<oHg4p>s>nAbHdVjLôNoY!?d3p@q;jVb9e@b3!FbEmF!9e8jAtXqEpHt?j?uQjVwLpH!F)WH9j0hYbD!Xc8zRu4fBt2*YD@') + '<BR>');    //'Cantidad de información copiada al dispositivo (Giga bytes).'
      f.Add('<B>' + fInitialGB + ': </B>' + DecStr('M/TpRo@uKf;oTj7e?pA!3bXm0!7j4o2jJd;jQp4!MeAfD!Em4bW!=dWp;oQfSyAjEô;oN!>)AHBjXh?bH!McDz7u=fAtD*>DC') + '<BR>');                           //'Contenido al inicio de la conexión (Giga bytes).'
      f.Add('<B>' + fMinimalGB + ': </B>' + DecStr('2/7pYoGuCfUoRjHeAp<!SnHî;oKjTn:pJ!1rCvRfT!9mYmLfMh;ôV!9bD!>uQfIo1fAs9!D)EH:jGhIb>!HcPzMu6fVtB*NDG') + '<BR>');                           //'Contenido mínimo que llegó a tener (Giga bytes).'
      f.Add('<B>' + fFinalGB + ': </B>' + DecStr('F/EpNoUuGfIoSjAe0pW!1bNmV!:gKjWoPb2m4!?eUf8!WmHbY!Qd@p1o0fIyQjCôEoO!W);H6j5hYb5!>c6zAuDf4t?*<D;') + '<BR>');                               //'Contenido al final de la conexión (Giga bytes).'
      f.Add('<BR>');

      f.Add('<B>DATOS</B><BR>');
      f.Add('<TABLE border = 1 bgcolor = FFFFFF bordercolor = 000000 cellspacing = 0>');
      f.Add('<TR bgcolor = AACCBB>');
      f.Add('<TD><B>' + fNumber + '</B></TD>');
      f.Add('<TD><B>' + fUnit + '</B></TD>');
      f.Add('<TD><B>' + fName + '</B></TD>');
      f.Add('<TD><B>' + fType + '</B></TD>');
      f.Add('<TD><B>' + fSerial + '</B></TD>');
      f.Add('<TD><B>' + fCapacityGB + '</B></TD>');
      f.Add('<TD><B>' + fConection + '</B></TD>');
      f.Add('<TD><B>' + fDurationMin + '</B></TD>');
      f.Add('<TD><B>' + fCopyedGB + '</B></TD>');
      f.Add('<TD><B>' + fInitialGB + '</B></TD>');
      f.Add('<TD><B>' + fMinimalGB + '</B></TD>');
      f.Add('<TD><B>' + fFinalGB + '</B></TD>');
      f.Add('</TR>');

      i := 0;
      while FContinue do                                               //Mientras esté activado el ciclo...
            begin
            Inc(i);

            //Obtiene datos del dispositivo.
            vNumber := ClientDataSet.FieldByName(fNumber).AsInteger;
            vUnit := ClientDataSet.FieldByName(fUnit).AsString;
            vName := ClientDataSet.FieldByName(fName).AsString;
            vType := ClientDataSet.FieldByName(fType).AsString;
            vSerial := ClientDataSet.FieldByName(fSerial).AsString;
            vConection := ClientDataSet.FieldByName(fConection).AsDateTime;
            vDuration := ClientDataSet.FieldByName(fDurationMin).AsInteger;
            vCapacityGB := ClientDataSet.FieldByName(fCapacityGB).AsFloat;
            vCopyedGB := ClientDataSet.FieldByName(fCopyedGB).AsFloat;
            vInitialGB := ClientDataSet.FieldByName(fInitialGB).AsFloat;
            vMinimalGB := ClientDataSet.FieldByName(fMinimalGB).AsFloat;
            vFinalGB := ClientDataSet.FieldByName(fFinalGB).AsFloat;

            //Aquí es donde se escribe el fichero de salida.
            f.Add('<TR>');
            f.Add('<TD>' + IntToStr(vNumber) + '</TD>');
            f.Add('<TD>' + UpperCase(vUnit) + '</TD>');
            f.Add('<TD>' + vName + '</TD>');
            f.Add('<TD>' + vType + '</TD>');
            f.Add('<TD>' + vSerial + '</TD>');
            f.Add('<TD>' + FloatToStrF(vCapacityGB, ffFixed, 16, 2) + '</TD>');
            f.Add('<TD>' + DateTimeToStr(vConection) + '</TD>');
            f.Add('<TD>' + IntToStr(vDuration) + '</TD>');
            f.Add('<TD>' + FloatToStrF(vCopyedGB, ffFixed, 16, 2) + '</TD>');
            f.Add('<TD>' + FloatToStrF(vInitialGB, ffFixed, 16, 2) + '</TD>');
            f.Add('<TD>' + FloatToStrF(vMinimalGB, ffFixed, 16, 2) + '</TD>');
            f.Add('<TD>' + FloatToStrF(vFinalGB, ffFixed, 16, 2) + '</TD>');

            f.Add('<TD>');
            fl := TStringList.Create;            //Crea una lista temporal de cadenas y...
            fl.Text := vFiles;                   //le asigna la lista de ficheros.
            if fl.Count > 0 then                 //Si hay algún fichero en la lista...
               for m := 0 to fl.Count - 1 do     //Pone el nombre de cada uno de
                   f.Add(fl[m] + '<BR>');        //los ficheros en la tabla.
            fl.Free;                             //Elimina la lista temporal.
            f.Add('</TD>');

            f.Add('</TR>');

            //Dispara un evento que informa del estado del proceso
            if Assigned(FOnProcess) then FOnProcess(i, Count);

            //Termina la operación o continúa si aún quedan registros.
            ClientDataSet.Next;
            if ClientDataSet.Eof then
               begin
               f.Add('</TABLE>');
               f.Add('</BODY>');
               f.Add('</HTML>');
               f.SaveToFile(Name);     //Guarda el fichero.
               Result := True;         //Indica el éxito de la operación.
               Break;                  //Sale del ciclo y de la función.
               end;
            end;
   except
      OnEventLog('Error: TDataControl.ExportToHTML');
      Result := False;
   end;
end;

//-----------------------------------------------------------------------------
// Exporta los registros de la base de datos a un fichero tipo XML.
//-----------------------------------------------------------------------------
function TDataControl.ExportToXML(Name: String): Boolean;
var Count, i, m: Integer;

    //Valores de los campos.
    vNumber, vDuration, vCapacityGB, vInitialGB: String;
    vMinimalGB, vFinalGB, vCopyedGB, vUnit: String;
    vName, vType, vSerial, vFiles, vConection: String;

    //Nombres de los campos.
    nNumber, nDuration, nCapacityGB, nInitialGB: String;
    nMinimalGB, nFinalGB, nCopyedGB, nUnit: String;
    nName, nType, nSerial, nFiles, nConection: String;

    f, fl: TStrings;
begin
Result := False;
if FLock then Exit;
count := ClientDataSet.RecordCount;
if count > 0 then
   try
      //Prepara el ciclo de búsqueda y escritura.
      FContinue := True;                                             //Activa el inicio del ciclo de búsqueda.
      ClientDataSet.First;                                           //Se mueve al primer registro.
      f := TStringList.Create;                                       //Crea un objeto para guardar el fichero.

      f.Add('<?xml version="1.0"?>');
      f.Add('<REGISTROS>');
      f.Add('<APLICACION>' + DecStr(cAppTitle) + '</APLICACION>');   //Nombre del programa.
      f.Add('<VERSION>' + IntToStr(cAppVersion)+'.'+IntToStr(cAppSubVersion) + '</APLICACION>');    //Versión del programa.
      f.Add('<FECHA_HORA>' + DateTimeToStr(Now) + '</FECHA_HORA>');                              //Fecha y hora de la exportación.
      f.Add('<USUARIO>' + GetWindowsUser + '</USUARIO>');                                        //Usuario de windows.

      //Muestra los filtros establecidos.
      f.Add('<FILTROS>');
      if FFilterOfDate <> '' then
         begin
         f.Add('<DESDE>' + DateToStr(FFilterOfDateInitial) + '</DESDE>');
         f.Add('<HASTA>' + DateToStr(FFilterOfDateFinal) + '</HASTA>');
         end;
      if FFilterOfCapcity <> '' then
         f.Add('<CAPACIDAD>' + FFilterOfCapacityOp + ' ' + ToXByte(FFilterOfCapacityGB)+ '</CAPACIDAD>');
      if FFilterOfCopy <> '' then
         f.Add('<INFO_COPIADA>' + FFilterOfCopyOp + ' ' + ToXByte(FFilterOfCopyGB) + '</INFO_COPIADA>');
      f.Add('</FILTROS>');

      //Crea los nombres de los campos.
      nNumber     := UpperCase(FilterXML(fNumber));
      nUnit       := UpperCase(FilterXML(fUnit));
      nName       := UpperCase(FilterXML(fName));
      nType       := UpperCase(FilterXML(fType));
      nSerial     := UpperCase(FilterXML(fSerial));
      nCapacityGB := UpperCase(FilterXML(fCapacityGB));
      nConection  := UpperCase(FilterXML(fConection));
      nDuration   := UpperCase(FilterXML(fDurationMin));
      nCopyedGB   := UpperCase(FilterXML(fCopyedGB));
      nInitialGB  := UpperCase(FilterXML(fInitialGB));
      nMinimalGB  := UpperCase(FilterXML(fMinimalGB));
      nFinalGB    := UpperCase(FilterXML(fFinalGB));

      f.Add('<TOTAL_DATOS>' + IntToStr(GetDevicesTotal) + '</TOTAL_DATOS>');
      f.Add('<DATOS>');

      i := 0;
      while FContinue do                                               //Mientras esté activado el ciclo...
            begin
            Inc(i);

            //Obtiene datos del dispositivo.
            vNumber     := ClientDataSet.FieldByName(fNumber).AsString;
            vUnit       := ClientDataSet.FieldByName(fUnit).AsString;
            vName       := ClientDataSet.FieldByName(fName).AsString;
            vType       := ClientDataSet.FieldByName(fType).AsString;
            vSerial     := ClientDataSet.FieldByName(fSerial).AsString;
            vConection  := ClientDataSet.FieldByName(fConection).AsString;
            vDuration   := ClientDataSet.FieldByName(fDurationMin).AsString;
            vCapacityGB := ClientDataSet.FieldByName(fCapacityGB).AsString;
            vCopyedGB   := ClientDataSet.FieldByName(fCopyedGB).AsString;
            vInitialGB  := ClientDataSet.FieldByName(fInitialGB).AsString;
            vMinimalGB  := ClientDataSet.FieldByName(fMinimalGB).AsString;
            vFinalGB    := ClientDataSet.FieldByName(fFinalGB).AsString;

            //Aquí es donde se escriben los datos.
            f.Add('<DATO_' + IntToStr(i) + '>');

            f.Add('<' + nNumber     + '>' + vNumber     + '</' + nNumber     + '>');
            f.Add('<' + nUnit       + '>"' + vUnit       + '"</' + nUnit       + '>');
            f.Add('<' + nName       + '>"' + vName       + '"</' + nName       + '>');
            f.Add('<' + nType       + '>"' + vType       + '"</' + nType       + '>');
            f.Add('<' + nSerial     + '>"' + vSerial     + '"</' + nSerial     + '>');
            f.Add('<' + nCapacityGB + '>' + vCapacityGB + '</' + nCapacityGB + '>');
            f.Add('<' + nConection  + '>' + vConection  + '</' + nConection  + '>');
            f.Add('<' + nDuration   + '>' + vDuration   + '</' + nDuration   + '>');
            f.Add('<' + nCopyedGB   + '>' + vCopyedGB   + '</' + nCopyedGB   + '>');
            f.Add('<' + nInitialGB  + '>' + vInitialGB  + '</' + nInitialGB  + '>');
            f.Add('<' + nMinimalGB  + '>' + vMinimalGB  + '</' + nMinimalGB  + '>');
            f.Add('<' + nFinalGB    + '>' + vFinalGB    + '</' + nFinalGB    + '>');

            fl := TStringList.Create;                  //Crea una lista temporal de cadenas y...
            fl.Text := vFiles;                         //le asigna la lista de ficheros.
            f.Add('<TOTAL_' + nFiles + '>' + IntToStr(fl.Count) + '</TOTAL_' + nFiles + '>');
            f.Add('<' + nFiles + '>');
            if fl.Count > 0 then                       //Si hay algún fichero en la lista...
               for m := 0 to fl.Count - 1 do
                   f.Add('<FICHERO_' + IntToStr(m+1) + '>"' + fl[m] + '"</FICHERO_' + IntToStr(m+1) + '>');
            fl.Free;
            f.Add('</' + nFiles + '>');

            f.Add('</DATO_' + IntToStr(i) + '>');

            //Dispara un evento que informa del estado del proceso
            if Assigned(FOnProcess) then FOnProcess(i, Count);

            //Termina la operación o continúa si aún quedan registros.
            ClientDataSet.Next;
            if ClientDataSet.Eof then
               begin
               f.Add('</DATOS>');
               f.Add('</REGISTROS>');
               f.SaveToFile(Name);     //Guarda el fichero.
               Result := True;         //Indica el éxito de la operación.
               Break;                  //Sale del ciclo y de la función.
               end;
            end;
   except
      OnEventLog('Error: TDataControl.ExportToXML');
      Result := False;
   end;
end;

//-----------------------------------------------------------------------------
// Inicia una búsqueda desde el inicio de la tabla de datos.
// Se detiene cuando encuentra una coincidencia.
//
// Entradas:
// Text = Texto que se desea buscar.
// CaseSensitive = TRUE indica que se deben tener en cuenta las
//                 diferencias entre mayúsculas y minúsculas.
// Exact = TRUE indica que las coincidencias debene ser exactas.
//         FALSE permite que la cadena buscada esté contenida
//         dentro de un texto más grande.
// Fill = Aquí se devuelve el número del campo donde se encontró
//        la coincidencia.
//
// Salidas: Devuelve TRUE si se encuentra una coincidencia.
//
// Nota: El cursor de datos se queda en la posición de la coincidencia.
//-----------------------------------------------------------------------------
function TDataControl.SearchText(Text: String; CaseSensitive: Boolean; Exact: Boolean; var Fill: Integer): Boolean;
begin
try
   FSearchText := Text;                //Guarda el texto que se debe buscar.
   FCaseSensitive := CaseSensitive;    //Guarda la configuración de la sensibilidad a mayúsculas y inúsculas.
   FExact := Exact;                    //Guarda la exactitud necesaria.
   ClientDataSet.First;                //Se para en la primera posición.
   Result := SearchNext(True, Fill);   //Inicia la búsqueda.
except
   OnEventLog('Error: TDataControl.SearchText');
   Result := False;
end;    
end;

//-----------------------------------------------------------------------------
// Realiza una búsqueda desde la posición actual del puntero de datos.
// Se detiene cuando encuentra una coincidencia.
//
// Entradas:
// First = TRUE indica que esta es la primera búsqueda de la cadena.
// Fill = Aquí se devuelve el número del campo donde se encontró
//        la coincidencia.
//
// Salidas: Devuelve TRUE si se encuentra una coincidencia.
//
// Nota: El cursor de datos se queda en la posición de la coincidencia.
//-----------------------------------------------------------------------------
function TDataControl.SearchNext(First: Boolean; var Fill: Integer): Boolean;
var count, n, i: Integer;
    vCapacityGB, vInitialGB, vMinimalGB, vFinalGB, vCopyedGB, vDuration: String;
    vUnit, vName, vType, vSerial, vConection, vNumber: String;
    vFiles: TStrings;
begin
Result := False;                              //Por defecto devuelve FALSE.
if not FOpened then Exit;                     //Sale si no se ha abierto un fichero de DB.
if FSearchText = '' then Exit;                //Sale si no hay texto que buscar..
try
   count := ClientDataSet.RecordCount;        //Cuenta la cantidad de filas.
   if count > 0 then                          //Si hay filas...
      begin
      FContinue := True;                      //Activa el inicio del ciclo de búsqueda.
      if not First then ClientDataSet.Next;   //Se mueve al siguiente registro.
      i := 0;
      while FContinue do                      //Mientras esté activado el ciclo...
            begin
            Inc(i);

            //Obtiene datos del dispositivo.
            vNumber := ClientDataSet.FieldByName(fNumber).AsString;
            vUnit := ClientDataSet.FieldByName(fUnit).AsString;
            vName := ClientDataSet.FieldByName(fName).AsString;
            vType := ClientDataSet.FieldByName(fType).AsString;
            vSerial := ClientDataSet.FieldByName(fSerial).AsString;
            vConection := ClientDataSet.FieldByName(fConection).AsString;
            vDuration := ClientDataSet.FieldByName(fDurationMin).AsString;
            vCapacityGB := ClientDataSet.FieldByName(fCapacityGB).AsString;
            vCopyedGB := ClientDataSet.FieldByName(fCopyedGB).AsString;
            vInitialGB := ClientDataSet.FieldByName(fInitialGB).AsString;
            vMinimalGB := ClientDataSet.FieldByName(fMinimalGB).AsString;
            vFinalGB := ClientDataSet.FieldByName(fFinalGB).AsString;

            //Realiza la comparación en busca de coincidencias.
            Fill := -1;
            if SearchInString(vNumber, FSearchText, FCaseSensitive, FExact)     then Fill := 0;
            if SearchInString(vUnit, FSearchText, FCaseSensitive, FExact)       then Fill := 1;
            if SearchInString(vName, FSearchText, FCaseSensitive, FExact)       then Fill := 2;
            if SearchInString(vType, FSearchText, FCaseSensitive, FExact)       then Fill := 3;
            if SearchInString(vSerial, FSearchText, FCaseSensitive, FExact)     then Fill := 4;
            if SearchInString(vCapacityGB, FSearchText, FCaseSensitive, FExact) then Fill := 5;
            if SearchInString(vConection, FSearchText, FCaseSensitive, FExact)  then Fill := 6;
            if SearchInString(vDuration, FSearchText, FCaseSensitive, FExact)   then Fill := 7;
            if SearchInString(vCopyedGB, FSearchText, FCaseSensitive, FExact)   then Fill := 8;
            if SearchInString(vInitialGB, FSearchText, FCaseSensitive, FExact)  then Fill := 9;
            if SearchInString(vMinimalGB, FSearchText, FCaseSensitive, FExact)  then Fill := 10;
            if SearchInString(vFinalGB, FSearchText, FCaseSensitive, FExact)    then Fill := 11;
            Result := Fill >= 0;

            if Assigned(FOnProcess) then FOnProcess(i, count);  //Dispara un evento que informa del estado del proceso.
            if ClientDataSet.Eof or Result then Break;          //Sale si encuentra coincidencia o llega al final.
            ClientDataSet.Next;                                 //Se coloca en la siguiente posición.
            end;
      end;
except
   OnEventLog('Error: TDataControl.SearchNext');
   Result := False;                         //Indica el éxito.
end;
end;

//-----------------------------------------------------------------------------
// Realiza un recorrido por las filas de la base de datos.
//
// Entradas:
// IntervalMls = Intervalo mínimo de tiempo que toma hacer el cambio entre
//               un registro y otro cuando se recorren los datos. Se utiliza
//               para darle tiempo al usuario de reaccionar y poder detener
//               el recorrido cuando vea algo de interés.
//-----------------------------------------------------------------------------
procedure TDataControl.Traverse(IntervalMls: Integer; First: Boolean);
var count, i: Integer;
begin
if not FOpened then Exit;                     //Sale si no se ha abierto un fichero de DB.
if First then ClientDataSet.First;            //Se para en la primera posición si es la primera búsqueda.
try
   count := ClientDataSet.RecordCount;        //Cuenta la cantidad de filas.
   if count > 0 then                          //Si hay filas...
      begin
      FContinue := True;                      //Activa el inicio del ciclo de recorrido.
      i := 0;
      while FContinue do                      //Mientras esté activado el ciclo...
            begin
            Inc(i);
            Sleep(IntervalMls);                                 //Realiza una espera de x milisegundos...
            if Assigned(FOnProcess) then FOnProcess(i, count);  //Dispara un evento que informa del estado del proceso.
            if ClientDataSet.Eof then Break;                    //Sale si llega al final.
            ClientDataSet.Next;                                 //Se coloca en la siguiente posición.
            end;
      end;
except
   OnEventLog('Error: TDataControl.Traverse');
end;
end;

//-----------------------------------------------------------------------------
// Busca la coincidencia entre dos cadenas de texto.
//
// Entradas:
// Str1 = Cadena de texto dentro de la cual se busca.
// SearchText = Cadena de texto que se busca.
// Sensitive = TRUE indica que se deben tener en cuenta las
//             diferencias entre mayúsculas y minúsculas.
// Exact = TRUE indica que las coincidencias debene ser exactas.
//         FALSE permite que la cadena buscada esté contenida
//         dentro de un texto más grande.
//
// Salidas: Devuelve TRUE si se encuentra una coincidencia.
//
// Nota: El cursor de datos se queda en la posición de la coincidencia.
//-----------------------------------------------------------------------------
function TDataControl.SearchInString(Str1: String; SearchText: String; Sensitive: Boolean; Exact: Boolean): Boolean;
begin
if not Sensitive then                     //Si no se tienen en cuenta las diferencias
   begin                                  //entre mayúsculas y minúsculas.
   Str1 := UpperCase(Str1);               //Convierte a mayúsculas las dos
   SearchText := UpperCase(SearchText);   //cadenas que se comparan.
   end;

if Exact then                             //Si la comparación debe ser exacta entonces...
   Result := Str1 = SearchText            //devuelve TRUE si las cadenas son iguales.
else                                      //Si la comparación no debe ser axacta...
   Result := Pos(SearchText, Str1) > 0;   //Si la cadena Str1 contiene a SearchText devuelve TRUE.
end;

//-----------------------------------------------------------------------------
// Dispara el evento OnError.
//-----------------------------------------------------------------------------
procedure TDataControl.OnEventLog(msg: String);
begin
if Assigned(FOnLog) then            //Si el evento está asignado...
   FOnLog(msg);                     //reporta el error disparando un evento.
end;

end.
