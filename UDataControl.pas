
///////////////////////////////////////////////////////////////////////////////
// Nombre: UDataControl
// Autor: Santiago A. Orellana P�rez (Chago)
// Creado: 23/07/2016
// Objetivo: Implementa el almacenamiento, filtrado y control de los datos.
///////////////////////////////////////////////////////////////////////////////

unit UDataControl;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, DB, DBClient, QButtons, QComCtrls, QGrids, QDBGrids;

//-----------------------------------------------------------------------------
// Campos que conforman la tabla de datos.
//-----------------------------------------------------------------------------
const cNumber       = 'N�mero';         //N�mero identificador del evento.
const cUnit         = 'Unidad';         //Letra asignada por el sistema operativo.
const cName         = 'Nombre';         //Nombre del dispositivo.
const cType         = 'Tipo';           //Tipo de dispositivo.
const cSerial       = 'Serial';         //N�mero de serie del dispositivo.
const cCapacityGB   = 'Capacidad GB';   //Capacidad del dispositivo en GB.
const cConection    = 'Conectado';      //Fecha y hora de la conexi�n.
const cDurationMin  = 'Minutos';        //Duraci�n de la conexi�n en minutos.
const cInitialGB    = 'Inicio GB';      //Cantidad de informaci�n inicial contenida en el dispositivo.
const cMinimalGB    = 'Minimo GB';      //Cantidad de informaci�n m�nima que hubo en el dispositivo.
const cFinalGB      = 'Final GB';       //Cantidad de informaci�n que finalmente obtiene el dispositivo.

//-----------------------------------------------------------------------------
// Clase que implementa el controlador de datos.
//-----------------------------------------------------------------------------
type
  TDataControl = class
    ClientDataSet: TClientDataSet;
    DataSource: TDataSource;
  private
    procedure AddField(pName: String; pType: TFieldType; pSize: Integer);
    function DesignTable: Boolean;
    function SaveTable(FileName: String): Boolean;
  public
    constructor Create;
    destructor Destroy;
    function CreateTable(FileName: String): Boolean;
    function LoadTable(FileName: String): Boolean;
    function AddData(pUnit: String;             //Letra asignada por el sistema operativo.
                     pName: String;             //Nombre del dispositivo.
                     pType: String;             //Tipo de dispositivo.
                     pSerial: String;           //N�mero de serie del dispositivo.
                     pCapacityGB: Double;       //Capacidad del dispositivo en GB.
                     pConection: TDateTime;     //Fecha y hora de la conexi�n.
                     pDurationMin: Integer;     //Duraci�n de la conexi�n en minutos.
                     pInitialGB: Double;        //Cantidad de informaci�n inicial contenida en el dispositivo.
                     pMinimalGB: Double;        //Cantidad de informaci�n m�nima que hubo en el dispositivo.
                     pFinalGB: Double           //Cantidad de informaci�n que finalmente obtiene el dispositivo.
                     ): Boolean;
    function ClearData: Boolean;                //Borra todos los datos de la tabla.
  end;


implementation

//-----------------------------------------------------------------------------
// Inicia las variables y datos del controlador.
//-----------------------------------------------------------------------------
constructor TDataControl.Create;
begin
ClientDataSet := TClientDataSet.Create(nil);    //Crea el Dataset.
DataSource := TDataSource.Create(nil);          //Crea el DatSource.
DataSource.DataSet := ClientDataSet;            //Enlaza el DatSource al DataSet.
end;

//-----------------------------------------------------------------------------
// Libera las variables y datos del controlador.
//-----------------------------------------------------------------------------
destructor TDataControl.Destroy;
begin
ClientDataSet.Free;
DataSource.Free;
end;

//-----------------------------------------------------------------------------
// Crea una tabla y la guarda en el fichero especificado, sin abrirla.
//-----------------------------------------------------------------------------
function TDataControl.CreateTable(FileName: String): Boolean;
begin
Result := False;                                //Por defecto devuelve FALSE.
if DesignTable then                             //Si se puede dise�ar la tabla...
   if SaveTable(FileName) then                  //Crea la tabla y si se pudo crear...
      Result := True;                           //Devuelve TRUE indicando �xito.
end;

//-----------------------------------------------------------------------------
// Carga y abre una tabla desde un fichero de disco.
//-----------------------------------------------------------------------------
function TDataControl.LoadTable(FileName: String): Boolean;
begin
Result := False;                                //Por defecto devuelve FALSE.
If FileExists(FileName) then                    //Si el fichero existe,
   try
      ClientDataSet.Active := False;            //Desactiva...
      ClientDataSet.FileName := FileName;       //Indica la ruta y...
      ClientDataSet.Active := True;             //Activa nuevamente.
   finally
      Result := True;                           //Indica el �xito.
   end;
end;

//-----------------------------------------------------------------------------
// Agrega un registro de datos a la tabla.
//-----------------------------------------------------------------------------
function TDataControl.AddData(pUnit: String;             //Letra asignada por el sistema operativo.
                              pName: String;             //Nombre del dispositivo.
                              pType: String;             //Tipo de dispositivo.
                              pSerial: String;           //N�mero de serie del dispositivo.
                              pCapacityGB: Double;       //Capacidad del dispositivo en GB.
                              pConection: TDateTime;     //Fecha y hora de la conexi�n.
                              pDurationMin: Integer;     //Duraci�n de la conexi�n en minutos.
                              pInitialGB: Double;        //Cantidad de informaci�n inicial contenida en el dispositivo.
                              pMinimalGB: Double;        //Cantidad de informaci�n m�nima que hubo en el dispositivo.
                              pFinalGB: Double           //Cantidad de informaci�n que finalmente obtiene el dispositivo.
                              ): Boolean;
begin
Result := False;                //Por defecto devuelve FALSE.
try
  with ClientDataSet do         //Comienza a agregar los
       begin                    //valores de cada campo.
       Open;                    //Abre la tabla.
       Append;                  //Agrega un registro al final de la tabla.

       FieldByName(cUnit).AsString := pUnit;
       FieldByName(cName).AsString := pName;
       FieldByName(cType).AsString := pType;
       FieldByName(cSerial).AsString := pSerial;
       FieldByName(cCapacityGB).AsFloat := pCapacityGB;
       FieldByName(cConection).AsDateTime := pConection;
       FieldByName(cDurationMin).AsInteger := pDurationMin;
       FieldByName(cInitialGB).AsFloat := pInitialGB;
       FieldByName(cMinimalGB).AsFloat := pMinimalGB;
       FieldByName(cFinalGB).AsFloat := pFinalGB;

       Post;                    //Guarda el registro y sus valores.
       Close;                   //Cierra la tabla.
       end;
finally
  Result := True;               //Indica el �xito.
end;
end;

//-----------------------------------------------------------------------------
// Borra todos los datos de la tabla.
//-----------------------------------------------------------------------------
function TDataControl.ClearData: Boolean;
begin
Result := False;                      //Por defecto devuelve FALSE.
try
   ClientDataSet.Open;                //Activa...
   ClientDataSet.Edit;                //Activa la edici�n.
//   ClientDataSet.deFields;        //Borra todos los campos.**************************************************************************
   ClientDataSet.Post;                //Guarda los cambios.
   ClientDataSet.Close;               //Desactiva...
finally
   Result := True;                    //Indica el �xito.
end;
end;

//-----------------------------------------------------------------------------
// Agrega campos a la tabla.
//-----------------------------------------------------------------------------
procedure TDataControl.AddField(pName: String; pType: TFieldType; pSize: Integer);
begin
  try
    ClientDataSet.Active := False;                 //Deasactiva el DataSet.
    with ClientDataSet.FieldDefs.AddFieldDef do
         begin
         Name := pName;                            //Establece el nombre.
         DataType := pType;                        //Establece el tipo de dato.
         Size := pSize;                            //Establece el tama�o del campo.
         end;
    ClientDataSet.CreateDataSet;                   //Crea el campo.
  finally
    ClientDataSet.Active := True;                  //Activa nuevamente el DataSet.
  end;
end;

//-----------------------------------------------------------------------------
// Dise�a una tabla para guardar los datos.
//-----------------------------------------------------------------------------
function TDataControl.DesignTable: Boolean;
begin
Result := False;
try
   AddField(cNumber, ftAutoInc, 0);         //N�mero identificador del evento.
   AddField(cUnit, ftString, 1);            //Letra asignada por el sistema operativo.
   AddField(cName, ftString, 20);           //Nombre del dispositivo.
   AddField(cType, ftString, 20);           //Tipo de dispositivo.
   AddField(cSerial, ftString, 16);         //N�mero de serie del dispositivo.
   AddField(cCapacityGB, ftFloat, 0);       //Capacidad del dispositivo en GB.

   AddField(cConection, ftDateTime, 0);     //Fecha y hora de la conexi�n.
   AddField(cDurationMin, ftInteger, 0);    //Duraci�n de la conexi�n en minutos.
   AddField(cInitialGB, ftFloat, 0);        //Cantidad de informaci�n inicial contenida en el dispositivo.
   AddField(cMinimalGB, ftFloat, 0);        //Cantidad de informaci�n m�nima que hubo en el dispositivo.
   AddField(cFinalGB, ftFloat, 0);          //Cantidad de informaci�n que finalmente obtiene el dispositivo.
finally
   Result := True;                          //Devuelve TRUE si todo sali� OK.
end;
end;

//-----------------------------------------------------------------------------
// Crea una tabla vac�a para guardar los datos.
//-----------------------------------------------------------------------------
function TDataControl.SaveTable(FileName: String): Boolean;
begin
Result := False;                                   //Por defecto devuelve FALSE.
If not FileExists(FileName) then                   //Si el fichero no existe,
   try
      Clientdataset.SaveToFile(FileName, dfxml);   //Guarda la tabla en un fichero.
   finally
      Result := True;                              //Devuelve TRUE.
   end;
end;


end.
