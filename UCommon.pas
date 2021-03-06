
///////////////////////////////////////////////////////////////////////////////
// Nombre: UCommon
// Autor: Santiago A. Orellana P?rez (Chago)
// Creado: 25/12/2016
// Objetivo: Agrupa un grupo de funciones y constantes necesarias
//           y comunes a la mayor parte del c?digo del programa.
///////////////////////////////////////////////////////////////////////////////

unit UCommon;

interface

uses Classes, UAntireversing, SysUtils, Windows, TLHelp32, SHFolder, Registry,
     Graphics, DateUtils, Math;
     
//-----------------------------------------------------------------------------
// Equivalencias de las unidades de medida de informaci?n.
//-----------------------------------------------------------------------------
const cKByte = 1024.0;
const cMByte = cKByte * 1024.0;
const cGByte = cMByte * 1024.0;
const cTByte = cGByte * 1024.0;

//-----------------------------------------------------------------------------
// Definici?n de algunas de las constantes del programa.
//-----------------------------------------------------------------------------
const cCompanny       = '0p1fTd8o;pLD=i6b?hAU3';                                                  //'TecnoChago'
const cAppTitle       = '0P1ITJ8W;BLU=D6';                                                        //Nombre de la aplicaci?n = 'CHIVATO';
const cAppID          = 1;                                                                        //Identificador de la aplicaci?n.
const cAppVersion     = 1;                                                                        //Versi?n de la aplicaci?n.
const cAppSubVersion  = 3;                                                                        //Subversi?n de la aplicaci?n.
const cAppAutor       = 'T{Lv<u3pJs;;T!6T;bFoXu7j6b;h7pH!DB;/6!IPOsOf3m3m2b4oQbQ!@QI?BsOfDB>';    //'Autor: Santiago A. Orellana P?rez';
const cAppPhone       = '35Cf2mS?2g<pVo?pP;=!M6S5N7<466=:C5:UR';                                  //'Tel?fono: 54635944';
const cAppMail        = ';vDp6sTs<fPpY;D!UuRf0d5o6pEd0iHb0hPpKAPoMbGv8uLbH/XdKDY';                //'Correo: tecnochago@nauta.cu';
const cAppCountryYear = '081bT!8I;bLc=b6o?bA-3!CD2vSc2b<-V!?3P1=2MMS';                            //'La Habana, Cuba, 2017';
const cRights         = ';/Dp6eTp<tP!YmDpUtR!0e5f6sEf0dHi0pPtK!PsMfGt8fLsHwXbKeYp:tLU<';          //'Todos los derechos reservados.'

//-----------------------------------------------------------------------------
// Nombre de los objetos mutex que crean los ejecutables.
//-----------------------------------------------------------------------------
const cSemaphoreChivato = 'SemaphoreChivato';
const cSemaphoreLoader  = 'SemaphoreLoaders';


//-----------------------------------------------------------------------------
// Ficheros que pertenecen al programa.
//-----------------------------------------------------------------------------
const cHF1 = 'Index.mht';
const cHF2 = 'Index.pdf';

//-----------------------------------------------------------------------------
// Inicial con que empiezan todos los nombres de los ficheros de este programa.
//-----------------------------------------------------------------------------
const cFileInit = '0';   //Este es el programa n?mero 0.

//-----------------------------------------------------------------------------
// Constantes que definen los nombres para ficheros, PIPES, Llaves del
// registro de windows y otros objetos.
//-----------------------------------------------------------------------------
const cFDataBase = 'DataBase';
const cKPassword = 'Password';

//-----------------------------------------------------------------------------
// Representan los tipos de ficheros.
//-----------------------------------------------------------------------------
type TFilesTypes = (ftTXT, ftCSV, ftHTML, ftRTF, ftXML, ftINI);

//-----------------------------------------------------------------------------
// Funciones que se exportan...
//-----------------------------------------------------------------------------

function FilterHexa(str: String): String;
function FilterAbove31(str: String): String;
function FilterExtractSpace(str: String): String;
function FilterXML(str: String): String;

function ParamToFile(FileName: String; Value: String): Boolean;
function ParamFromFile(FileName: String): String;
function ParamToRegistry(Root: HKEY; Key: String; ValueName: String; Value: String): Boolean;
function ParamFromRegistry(Root: HKEY; Key: String; ValueName: String): String;

function GetWindowsUser: String;
function GetDateForName: String;
function NormaliceDirectory(Dir: String): String;
function KillTask(FileName: String): Integer;
function GetDataDirectory(Dir: Integer): String;
function IsAccessible(Path: String): Boolean;

function ToXByte(GB: Double): String;
function ToXTime(M: Integer): String;

function GetDiskSerial(DiskChar: Char): DWORD;
function GetDayName(number: Byte): String;
function GetDayColor(number: Byte): TColor;


implementation

//-----------------------------------------------------------------------------
// Deja solo los caracteres hexadecimales.
//-----------------------------------------------------------------------------
function FilterHexa(str: String): String;
var n, len: Integer;
    s: String;
begin
Result := '';
len := Length(str);
s := UpperCase(str);
if len > 0 then
   for n := 1 to len do
       if (str[n] in ['A'..'F', '0'..'9']) then
          Result := Result + s[n];
end;

//-----------------------------------------------------------------------------
// Deja solo los caracteres superiores a 0x31.
//-----------------------------------------------------------------------------
function FilterAbove31(str: String): String;
var n, len: Integer;
begin
Result := '';
len := Length(str);
if len > 0 then
   for n := 1 to len do
       if (Ord(str[n]) >= 32) then
          Result := Result + str[n];
end;

//-----------------------------------------------------------------------------
// Elimina los caracteres de espacio.
//-----------------------------------------------------------------------------
function FilterExtractSpace(str: String): String;
var n, len: Integer;
begin
Result := '';
len := Length(str);
if len > 0 then
   for n := 1 to len do
       if not (str[n] = Chr($20)) then
          Result := Result + str[n];
end;

//-----------------------------------------------------------------------------
// Elimina los caracteres que no se permiten en un documento XML.
//-----------------------------------------------------------------------------
function FilterXML(str: String): String;
var n, len: Integer;
begin
Result := '';
len := Length(str);
if len > 0 then
   for n := 1 to len do
       case str[n] of
            Chr($20): Result := Result + '_';
            '?': Result := Result + 'a';
            '?': Result := Result + 'e';
            '?': Result := Result + 'i';
            '?': Result := Result + 'o';
            '?': Result := Result + 'u';
            '?': Result := Result + 'n';
            '?': Result := Result + 'A';
            '?': Result := Result + 'E';
            '?': Result := Result + 'I';
            '?': Result := Result + 'O';
            '?': Result := Result + 'U';
            '?': Result := Result + 'N';
            else
               Result := Result + str[n];
            end;
end;

//-----------------------------------------------------------------------------
// Escribe un valor en un fichero de texto.
//
// Entradas:
// FileName = Nombre completo del fichero que se crea.
// Value = Valor que se escribe en el fichero. Debe ser una cadena hexadecimal.
//
// Salida:
// Devuelve TRUE solo si se pudo crear el fichero y guardar el valor.
//-----------------------------------------------------------------------------
function ParamToFile(FileName: String; Value: String): Boolean;
var Writer: TStrings;
begin
Result := ReturnFalse(FileName, 127, 3);                    //Por defecto devuelve FALSE. Los par?metros son para confundir.
try
   Writer := TStringList.Create;                               //Crea un objeto para escribir un fichero.
   Writer.Text := FilterAbove31(EncProtectStr(Value));      //Obtien los datos a escribir.
   try
      Writer.SaveToFile(FileName);                          //Crea el fichero y escribe los datos.
      Result := ReturnTrue(FileName, 255, 3);               //Siempre devuelve TRUE. Los par?metros son para confundir.
   except
      MessageBeep(MB_ICONERROR);
   end;
finally
   Writer.Free;                                             //Destruye el objeto.
end;
end;

//-----------------------------------------------------------------------------
// Lee un valor desde un fichero de texto.
//
// Entradas:
// FileName = Nombre completo del fichero que se lee.
// Value = Obtiene el valor que se lee desde el fichero.
//         Debe ser una cadena hexadecimal.
//
// Salida:
// Devuelve TRUE solo si se pudo leer el fichero y el valor.
//-----------------------------------------------------------------------------
function ParamFromFile(FileName: String): String;
var Reader: TStrings;
begin
Result := '';                                                     //Por defecto devuelve FALSE. Los par?metros son para confundir.
if FileExists(FileName) then
   begin
   try
      Reader := TStringList.Create;                               //Crea un objeto para leer un fichero.
      try
         Reader.LoadFromFile(FileName);                           //Abre el fichero y lee los datos.
         Result := DecProtectStr(FilterAbove31(Reader.Text));     //Lee los datos y los filtra.
      except
         MessageBeep(MB_ICONERROR);
      end;
   finally
      Reader.Free;                                                //Destruye el objeto.
   end;
   end;
end;

//-----------------------------------------------------------------------------
// Escribe un valor en una llave del registro de windows.
//
// Entradas:
// Roor = Llave raiz de las establecidas por windows.
// Key = Llave creada por la aplicaci?n.
// ValueName = Nombre del valor dentro de la llave.
// Value = Valor que se le asigna.
//
// Salida:
// Devuelve TRUE si se pudo crear correctamente la llave y guardar el valor.
//-----------------------------------------------------------------------------
function ParamToRegistry(Root: HKEY; Key: String; ValueName: String; Value: String): Boolean;
var Reg: TRegistry;
begin
Result := False;
if Key = '' then Exit;
if ValueName = '' then Exit;
try
   Reg := TRegistry.Create;
   if Key[1] <> '\' then Key := '\' + Key;
   Reg.RootKey := Root;
   try
      if Reg.OpenKey(Key, True) then
         begin
         Reg.WriteString(ValueName, FilterAbove31(EncProtectStr(Value)));
         Reg.CloseKey;
         Result := True;
         end;
   except
      MessageBeep(MB_ICONERROR);
   end;
finally
   Reg.Free;
end;
end;

//-----------------------------------------------------------------------------
// Lee un valor desde un fichero de texto.
//
// Entradas:
// Roor = Llave raiz de las establecidas por windows.
// Key = Llave creada por la aplicaci?n.
// ValueName = Nombre del valor dentro de la llave.
//
// Salida:
// Devuelve el valor almacenado en la llave bajo el nombre indicado.
//-----------------------------------------------------------------------------
function ParamFromRegistry(Root: HKEY; Key: String; ValueName: String): String;
var Reg: TRegistry;
begin
Result := '';
if Key = '' then Exit;
if ValueName = '' then Exit;
try
   Reg := TRegistry.Create;
   if Key[1] <> '\' then Key := '\' + Key;
   Reg.RootKey := Root;
   try
      if Reg.KeyExists(Key) then
         if Reg.OpenKey(Key, True) then
            begin
            Result := DecProtectStr(FilterAbove31(Reg.ReadString(ValueName)));
            Reg.CloseKey;
            end;
   except
      MessageBeep(MB_ICONERROR);
   end;
finally
   Reg.Free;
end;
end;

//-----------------------------------------------------------------------------
// Obtiene el nombre del usuario de Windows.
//-----------------------------------------------------------------------------
function GetWindowsUser: String;
var pcUser: PChar;
    dwUSize: DWORD;
begin
dwUSize := 21;
try
   GetMem(pcUser, dwUSize);
   if Windows.GetUserName(pcUser, dwUSize) then Result := pcUser
finally
   FreeMem(pcUser);
end;
end;

//-----------------------------------------------------------------------------
// Obtiene la fecha actual en formato ?til para nombres de ficheros.
//-----------------------------------------------------------------------------
function GetDateForName: String;
var s: String;
    d: TDateTime;
begin
d := Now;
s := '';
s := s + IntToStr(YearOf(d)) + '_';
s := s + IntToStr(MonthOf(d)) + '_';
s := s + IntToStr(DayOf(d)) + '_';
s := s + IntToStr(HourOf(d)) + '_';
s := s + IntToStr(MinuteOf(d)) + '_';
s := s + IntToStr(SecondOf(d)) + '_';
s := s + IntToStr(MilliSecondOf(d));
Result := s;
end;

//-----------------------------------------------------------------------------
// Normalizar el nombre de un directorio.
//-----------------------------------------------------------------------------
function NormaliceDirectory(Dir: String): String;
begin
if Dir <> '' then if Dir[Length(Dir)] <> '\' then Dir := Dir + '\';
Result := Dir;
end;

//-----------------------------------------------------------------------------
// Cierra una aplicaci?n por el nombre de su fichero ejecutable.
//-----------------------------------------------------------------------------
function KillTask(FileName: String): Integer;
var ContinueLoop: BOOL;
    FSnapshotHandle: THandle;
    FProcessEntry32: TProcessEntry32;
const PROCESS_TERMINATE = $0001;
begin
try
   FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
   FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
   ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
   while integer(ContinueLoop) <> 0 do
         begin
         if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(FileName)) or
            (UpperCase(FProcessEntry32.szExeFile) = UpperCase(FileName))) then
            Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE,BOOL(0),
                                               FProcessEntry32.th32ProcessID),0));
         ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
         end;
finally
   CloseHandle(FSnapshotHandle);
end;
end;

//-----------------------------------------------------------------------------
// Devuelve la fecha de creaci?n de un fichero.
//-----------------------------------------------------------------------------
function GetFileDate(FileName: String): TDateTime;
var FHandle: Integer;
begin
try
   FHandle := FileOpen(FileName, 0);
   Result := FileDateToDateTime(FileGetDate(FHandle));
finally
   FileClose(FHandle);
end;
end;

//-----------------------------------------------------------------------------
// Obtiene el directorio de datos de aplicaciones de todos los usuarios.
//-----------------------------------------------------------------------------
function GetDataDirectory(Dir: Integer): String;
const SHGFP_TYPE_CURRENT = 0;
var path: array[0..MaxChar] of char;
begin
try
   SHGetFolderPath(0, Dir, 0, SHGFP_TYPE_CURRENT, @path[0]);
   Result := path + '\';
except
   Result := '';
end;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el directorio indicado es accesible por el usuario
// actual de windows. Intenta escribir un fichero en el directorio.
//-----------------------------------------------------------------------------
function IsAccessible(Path: String): Boolean;
var Writer: TStrings;
    TestFile: String;
begin
Result := True;
Writer := TStringList.Create;                         //Crea el escritor.
try
   Writer.Text := DateTimeToStr(Now);                 //Pone un texto cualquiera en el escritor.
   TestFile := NormaliceDirectory(Path) + 'test';     //es el nombre del fichero a escribir.
   try
      Writer.SaveToFile(TestFile);                    //Intenta escribir el fichero.
      DeleteFile(PChar(TestFile));                    //Intenta borrarlo.
   except
      Result := False;                                //Devuelve FALSE si ocurre error.
   end;
finally
   Writer.Free;                                       //Libera el escritor.
end;
Result := True;
end;

//-----------------------------------------------------------------------------
// Devuelve una cadena representando los valores en MegaBytes, GigaByte
// o TeraByte seg?n la cantidad del valor.
//
// Entrada:
// GB = Cantidad de informaci?n en Giga Bytes.
//-----------------------------------------------------------------------------
function ToXByte(GB: Double): String;
const cVal: Array [0 .. 8] of string =
     ('Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB');
var i: Integer;
    v: Double;
begin
try
   v := GB * cGByte;
   i := 0;
   while v > Power(1024, i + 1) do Inc(i);
   Result := FloatToStrF(v / Power(1024, i), ffFixed, 16, 4) + Chr($20) + cVal[i];
except
   Result := '';
end;
end;

//-----------------------------------------------------------------------------
// Devuelve una cadena representando el tiempo en Horas o minutos.
//
// Entrada:
// M = Cantidad de tiempo en minutos.
//-----------------------------------------------------------------------------
function ToXTime(M: Integer): String;
const CM = 60;
begin
try
   if M < CM then
      Result := IntToStr(M) + ' Minutos'
   else
      Result := FloatToStrF(M / CM, ffFixed, 16, 2) + ' Horas';    
except
   Result := '';
end;
end;

//-----------------------------------------------------------------------------
// Obtiene el n?mero de serie del dispositivo indicado.
//-----------------------------------------------------------------------------
function GetDiskSerial(DiskChar: Char): DWORD;
var dwSDSize, MaxFNLength, VolFlags, FDiskSerial: DWORD;
    SystemDir: String;
begin
SystemDir := DiskChar + ':\';                         //Crea el Path del disco.
try                                                   //Obtiene el n?mero de serie y nombre del dispositivo de almacenamiento.
   if not GetVolumeInformation(PChar(SystemDir),      //Path del disco de sistema.
                               nil,                   //No lo usamos.
                               0,                     //No lo usamos.
                               @FDiskSerial,          //Aqu? se coloca el n?mero de serie.
                               MaxFNLength,           //No lo usamos.
                               VolFlags,              //No lo usamos.
                               nil,                   //No lo usamos.
                               0                      //No lo usamos.
                               )then                  //Si ocurre alg?n error
      Result := 0                                     //devuelve cero...
   else                                               //de lo contrario, devuelve
      Result := FDiskSerial;                          //el n?mero e serie.
except
   Result := 0;
end;
end;

//-----------------------------------------------------------------------------
// Devuelve el nombre del d?a de la semana.
//-----------------------------------------------------------------------------
function GetDayName(number: Byte): String;
begin
case number of
     1: Result := 'Lunes';
     2: Result := 'Martes';
     3: Result := 'Mi?rcoles';
     4: Result := 'Jueves';
     5: Result := 'Viernes';
     6: Result := 'S?bado';
     7: Result := 'Domingo';
     else Result := '';
     end;
end;

//-----------------------------------------------------------------------------
// Devuelve el nombre del d?a de la semana.
//-----------------------------------------------------------------------------
function GetDayColor(number: Byte): TColor;
begin
case number of
     1: Result := clRed;          //'Lunes'
     2: Result := clGreen;        //'Martes'
     3: Result := clBlue;         //'Mi?rcoles'
     4: Result := clMaroon;       //'Jueves'
     5: Result := clYellow;       //'Viernes'
     6: Result := clLime;         //'S?bado'
     7: Result := clFuchsia;      //'Domingo'
     else Result := clBlack;
     end;
end;

end.
