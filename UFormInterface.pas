
///////////////////////////////////////////////////////////////////////////////
// Nombre: UFormInterface
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 24/07/2016
// Objetivo: Implementa el formulario de interface de la aplicación.
//           Contiene los menús de control de la aplicación y los
//           visores de datos, gráficas y otros valores.
///////////////////////////////////////////////////////////////////////////////

unit UFormInterface;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, StdCtrls, ExtCtrls, ActnList, Menus, DB, Spin,
  ComCtrls, ToolWin, DBClient, UViewDetail, UShowLog,        
  UQueryNewPassword, UAntiReversing, UQuerySearchText,
  DBCtrls, UFormOrigin, UViewStatistics, UStoreDataControl,
  StdActns, UCommon, ShellApi,
  UViewPresentDevices, UConvert;

//-----------------------------------------------------------------------------
const CMaxEscala = 100;

//-----------------------------------------------------------------------------
// Clase que implementa el formulario de interface.
//-----------------------------------------------------------------------------
type
  TFormInterface = class(TForm)
    DataSource1: TDataSource;
    MainMenu1: TMainMenu;
    Ayuda1: TMenuItem;
    Archivo1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    StatusBar1: TStatusBar;
    ActionList1: TActionList;
    ActionChangePassword: TAction;
    ActionExportToTXT: TAction;
    ActionExportToCSV: TAction;
    ActionExportToHTML: TAction;
    ActionOrigin: TAction;
    ActionUtilisation: TAction;
    ToolBar2: TToolBar;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    DTPFrom: TDateTimePicker;
    DTPTo: TDateTimePicker;
    ComboBoxDateRank: TComboBox;
    GroupBox1: TGroupBox;
    ToolButton3: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ComboBoxOp2: TComboBox;
    ToolButton27: TToolButton;
    SpinEditGB2: TSpinEdit;
    GroupBox3: TGroupBox;
    CheckBoxFilterCapacity: TCheckBox;
    ComboBoxOp1: TComboBox;
    SpinEditGB1: TSpinEdit;
    CheckBoxFilterCopy: TCheckBox;
    CheckBoxFilterDate: TCheckBox;
    ActionClose: TAction;
    Cerrar1: TMenuItem;
    Procedencia1: TMenuItem;
    ActionShowStatistics: TAction;
    Origen1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Cambiarpassword1: TMenuItem;
    ActionClearDB: TAction;
    N4: TMenuItem;
    View1: TMenuItem;
    ActionFilterDate: TAction;
    ActionFilterCapacity: TAction;
    ActionFilterCopyed: TAction;
    ActionFilterDate1: TMenuItem;
    ActionFilterCapacity1: TMenuItem;
    ActionFilterCopyed1: TMenuItem;
    OpenDialog1: TOpenDialog;
    DBGrid1: TDBGrid;
    SaveDialog2: TSaveDialog;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ActionViewDetails: TAction;
    ToolButton14: TToolButton;
    ActionFinally: TAction;
    ActionViewSelect: TAction;
    ActionColorFromCopy: TAction;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton5: TToolButton;
    ActionSearchText: TAction;
    ActionSearchNexText: TAction;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    Anlisis1: TMenuItem;
    Exportar1: TMenuItem;
    Verdetallesdeundispositivo1: TMenuItem;
    Verlasestadsticas1: TMenuItem;
    GuardartablacomoTXT1: TMenuItem;
    GuardartablacomoCSV1: TMenuItem;
    GuardartablacomoHTML1: TMenuItem;
    Buscar1: TMenuItem;
    Buscartexto1: TMenuItem;
    Buscarsiguientecoincidencia1: TMenuItem;
    ActionViewLog: TAction;
    ActionClearLog: TAction;
    Mantenimiento1: TMenuItem;
    Vaciarlabasededatos1: TMenuItem;
    ActionAutoExport: TAction;
    Guardarautomticamente1: TMenuItem;
    N2: TMenuItem;
    N5: TMenuItem;
    Seleccindesimilitud1: TMenuItem;
    ColorporGBcopiados1: TMenuItem;
    ActionExportToXML: TAction;
    GuardartablacomoXML1: TMenuItem;
    Finalizarmonitoreo1: TMenuItem;
    ActionViewPresentDevices: TAction;
    Dispositivosactualmenteconectados1: TMenuItem;
    ActionConvert: TAction;
    Convertidor1: TMenuItem;
    N8: TMenuItem;
    ActionBegin: TAction;
    ActionEnd: TAction;
    ActionTraverse: TAction;
    Iralinicio1: TMenuItem;
    Iralfinal1: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    Recorrerdatos1: TMenuItem;
    ActionExportToINI: TAction;
    ExportarcomoINI1: TMenuItem;
    ToolButton4: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton7: TToolButton;
    ToolButton13: TToolButton;
    N1: TMenuItem;
    N3: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure ActionChangePasswordExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBoxDateRankChange(Sender: TObject);
    procedure DTPFromChange(Sender: TObject);
    procedure DTPToChange(Sender: TObject);
    procedure CheckBoxFilterCopyClick(Sender: TObject);
    procedure ComboBoxOp1Change(Sender: TObject);
    procedure SpinEditGB1Change(Sender: TObject);
    procedure ComboBoxOp2Change(Sender: TObject);
    procedure SpinEditGB2Change(Sender: TObject);
    procedure CheckBoxFilterCapacityClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxFilterDateClick(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionOriginExecute(Sender: TObject);
    procedure ActionUtilisationExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ActionClearDBExecute(Sender: TObject);
    procedure ActionFilterDateExecute(Sender: TObject);
    procedure ActionFilterDateUpdate(Sender: TObject);
    procedure ActionFilterCapacityExecute(Sender: TObject);
    procedure ActionFilterCapacityUpdate(Sender: TObject);
    procedure ActionFilterCopyedExecute(Sender: TObject);
    procedure ActionFilterCopyedUpdate(Sender: TObject);
    procedure ActionShowStatisticsExecute(Sender: TObject);
    procedure ActionExportToTXTExecute(Sender: TObject);
    procedure ActionExportToCSVExecute(Sender: TObject);
    procedure ActionExportToHTMLExecute(Sender: TObject);
    procedure ActionViewDetailsExecute(Sender: TObject);
    procedure ActionFinallyExecute(Sender: TObject);
    procedure ActionViewSelectExecute(Sender: TObject);
    procedure ActionViewSelectUpdate(Sender: TObject);
    procedure ActionColorFromCopyExecute(Sender: TObject);
    procedure ActionColorFromCopyUpdate(Sender: TObject);
    procedure ActionSearchTextExecute(Sender: TObject);
    procedure ActionSearchNexTextExecute(Sender: TObject);
    procedure ActionExportToXMLExecute(Sender: TObject);
    procedure ActionViewPresentDevicesExecute(Sender: TObject);
    procedure ActionConvertExecute(Sender: TObject);
    procedure ActionBeginExecute(Sender: TObject);
    procedure ActionEndExecute(Sender: TObject);
    procedure ActionTraverseExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ActionExportToINIExecute(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    FSelectedField: String;
    FSelectedText: String;
    FDrawSimilarSelected: Boolean;
    FDrawGbCopyed: Boolean;
    MainMenuEnable: Boolean;

    procedure DBGrid1WheelResponse(var Message: TMessage);
    function InterpolateTwoColors(Percent: Double; Clr1, Clr2: TColor): TColor;
    procedure EnableControls(val: Boolean);
    procedure ExportToFile(FType: TFilesTypes);
  public
    procedure OnProcessEvent(n: Integer; t: Integer);
  end;


implementation

{$R *.dfm}

uses UFormMain, DateUtils, Types, Math;

type AsControlClass = class(TControl);


//-----------------------------------------------------------------------------
// Prepara las variables internas y los parámetros necesarios.
// Muestra la aplicación en la barra de tareas y Alt + Tab.
//-----------------------------------------------------------------------------
procedure TFormInterface.FormCreate(Sender: TObject);
begin
//Establece el título de la aplicación.
Caption := DecStr(cAppTitle) + ' ' + IntToStr(cAppVersion)+ '.' + IntToStr(cAppSubVersion);

//Asigna propiedades del DBGrid.
try
   DBGrid1.DataSource := DataSource1;
   DataSource1.DataSet := MainForm.DataControl.ClientDataSet;
   DBGrid1.WindowProc := DBGrid1WheelResponse;
   DBGrid1.DoubleBuffered := True;
except
   Application.Terminate;
end;

//Inicia las variables de control de las selecciones.
FSelectedField := fSerial;
FSelectedText := '';

//Muestra el programa en la barra de tareas...
MainForm.ShowInShel;

//Establece el ancho mínimo predeterminado de las columnas del DBGrid.
//Esto es para el redimensionado automático de las columnas del DBGrid.
try
   with MainForm.DataControl.ClientDataSet do
        begin
        FieldByName(fNumber).Tag := 4 + Canvas.TextWidth(FieldByName(fNumber).DisplayName);
        FieldByName(fUnit).Tag := 4 + Canvas.TextWidth(FieldByName(fUnit).DisplayName);
        FieldByName(fName).Tag := 4 + Canvas.TextWidth(FieldByName(fName).DisplayName);
        FieldByName(fType).Tag := 4 + Canvas.TextWidth(FieldByName(fType).DisplayName);
        FieldByName(fSerial).Tag := 4 + Canvas.TextWidth(FieldByName(fSerial).DisplayName);
        FieldByName(fCapacityGB).Tag := 4 + Canvas.TextWidth(FieldByName(fCapacityGB).DisplayName);
        FieldByName(fConection).Tag := 4 + Canvas.TextWidth(FieldByName(fConection).DisplayName);
        FieldByName(fDurationMin).Tag := 4 + Canvas.TextWidth(FieldByName(fDurationMin).DisplayName);
        FieldByName(fCopyedGB).Tag := 4 + Canvas.TextWidth(FieldByName(fCopyedGB).DisplayName);
        FieldByName(fInitialGB).Tag := 4 + Canvas.TextWidth(FieldByName(fInitialGB).DisplayName);
        FieldByName(fMinimalGB).Tag := 4 + Canvas.TextWidth(FieldByName(fMinimalGB).DisplayName);
        FieldByName(fFinalGB).Tag := 4 + Canvas.TextWidth(FieldByName(fFinalGB).DisplayName);
        end;
except
end;

//Establece el manejador del evento que muestra el progreso de las operaciones.
MainForm.DataControl.OnProcess := OnProcessEvent;

//Habilita el menú principal.
MainMenuEnable := True;

//Establece el dibujado de las filas del DBGrid.
FDrawSimilarSelected := True;
FDrawGbCopyed := True;

//Maximiza el formulario.
WindowState := wsMaximized;
end;

//-----------------------------------------------------------------------------
// Oculta la aplicación para que no se vea en el Shell de Windows.
//-----------------------------------------------------------------------------
procedure TFormInterface.FormClose(Sender: TObject; var Action: TCloseAction);
begin
MainForm.HideInShel;
Action := caFree;
end;

//-----------------------------------------------------------------------------
// Abre una ventana que muestra los datos de cada dispositivo con más detalles.
//-----------------------------------------------------------------------------
procedure TFormInterface.DBGrid1DblClick(Sender: TObject);
begin
with TFormViewDetail.Create(Self) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Hace el scroll del DBGrid con el mause Wheel.
//-----------------------------------------------------------------------------
procedure TFormInterface.DBGrid1WheelResponse(var Message: TMessage);
var Cuanto: SHORT;
    n: integer;
begin
if (Message.Msg = WM_MOUSEWHEEL) then
   begin
   Cuanto := HIWORD(Message.WParam);
   Cuanto := Cuanto div 120;
   for n := 1 to Abs(Cuanto) do
       begin
       if Cuanto > 0 then
          DbGrid1.DataSource.DataSet.Prior
       else
          DbGrid1.DataSource.DataSet.Next;
       end;
   end
else
   begin
   AsControlClass(DBGrid1).WndProc(Message);
   end;
end;

//----------------------------------------------------------------------------
// Devuelve la interpolación de dos colores.
//
// Entradas:
// Percent = Valor entre 0 y 1 que representa la posición a interpolar.
// Clr1 = Color inicial de la interpolación que representa el cero.
// Clr2 = Color final de la interpolación que representa el uno.
//
// Salida: Devuelve el color resultante de la interpolación.
//----------------------------------------------------------------------------
function TFormInterface.InterpolateTwoColors(Percent: Double; Clr1, Clr2: TColor): TColor;
var complement: Double;
    R1, R2, G1, G2, B1, B2: BYTE;
begin
if Percent <= 0 then
   Result := Clr1
else
   if Percent >= 1.0 then
      Result := Clr2
   else
      begin
      R1 := GetRValue(Clr1);
      G1 := GetGValue(Clr1);
      B1 := GetBValue(Clr1);
      R2 := GetRValue(Clr2);
      G2 := GetGValue(Clr2);
      B2 := GetBValue(Clr2);
      complement := 1.0 - Percent;
      Result := RGB( Round(complement * R1 + Percent * R2),
                     Round(complement * G1 + Percent * G2),
                     Round(complement * B1 + Percent * B2));
      end
end;


//-----------------------------------------------------------------------------
// Dibuja las filas alternadamente para que se diferencien una de la otra.
// Si hay un texto seleccionado, dibuja la fila que contenga dicho texto.
//-----------------------------------------------------------------------------
procedure TFormInterface.DBGrid1DrawColumnCell(Sender: TObject;
                                               const Rect: TRect;
                                               DataCol: Integer;
                                               Column: TColumn;
                                               State: TGridDrawState
                                               );
const clInit = TColor($EEFFEE);   //Color mínimo.
const clFin = TColor($77FF77);    //Color máximo.
var gbfin, gbmin, gbcopy: Double;
begin
//Dibuja las celdas según la cantidad de GBytes copiados.
try
   if FDrawGbCopyed then
      begin
      gbcopy := 0;                                                     //Inicia el % de color por Gb.
      gbmin := Column.Field.Dataset.FieldbyName(fMinimalGB).AsFloat;   //Obtiene el mínimo de Gb.
      gbfin := Column.Field.Dataset.FieldbyName(fFinalGB).AsFloat;     //Obtiene el máximo de Gb.
      if gbfin > gbmin then gbcopy := gbfin - gbmin;                   //Calcula el % de color.
      if gbcopy > 1 then gbcopy := 1;                                  //El máximo % de color debe ser 1.
      if gbcopy > 0 then                                               //Si el % es mayor que cero...
         if (gdFocused in State) then                                  //Si la fila tiene el foco,...
            dbgrid1.canvas.brush.color := clBlack                                        //la dibuja de negro.
         else                                                                            //De lo contrario...
            dbgrid1.canvas.brush.color := InterpolateTwoColors(gbcopy, clInit, clFin);   //La dibuja según el % de color.
      end;
except
end;

//Marca las filas que contengan el valor seleccionado.
try
   if FDrawSimilarSelected then
      With MainForm.DataControl.ClientDataSet do
           if FieldList.IndexOf(FSelectedField) >= 0 then
              if FieldByName(FSelectedField).AsString = FSelectedText then
                 DBGrid1.Canvas.Brush.Color := TColor($FF8888);             //Azul
except
end;

try
   DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State) ;
except
end;
end;

//-----------------------------------------------------------------------------
// Copia el texto de la celda que se selecciona.
//-----------------------------------------------------------------------------
procedure TFormInterface.DBGrid1CellClick(Column: TColumn);
begin
try
   FSelectedField := Column.FieldName;        //Copia el nombre del campo de texto.
   FSelectedText := Column.Field.AsString;    //Copia el texto.
   DBGrid1.Invalidate;                        //Dibuja nuevamente la tabla.
except
end;
end;

//-----------------------------------------------------------------------------
// Abre un diálogo que permite cambiar la password de acceso al programa.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionChangePasswordExecute(Sender: TObject);
var Old, New1, New2: String;
    rIQNP: Boolean;
begin
if IsAccessible(MainForm.AccessControl.Directory) then                     //Si el directorio del control de datos es accesible...
   begin
   rIQNP := InputQueryNewPassword(Old, New1, New2);                        //Abre el diálogo para cambiar la password.
   if rIQNP then                                                           //Si se ordenó cambiar la password y...
      if MainForm.AccessControl.ValidatePassword(Old) then                 //Si la password de acceso es válida...
         begin
         if New1 = New2 then                                               //Si las copias de la password nueva son iguales...
            begin
            if MainForm.AccessControl.SetPassword(Old, New1, New2) then    //Si se puede cambiar la password.
               begin
               if MainForm.AccessControl.SavePassword then
                  begin
                  Application.MessageBox(PChar('No se ha podido cargar la password original.'), PChar('ALERTA'), MB_ICONASTERISK);
                  end
               else
                  begin
                  Application.MessageBox(PChar('No se pudo guardar la password nueva.'), PChar('ERROR DESCONOCIDO'), MB_ICONERROR);
                  Application.Terminate;            //Cierra la aplicación.
                  end;
               end
            else
               Application.MessageBox(PChar('No se pudo cambiar la password.'), PChar('ERROR'), MB_ICONERROR);
            end
         else
            Application.MessageBox(PChar('Diferencias entre la password nueva y su repetición.'), PChar('ERROR'), MB_ICONERROR);
         end
      else
         Application.MessageBox(PChar('Usted no puede cambiar la password.'), PChar('ACCESO DENEGADO'), MB_ICONERROR);
   end
else
   Application.MessageBox(PChar('El usuario de Windows no tiene acceso a esta funcionalidad.'), PChar('ERROR'), MB_ICONERROR);
end;

//-----------------------------------------------------------------------------
// Activa o desactiva el filtro de datos por fecha y hora.
//-----------------------------------------------------------------------------
procedure TFormInterface.CheckBoxFilterDateClick(Sender: TObject);
begin
ComboBoxDateRank.Enabled := CheckBoxFilterDate.Checked;
DTPFrom.Enabled := CheckBoxFilterDate.Checked;
DTPTo.Enabled := CheckBoxFilterDate.Checked;
Label1.Enabled := CheckBoxFilterDate.Checked;
ComboBoxDateRankChange(Sender);
end;

procedure TFormInterface.ComboBoxDateRankChange(Sender: TObject);
begin
case ComboBoxDateRank.ItemIndex of
     0: begin   //Hoy
        DTPFrom.DateTime := StartOfTheDay(Now);       //Toma el inicio del día.
        DTPTo.DateTime := EndOfTheDay(Now);           //Toma el final del día.
        DTPFrom.Enabled := False;                     //Activa solo un control.
        DTPTo.Enabled := False;
        DTPFrom.Visible := True;
        DTPTo.Visible := False;
        Label1.Visible := False;
        end;

     1: begin   //Esta semana
        DTPFrom.DateTime := StartOfTheWeek(Now);      //Toma el inicio de la semana.
        DTPTo.DateTime := EndOfTheWeek(Now);          //Toma el final de la semana.
        DTPFrom.Enabled := False;                     //Activa ambos controles.
        DTPTo.Enabled := False;
        DTPFrom.Visible := True;
        DTPTo.Visible := True;
        Label1.Visible := True;
        end;

     2: begin   //Este més
        DTPFrom.DateTime := StartOfTheMonth(Now);     //Toma el inicio del mes.
        DTPTo.DateTime := EndOfTheMonth(Now);         //Toma el final del mes.
        DTPFrom.Enabled := False;                     //Activa ambos controles.
        DTPTo.Enabled := False;
        DTPFrom.Visible := True;
        DTPTo.Visible := True;
        Label1.Visible := True;
        end;

     3: begin   //Este año
        DTPFrom.DateTime := StartOfTheYear(Now);      //Toma el inicio del año.
        DTPTo.DateTime := EndOfTheYear(Now);          //Toma el final del año.
        DTPFrom.Enabled := False;                     //Activa ambos controles.
        DTPTo.Enabled := False;
        DTPFrom.Visible := True;
        DTPTo.Visible := True;
        Label1.Visible := True;
        end;

     4: begin   //Este rango
        DTPFrom.DateTime := StartOfTheYear(Now);      //Toma el inicio del año.
        DTPTo.DateTime := EndOfTheYear(Now);          //Toma el final del año.
        DTPFrom.Enabled := True;                      //Activa ambos controles
        DTPTo.Enabled := True;                        //Para que el usuario
        DTPFrom.Visible := True;
        DTPTo.Visible := True;
        Label1.Visible := True;
        end;

     5: begin   //Todos los días
        DTPFrom.Enabled := False;
        DTPTo.Enabled := False;
        DTPFrom.Visible := False;
        DTPTo.Visible := False;
        Label1.Visible := False;
        end;
     end;
DTPToChange(Sender);
end;

procedure TFormInterface.DTPFromChange(Sender: TObject);
begin
DTPToChange(Sender);
end;

procedure TFormInterface.DTPToChange(Sender: TObject);
begin
if ComboBoxDateRank.ItemIndex >= 0 then
   with MainForm.DataControl do
        begin
        CreateFilterOfDate(DTPFrom.DateTime, DTPTo.DateTime, CheckBoxFilterDate.Checked);
        ApplyFilters;
        end;
end;

//-----------------------------------------------------------------------------
// Activa o desactiva el filtro de datos por capacidad.
//-----------------------------------------------------------------------------
procedure TFormInterface.CheckBoxFilterCapacityClick(Sender: TObject);
begin
ComboBoxOp1.Enabled := CheckBoxFilterCapacity.Checked;
SpinEditGB1.Enabled := CheckBoxFilterCapacity.Checked;
SpinEditGB1Change(Sender);
end;

procedure TFormInterface.ComboBoxOp1Change(Sender: TObject);
begin
SpinEditGB1Change(Sender);
end;

procedure TFormInterface.SpinEditGB1Change(Sender: TObject);
var op: String;
begin
with ComboBoxOp1 do
     if Text = Items[0] then op := '>' else                           //'Mayor que'
        if Text = Items[1] then op := '<' else                        //'Menor que'
           if Text = Items[2] then op := '>=' else                    //'Mayor o igual a'
              if Text = Items[3] then op := '<=' else                 //'Menor o igual a'
                 if Text = Items[4] then op := '=' else               //'Igual a'
                    if Text = Items[5] then op := '<>' else           //'Diferente de'
                       Exit;                                          //Sale si no es un operador.
with MainForm.DataControl do
     begin
     CreateFilterOfCapacity(op, SpinEditGB1.Value, CheckBoxFilterCapacity.Checked);
     ApplyFilters;
     end;
end;

//-----------------------------------------------------------------------------
// Activa o desactiva el filtro de datos por GB copiados.
//-----------------------------------------------------------------------------
procedure TFormInterface.CheckBoxFilterCopyClick(Sender: TObject);
begin
ComboBoxOp2.Enabled := CheckBoxFilterCopy.Checked;
SpinEditGB2.Enabled := CheckBoxFilterCopy.Checked;
SpinEditGB2Change(Sender);
end;

procedure TFormInterface.ComboBoxOp2Change(Sender: TObject);
begin
SpinEditGB2Change(Sender);
end;

procedure TFormInterface.SpinEditGB2Change(Sender: TObject);
var op: String;
begin
with ComboBoxOp2 do
     if Text = Items[0] then op := '>' else                           //'Mayor que'
        if Text = Items[1] then op := '<' else                        //'Menor que'
           if Text = Items[2] then op := '>=' else                    //'Mayor o igual a'
              if Text = Items[3] then op := '<=' else                 //'Menor o igual a'
                 if Text = Items[4] then op := '=' else               //'Igual a'
                    if Text = Items[5] then op := '<>' else           //'Diferente de'
                       Exit;                                          //Sale si no es un operador.
with MainForm.DataControl do
     begin
     CreateFilterOfCopy(op, SpinEditGB2.Value, CheckBoxFilterCopy.Checked);
     ApplyFilters;
     end;
end;

//-----------------------------------------------------------------------------
// Inicia algunos componentes visuales.
//-----------------------------------------------------------------------------
procedure TFormInterface.FormShow(Sender: TObject);
begin
//Establece los valores iniciales.
ComboBoxDateRank.ItemIndex := 0;
ComboBoxOp1.ItemIndex := 0;
ComboBoxOp2.ItemIndex := 0;
CheckBoxFilterCapacity.Checked := False;
CheckBoxFilterCopy.Checked := False;

//Llama a los eventos responsables de ajustar los datos.
ComboBoxDateRankChange(Sender);
DTPFromChange(Sender);
DTPToChange(Sender);
CheckBoxFilterCapacityClick(Sender);
CheckBoxFilterCopyClick(Sender);
CheckBoxFilterDateClick(Sender);
end;

//-----------------------------------------------------------------------------
// Cierra el formulario de interface.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Muestra la ventana de procedencia de la aplicación.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionOriginExecute(Sender: TObject);
begin
with TFormOrigin.Create(nil) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Muestra la ayuda de la aplicación.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionUtilisationExecute(Sender: TObject);
var HF1, HF2: String;
begin
HF1 := MainForm.AppDirectory + cHF1;
HF2 := MainForm.AppDirectory + cHF2;
if FileExists(HF1) then
   ShellExecute(Handle, nil, PChar(HF1), '', '', SW_SHOWNORMAL)
else
   if FileExists(HF2) then
      ShellExecute(Handle, nil, PChar(HF2), '', '', SW_SHOWNORMAL);
end;

//-----------------------------------------------------------------------------
// Redimensiona las columnas del DBGrid automáticamente.
//
// Hace que las columnas se distribuyan de manera tal que rellenen todo
// el espacio cliente del DBGrid. Solamente redimensiona las colamnas que
// tengan en la propiedad "Tag" un valor diferente de cero.
//-----------------------------------------------------------------------------
procedure FixDBGridColumnsWidth(const DBGrid: TDBGrid);
var i : integer;
    TotWidth : integer;
    VarWidth : integer;
    ResizableColumnCount : integer;
    AColumn : TColumn;
begin
TotWidth := 0;
ResizableColumnCount := 0;
for i := 0 to -1 + DBGrid.Columns.Count do
    begin
    TotWidth := TotWidth + DBGrid.Columns[i].Width;
    if DBGrid.Columns[i].Field.Tag <> 0 then
       Inc(ResizableColumnCount);
    end;
if dgColLines in DBGrid.Options then
   TotWidth := TotWidth + DBGrid.Columns.Count;
if dgIndicator in DBGrid.Options then
   TotWidth := TotWidth + IndicatorWidth;
VarWidth :=  DBGrid.ClientWidth - TotWidth;
if ResizableColumnCount > 0 then
   VarWidth := varWidth div ResizableColumnCount;
for i := 0 to -1 + DBGrid.Columns.Count do
    begin
    AColumn := DBGrid.Columns[i];
    if AColumn.Field.Tag <> 0 then
       begin
       AColumn.Width := AColumn.Width + VarWidth;
       if AColumn.Width < AColumn.Field.Tag then
          AColumn.Width := AColumn.Field.Tag;
       end;
    end;
end;

procedure TFormInterface.FormResize(Sender: TObject);
begin
FixDBGridColumnsWidth(DBGrid1);
end;

//-----------------------------------------------------------------------------
// Vacía el contenido de la base de datos.
// Si no hay licencia válida, no se permite vaciar la base de datos.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionClearDBExecute(Sender: TObject);
begin
if Application.MessageBox(PChar('Desea vaciar la base de datos completa.'), PChar('CONFIRMAR'), MB_YESNO) = ID_YES then
   begin
   CheckBoxFilterDate.Checked := False;
   CheckBoxFilterCapacity.Checked := False;
   CheckBoxFilterCopy.Checked := False;
   Application.ProcessMessages;
   try
      DataSource1.Enabled := False;              //Desactiva la fuente de datos.
      if MainForm.DataControl.ClearDB then
         begin
         Application.MessageBox(PChar('Se ha vaciado la base de datos.'),
                                PChar('OPERACIÓN TERMINADA'),               
                                MB_OK);
         end;
      DataSource1.Enabled := True;               //Activa la fuente de datos.
   except
   end;
   StatusBar1.Panels[2].Text := '';
   end;
end;

//-----------------------------------------------------------------------------
// Controla la áctivación de los filtros.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionFilterDateExecute(Sender: TObject);
begin
with CheckBoxFilterDate do Checked := not Checked;
end;

procedure TFormInterface.ActionFilterDateUpdate(Sender: TObject);
begin
TAction(Sender).Checked := CheckBoxFilterDate.Checked;
TAction(Sender).Enabled := MainMenuEnable;
end;

procedure TFormInterface.ActionFilterCapacityExecute(Sender: TObject);
begin
with CheckBoxFilterCapacity do Checked := not Checked;
end;

procedure TFormInterface.ActionFilterCapacityUpdate(Sender: TObject);
begin
TAction(Sender).Checked := CheckBoxFilterCapacity.Checked;
TAction(Sender).Enabled := MainMenuEnable;
end;

procedure TFormInterface.ActionFilterCopyedExecute(Sender: TObject);
begin
with CheckBoxFilterCopy do Checked := not Checked;
end;

procedure TFormInterface.ActionFilterCopyedUpdate(Sender: TObject);
begin
TAction(Sender).Checked := CheckBoxFilterCopy.Checked;
TAction(Sender).Enabled := MainMenuEnable;
end;

//-----------------------------------------------------------------------------
// Activa o desactiva los controles de la ventana.
//-----------------------------------------------------------------------------
procedure TFormInterface.EnableControls(val: Boolean);
begin
MainMenuEnable := val;
ToolBar1.Enabled := val;
StatusBar1.Enabled := val;
DBGrid1.Enabled := val;
end;

//-----------------------------------------------------------------------------
// Muestra el estado de la operación que se realiza con los datos..
// Permite al usuario detener la operación pulsando la tecla ESCAPE.
//-----------------------------------------------------------------------------
procedure TFormInterface.OnProcessEvent(n: Integer; t: Integer);
begin
if t > 0 then
   StatusBar1.Panels[1].Text := IntToStr(Round((n/t)*100)) + '%';
Application.ProcessMessages;
if MainForm.DataControl.Continue then                     //Si se está procesando.
   if (GetKeyState(VK_Escape) and 128 = 128) then         //Si se presiona la tecla ESCAPE....
      begin
      if Application.MessageBox(PChar('¿Desea abortar la operación?'),      //Texto que se muestra.
                                PChar('ATENCIÓN...'),                       //Título del diálogo.
                                MB_YESNO
                                ) = ID_YES then           //Si el usuario presiona YES:
         MainForm.DataControl.Continue := False;          //Detiene el procesamiento.
      end;
end;

//-----------------------------------------------------------------------------
// Muestra el reporte de las estadísticas de las conexiones.
// Luego de calcular las estadísticas, las muestra en un formulario.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionShowStatisticsExecute(Sender: TObject);
var st: TStatistics;
begin
EnableControls(False);                            //Desactiva los controles visuales.

try
   DataSource1.Enabled := False;                     //Desactiva la fuente de datos.
   StatusBar1.Panels[2].Text := 'Presione la tecla ESCAPE para detener el procesamiento.';
   if MainForm.DataControl.GetStatistic(st) then     //Si se pueden obtener las estadísticas...
      begin
      DataSource1.Enabled := True;                   //Activa primero la fuente de datos.
      StatusBar1.Panels[2].Text := '';
      with TFormViewStatistics.Create(nil) do        //Crea el formulario.
           begin
           Statistics := st;                         //Copia las estadísticas al formulario.
           ShowModal;                                //Muestra el formulario.
           end;
      end;
   DataSource1.Enabled := True;                      //Activa la fuente de datos por si no se activó anteriormente.
except
end;
StatusBar1.Panels[2].Text := '';

EnableControls(True);                             //Activa los controles visuales.
DBGrid1.Invalidate;                               //Hace que se dibuje nuevamente el DBGrid.
Application.ProcessMessages;                      //Ordena procesar los mensajes de Windows.
StatusBar1.Panels[1].Text := '';
end;

//-----------------------------------------------------------------------------
// Exporta los datos al formato indicado.
//-----------------------------------------------------------------------------
procedure TFormInterface.ExportToFile(FType: TFilesTypes);
var r: Boolean;
begin
r := False;
EnableControls(False);                            //Desactiva los controles visuales.

//Configura el diálogo según el tipo de fichero al que se va a exportar.
case FType of
     ftTXT:         //Para exportar como fichero de texto.
        begin
        SaveDialog2.DefaultExt := 'txt';
        SaveDialog2.Filter := 'Fichero TXT|*.txt';
        end;
     ftCSV:         //Para exportar como fichero que se abre con Excel.
        begin
        SaveDialog2.DefaultExt := 'csv';
        SaveDialog2.Filter := 'Fichero CSV|*.csv';
        end;
     ftINI:         //Para exportar como fichero de texto de configuración.
        begin
        SaveDialog2.DefaultExt := 'ini';
        SaveDialog2.Filter := 'Fichero INI|*.ini';
        end;
     ftHTML:        //Para exportar como página Web.
        begin
        SaveDialog2.DefaultExt := 'html';
        SaveDialog2.Filter := 'Fichero HTML|*.html';
        end;
     ftXML:         //Para exportar como documento XML.
        begin
        SaveDialog2.DefaultExt := 'xml';
        SaveDialog2.Filter := 'Fichero XML|*.xml';
        end;
     end;

//Exporta el tipo de fichero.
if SaveDialog2.Execute then
   try
      DataSource1.Enabled := False;                                               //Desactiva el datasource.
      StatusBar1.Panels[2].Text := 'Presione la tecla ESCAPE para detener la exportación.';
      case FType of
           ftTXT:  r := MainForm.DataControl.ExportToTXT(SaveDialog2.FileName);   //Para exportar como fichero de texto.
           ftCSV:  r := MainForm.DataControl.ExportToCSV(SaveDialog2.FileName);   //Para exportar como fichero que se abre con Excel.
           ftHTML: r := MainForm.DataControl.ExportToHTML(SaveDialog2.FileName);  //Para exportar como página Web.
           ftXML:  r := MainForm.DataControl.ExportToXML(SaveDialog2.FileName);   //Para exportar como documento XML.
           ftINI:  r := MainForm.DataControl.ExportToINI(SaveDialog2.FileName);   //Para exportar como fichero INI.
           else r := False;
           end;
      DataSource1.Enabled := True;                                                //Activa el datasource.
      StatusBar1.Panels[2].Text := '';
   except
   end;
   
EnableControls(True);                             //Activa los controles visuales.
DBGrid1.Invalidate;                               //Hace que se dibuje nuevamente el DBGrid.
Application.ProcessMessages;                      //Ordena procesar los mensajes de Windows.
if r then
   begin
   Application.MessageBox(PChar('Se han exportado los datos.'),    //Indica que se realizó correctamente la operación.
                          PChar('OPERACIÓN TERMINADA'),
                          MB_OK);
   end;
StatusBar1.Panels[1].Text := '';
end;

//-----------------------------------------------------------------------------
// Exportan los datos a un fichero con formato TXT, CSV o HTML.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionExportToTXTExecute(Sender: TObject);
begin
ExportToFile(ftTXT);
end;

procedure TFormInterface.ActionExportToCSVExecute(Sender: TObject);
begin
ExportToFile(ftCSV);
end;

procedure TFormInterface.ActionExportToINIExecute(Sender: TObject);
begin
ExportToFile(ftINI);
end;

procedure TFormInterface.ActionExportToHTMLExecute(Sender: TObject);
begin
ExportToFile(ftHTML);
end;

procedure TFormInterface.ActionExportToXMLExecute(Sender: TObject);
begin
ExportToFile(ftXML);
end;

//-----------------------------------------------------------------------------
// Permite ver los datos de un registro individualmente.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionViewDetailsExecute(Sender: TObject);
begin
with TFormViewDetail.Create(Self) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Finaliza la ejecución del monitoreo hasta el próximo reinicio.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionFinallyExecute(Sender: TObject);
begin
if Application.MessageBox(PChar('Desea detener el programa hasta el proximo reinicio del sistema.'), PChar('CONFIRMAR'), MB_YESNO) = ID_YES then
   begin
   MainForm.EndMonitor;           //Termina el monitoreo.
   Application.Terminate;         //Cierra la aplicación.
   end;
end;

//-----------------------------------------------------------------------------
// Para mostrar u ocultar la selección por similitud.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionViewSelectExecute(Sender: TObject);
begin
FDrawSimilarSelected := not FDrawSimilarSelected;
DBGrid1.Invalidate;
end;

procedure TFormInterface.ActionViewSelectUpdate(Sender: TObject);
begin
TAction(Sender).Checked := FDrawSimilarSelected;
end;

//-----------------------------------------------------------------------------
// Para mostrar u ocultar el coloreado según Gb copiados.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionColorFromCopyExecute(Sender: TObject);
begin
FDrawGbCopyed := not FDrawGbCopyed;
DBGrid1.Invalidate;
end;

procedure TFormInterface.ActionColorFromCopyUpdate(Sender: TObject);
begin
TAction(Sender).Checked := FDrawGbCopyed;
end;

//-----------------------------------------------------------------------------
// Abre un diálogo que permite buscar cadenas de texto en la tabla de datos.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionSearchTextExecute(Sender: TObject);
var Text: String;
    CaseSensitive, Exact: Boolean;
    Fill: Integer;
    r: Boolean;
begin
CaseSensitive := False;
Exact := False;
if InputQuerySearchText(Text, CaseSensitive, Exact) then
   begin
   StatusBar1.Panels[2].Text := 'Presione la tecla ESCAPE para detener la búsqueda.';
   DataSource1.Enabled := False;                     //Desactiva la fuente de datos.
   r := MainForm.DataControl.SearchText(Text, CaseSensitive, Exact, Fill);
   DataSource1.Enabled := True;                      //Activa la fuente de datos.
   StatusBar1.Panels[2].Text := '';
   if r then
      begin
      DBGrid1.SetFocus;                         //Pone el foco en el DBGrid.
      DBGrid1.SelectedIndex := Fill;            //Indica el campo donde se encontró la coincidencia.
      end
   else
      begin
      Application.MessageBox(PChar('No se ha encontrado coincidencia.'),
                             PChar('ATENCIÓN'),
                             MB_OK);
      MainForm.DataControl.ClientDataSet.First;
      end;
   StatusBar1.Panels[1].Text := '';
   end;
end;

//-----------------------------------------------------------------------------
// Permite buscar las siguientes cadenas de texto en la tabla de datos. (F3)
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionSearchNexTextExecute(Sender: TObject);
var Fill: Integer;
    r: Boolean;
begin
DataSource1.Enabled := False;                       //Desactiva la fuente de datos.
StatusBar1.Panels[2].Text := 'Presione la tecla ESCAPE para detener la búsqueda.';
r := MainForm.DataControl.SearchNext(False, fill);  //Realiza la búsqueda.
DataSource1.Enabled := True;                        //Activa la fuente de datos.
StatusBar1.Panels[2].Text := '';
if r then
   begin
   DBGrid1.SetFocus;                         //Pone el foco en el DBGrid.
   DBGrid1.SelectedIndex := Fill;            //Indica el campo donde se encontró la coincidencia.
   end
else
   begin
   Application.MessageBox(PChar('No se han encontrado más coincidencias.'),
                          PChar('ATENCIÓN'),
                          MB_OK);
   MainForm.DataControl.ClientDataSet.First;
   end;
StatusBar1.Panels[1].Text := '';
end;

//-----------------------------------------------------------------------------
// Desplaza el puntero al inicio de la tabla de datos.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionBeginExecute(Sender: TObject);
begin
try
   MainForm.DataControl.ClientDataSet.First;   //Va a la primera posición.
   DBGrid1.SetFocus;                           //Pone el foco en el DBGrid.
   DBGrid1.SelectedIndex := 0;                 //Indica el primer campo de la fila.
except
end;
end;

//-----------------------------------------------------------------------------
// Desplaza el puntero al final de la tabla de datos.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionEndExecute(Sender: TObject);
begin
try
   MainForm.DataControl.ClientDataSet.Last;    //Va a la primera posición.
   DBGrid1.SetFocus;                           //Pone el foco en el DBGrid.
   DBGrid1.SelectedIndex := 0;                 //Indica el primer campo de la fila.
except
end;
end;

//-----------------------------------------------------------------------------
// Recorre con el puntero toda la tabla de datos.
// Se puede parar con la tecla ESCAPE o SPACE_BAR
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionTraverseExecute(Sender: TObject);
begin
EnableControls(False);                            //Desactiva los controles visuales.

StatusBar1.Panels[2].Text := 'Presione la tecla ESCAPE para detener el recorrido.';
MainForm.DataControl.Traverse(10, True);
StatusBar1.Panels[2].Text := '';
StatusBar1.Panels[1].Text := '';

EnableControls(True);                             //Desactiva los controles visuales.
end;

//-----------------------------------------------------------------------------
// Muestra los dispositivos actualmente conectados y que aún
// no están registrados en la base de datos del programa.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionViewPresentDevicesExecute(Sender: TObject);
begin
with TFormViewPresentDevices.Create(Self) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Abre la herramienta de conversión de datos.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionConvertExecute(Sender: TObject);
begin
with TFormConvert.Create(Self) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Activa o desactiva las acciones según el estado del menu principal.
//-----------------------------------------------------------------------------
procedure TFormInterface.ActionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := MainMenuEnable;
end;

//-----------------------------------------------------------------------------
// Muestra la cantidad de registros en la barra de estado.
// Este evento se dispara cuando los datos del DataSet cambian.
//-----------------------------------------------------------------------------
procedure TFormInterface.DataSource1DataChange(Sender: TObject; Field: TField);
begin
try
   if MainForm.DataControl.Lock then Exit;
   StatusBar1.Panels[0].Text := 'Registros: ' + IntToStr(MainForm.DataControl.GetDevicesTotal);
except
end;
end;

end.


