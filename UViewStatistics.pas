
///////////////////////////////////////////////////////////////////////////////
// Nombre: UViewStatistics
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 19/08/2016
// Objetivo: Implementa el formulario que muestra las estadísticas
//           de las conexiones de los dispositivos de almacenamiento.
///////////////////////////////////////////////////////////////////////////////

unit UViewStatistics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UStoreDataControl, UAntiReversing, ComCtrls, ActnList,
  ToolWin, UCOmmon, Menus, ExtCtrls, TeeProcs, TeEngine, Chart, Series,
  DbChart;

type
  TFormViewStatistics = class(TForm)
    SaveDialog1: TSaveDialog;
    ActionList1: TActionList;
    ActionClose: TAction;
    ActionSaveAsTXT: TAction;
    ActionSaveAsRTF: TAction;
    PopupMenu1: TPopupMenu;
    ActionCopy: TAction;
    ActionPrint: TAction;
    ActionSelectAll: TAction;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    RichEdit1: TRichEdit;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton3: TToolButton;
    ToolButton7: TToolButton;
    ToolButton6: TToolButton;
    Chart1: TChart;
    Chart2: TChart;
    Chart3: TChart;
    TabSheet4: TTabSheet;
    Chart6: TChart;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionSaveAsTXTExecute(Sender: TObject);
    procedure ActionSaveAsRTFExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure PageControl1Resize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetColor(RichEdit: TRichEdit; str: String; Color: TColor; Style: TFontStyles);
    function ExportToTXT(FileName: String): Boolean;   //Para exportar como fichero de texto.
    function ExportToRTF(FileName: String): Boolean;   //Para exportar como fichero que se abre Word.
    function ExportToFile(FType: TFilesTypes): Boolean;

    procedure ShowHourFrequencyPerWeekDays;
    procedure ShowHourFrequency;
    procedure ShowDaysFrequency;
    procedure ShowTypesDevicesCopy;
  public
    Statistics: TStatistics;
  end;

implementation

uses UFormMain;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Prepara el formulario para su funcionamiento.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.FormCreate(Sender: TObject);
begin
RichEdit1.ReadOnly := True;
Caption := DecStr(cAppTitle) + ' (Estadísticas)';
end;

//-----------------------------------------------------------------------------
// Muestra el gráfico de la distribución de frecuencia por días.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ShowDaysFrequency;
var m: Integer;
begin
Chart3.AnimatedZoom := True;
Chart3.AnimatedZoomSteps := 100;
Chart3.Legend.Visible := False;
Chart3.LeftAxis.Grid.Visible := False;
Chart3.BottomAxis.Grid.Visible := False;
Chart3.View3D := True;
Chart3.BackColor := cl3DLight;
TStringList(Chart3.Title.Text).Text := 'Dispositivos por días de la semana';
Chart3.TopAxis.Labels := False;
Chart3.LeftAxis.LabelStyle := talValue;
Chart3.BottomAxis.LabelStyle := talText;

Chart3.AddSeries(TBarSeries.Create(Self));
TBarSeries(Chart3.Series[0]).Marks.Visible := False;
Chart3.Series[0].Clear;
Chart3.Series[0].Title := 'Días';
Chart3.Series[0].Identifier := Chart3.Series[0].Title;
for m := 1 to 7 do
    Chart3.Series[0].AddY(Statistics.Days[m], GetDayName(m)[1], GetDayColor(m));
end;

//-----------------------------------------------------------------------------
// Muestra el gráfico de la distribución de frecuencia por horas.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ShowHourFrequency;
var m: Integer;
begin
Chart2.AnimatedZoom := True;
Chart2.AnimatedZoomSteps := 100;
Chart2.Legend.Visible := False;
Chart2.LeftAxis.Grid.Visible := False;
Chart2.BottomAxis.Grid.Visible := False;
Chart2.View3D := False;
Chart2.BackColor := cl3DLight;
TStringList(Chart2.Title.Text).Text := 'Dispositivos por horas';
Chart2.TopAxis.Labels := False;
Chart2.LeftAxis.LabelStyle := talValue;
Chart2.BottomAxis.LabelStyle := talText;

Chart2.AddSeries(TAreaSeries.Create(Self));
Chart2.Series[0].Clear;
Chart2.Series[0].Title := 'Horas';
Chart2.Series[0].Identifier := Chart2.Series[0].Title;
for m := 1 to 24 do
    Chart2.Series[0].AddY(Statistics.Hours[m], IntToStr(m));
end;

//-----------------------------------------------------------------------------
// Muestra el gráfico de la distribución de frecuencia por horas en cada día.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ShowHourFrequencyPerWeekDays;
var n, m: Integer;
begin
Chart1.AnimatedZoom := True;
Chart1.AnimatedZoomSteps := 100;
Chart1.Legend.Visible := True;
Chart1.LeftAxis.Grid.Visible := False;
Chart1.BottomAxis.Grid.Visible := False;
Chart1.View3D := False;
Chart1.BackColor := cl3DLight;
TStringList(Chart1.Title.Text).Text := 'Dispositivos por horas en cada día de la semana';
Chart1.TopAxis.Labels := False;
Chart1.LeftAxis.LabelStyle := talValue;
Chart1.BottomAxis.LabelStyle := talText;
Chart1.Legend.Visible := False;

for n := 1 to 7 do
    begin
    Chart1.AddSeries(TLineSeries.Create(Self));
    TLineSeries(Chart1.Series[n-1]).LinePen.Width := 3;
    Chart1.Series[n-1].Clear;
    Chart1.Series[n-1].Title := GetDayName(n);
    Chart1.Series[n-1].Identifier := Chart1.Series[n-1].Title;
    for m := 1 to 24 do
        Chart1.Series[n-1].AddY(Statistics.HoursPerDays[n, m], IntToStr(m), GetDayColor(n));
    end;
end;

//-----------------------------------------------------------------------------
// Muestra gráfico de cantidad de dispositivos que copiaron información.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ShowTypesDevicesCopy;
const c1 = 'Extraibles';
const c2 = 'Fijos';
const c3 = 'Remotos';
const c4 = 'CD/DVD';
const c5 = 'Virtuales';
const c6 = 'Otros';
begin
Chart6.Legend.Visible := True;
Chart6.Legend.Alignment := laTop;
Chart6.View3D := False;
TStringList(Chart6.Title.Text).Text := 'Dispositivos según su tipo';
Chart6.TopAxis.Labels := False;
Chart6.LeftAxis.LabelStyle := talValue;
Chart6.BottomAxis.LabelStyle := talText;

//Agrega la serie del total de los dispositivos.
Chart6.AddSeries(TBarSeries.Create(Self));
TBarSeries(Chart6.Series[0]).Marks.Style := smsValue;
TBarSeries(Chart6.Series[0]).Marks.Arrow.Color := clMaroon;
TBarSeries(Chart6.Series[0]).Marks.Visible := False;
Chart6.Series[0].Clear;
Chart6.Series[0].Title := 'No copiaron información';
Chart6.Series[0].Identifier := Chart6.Series[0].Title;
TBarSeries(Chart6.Series[0]).MultiBar := mbStacked;

Chart6.Series[0].AddY(Statistics.CountTotalRemovable - Statistics.CountCopyedRemovable, c1);
Chart6.Series[0].AddY(Statistics.CountTotalFixed - Statistics.CountCopyedFixed, c2);
Chart6.Series[0].AddY(Statistics.CountTotalRemote - Statistics.CountCopyedRemote, c3);
Chart6.Series[0].AddY(Statistics.CountTotalCDROM - Statistics.CountCopyedCDROM, c4);
Chart6.Series[0].AddY(Statistics.CountTotalRAMDisk - Statistics.CountCopyedRAMDisk, c5);
Chart6.Series[0].AddY(Statistics.CountTotalUnknow - Statistics.CountCopyedUnknow, c6);

//Agrega la serie de los dispositivos que copiaron información.
Chart6.AddSeries(TBarSeries.Create(Self));
TBarSeries(Chart6.Series[1]).Marks.Style := smsValue;
TBarSeries(Chart6.Series[1]).Marks.Arrow.Color := clMaroon;
TBarSeries(Chart6.Series[1]).Marks.Visible := True;
Chart6.Series[1].Clear;
Chart6.Series[1].Title := 'Copiaron información';
Chart6.Series[1].Identifier := Chart6.Series[1].Title;
TBarSeries(Chart6.Series[1]).MultiBar := mbStacked;

Chart6.Series[1].AddY(Statistics.CountCopyedRemovable, c1);
Chart6.Series[1].AddY(Statistics.CountCopyedFixed, c2);
Chart6.Series[1].AddY(Statistics.CountCopyedRemote, c3);
Chart6.Series[1].AddY(Statistics.CountCopyedCDROM, c4);
Chart6.Series[1].AddY(Statistics.CountCopyedRAMDisk, c5);
Chart6.Series[1].AddY(Statistics.CountCopyedUnknow, c6);

end;

//-----------------------------------------------------------------------------
// Resalta una palabra en el color especificado.
// OJO: Funciona solo con cadenas de más de un caracter.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.SetColor(RichEdit: TRichEdit; str: String; Color: TColor; Style: TFontStyles);
var Text: string;
    Position: integer;
begin
Text := RichEdit.Lines.Text;
repeat
   Position := AnsiPos(str, Text);
   RichEdit.SelStart := Position - 1;
   RichEdit.SelLength := Length(str);
   RichEdit.SelAttributes.Color := Color;
   RichEdit.SelAttributes.Style := Style;
   Text[Position + 1] := Chr(255);
   Position := AnsiPos(str, Text);
until (Position = 0);
RichEdit.SelStart := 0;
RichEdit.SelLength := 0;
end;

//-----------------------------------------------------------------------------
// Obtiene las estadísticas de los datos de las conexiones.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.FormShow(Sender: TObject);
const c1 = 'Cantidad de dispositivos que se conectaron: ';
const c2 = 'Cantidad de dispositivos que copiaron información: ';
const c3 = 'Cantidad de información copiada hacia los dispositivos: ';

const c4 = 'Tipos de dispositivos que copiaron información...';
const c5 = 'Extraibles: ';
const c6 = 'Fijos: ';
const c7 = 'Remotos: ';
const c8 = 'CD/DVD: ';
const c9 = 'Virtuales: ';
const c10 = 'Otros: ';
const c11 = 'ESTADÍSTICAS:';

const c12 = 'Acumulado por horas...';
const c13 = '00:00 - 03:00 = ';
const c14 = '03:00 - 06:00 = ';
const c15 = '06:00 - 09:00 = ';
const c16 = '09:00 - 12:00 = ';
const c17 = '12:00 - 15:00 = ';
const c18 = '15:00 - 18:00 = ';
const c19 = '18:00 - 21:00 = ';
const c20 = '21:00 - 23:59 = ';

const c21 = 'Acumulado por días de semana...';
const c22 = 'Lunes = ';
const c23 = 'Martes = ';
const c24 = 'Miércoles = ';
const c25 = 'Jueves = ';
const c26 = 'Viernes = ';
const c27 = 'Sábado = ';
const c28 = 'Domingo = ';
const cdt = ' de un total de ';

var n: Integer;
begin
RichEdit1.Clear;
with Statistics do
     begin
     RichEdit1.Lines.Add(c11);
     RichEdit1.Lines.Add('');
     RichEdit1.Lines.Add(c1 + IntToStr(CountTotalAllDevices));
     RichEdit1.Lines.Add(c2 + IntToStr(CountCopyedAllDevices));
     RichEdit1.Lines.Add(c3 + FloatToStrF(TotalCopyedAllDevices, ffFixed, 16, 2) + ' Gb');
     RichEdit1.Lines.Add('');
     RichEdit1.Lines.Add(c4);
     RichEdit1.Lines.Add(c5 + IntToStr(CountCopyedRemovable) + cdt + IntToStr(CountTotalRemovable));
     RichEdit1.Lines.Add(c6 + IntToStr(CountCopyedFixed)     + cdt + IntToStr(CountTotalFixed));
     RichEdit1.Lines.Add(c7 + IntToStr(CountCopyedRemote)    + cdt + IntToStr(CountTotalRemote));
     RichEdit1.Lines.Add(c8 + IntToStr(CountCopyedCDROM)     + cdt + IntToStr(CountTotalCDROM));
     RichEdit1.Lines.Add(c9 + IntToStr(CountCopyedRAMDisk)   + cdt + IntToStr(CountTotalRAMDisk));
     RichEdit1.Lines.Add(c10 + IntToStr(CountCopyedUnknow)   + cdt + IntToStr(CountTotalUnknow));
     RichEdit1.Lines.Add('');
     RichEdit1.Lines.Add(c12);   //'Acumulado por horas...'
     RichEdit1.Lines.Add(c13 + IntToStr(Hours[1] + Hours[2] + Hours[3]));       //'00:00 - 03:00 = '
     RichEdit1.Lines.Add(c14 + IntToStr(Hours[4] + Hours[5] + Hours[6]));       //'03:00 - 06:00 = '
     RichEdit1.Lines.Add(c15 + IntToStr(Hours[7] + Hours[8] + Hours[9]));       //'06:00 - 09:00 = '
     RichEdit1.Lines.Add(c16 + IntToStr(Hours[10] + Hours[11] + Hours[12]));    //'09:00 - 12:00 = '
     RichEdit1.Lines.Add(c17 + IntToStr(Hours[13] + Hours[14] + Hours[15]));    //'12:00 - 15:00 = '
     RichEdit1.Lines.Add(c18 + IntToStr(Hours[16] + Hours[17] + Hours[18]));    //'15:00 - 18:00 = '
     RichEdit1.Lines.Add(c19 + IntToStr(Hours[19] + Hours[20] + Hours[21]));    //'18:00 - 21:00 = '
     RichEdit1.Lines.Add(c20 + IntToStr(Hours[22] + Hours[23] + Hours[24]));    //'21:00 - 23:59 = '
     RichEdit1.Lines.Add('');
     RichEdit1.Lines.Add(c21);   //'Acumulado por días de semana...'
     RichEdit1.Lines.Add(c22 + IntToStr(Days[1]));     //'Lunes = '
     RichEdit1.Lines.Add(c23 + IntToStr(Days[2]));     //'Martes = '
     RichEdit1.Lines.Add(c24 + IntToStr(Days[3]));     //'Miércoles = '
     RichEdit1.Lines.Add(c25 + IntToStr(Days[4]));     //'Jueves = '
     RichEdit1.Lines.Add(c26 + IntToStr(Days[5]));     //'Viernes = '
     RichEdit1.Lines.Add(c27 + IntToStr(Days[6]));     //'Sábado = '
     RichEdit1.Lines.Add(c28 + IntToStr(Days[7]));     //'Domingo = '
     RichEdit1.Lines.Add('');
     end;

//Resalta los textos con colores...
SetColor(RichEdit1, c1, clNavy, []);
SetColor(RichEdit1, c2, clNavy, []);
SetColor(RichEdit1, c3, clNavy, []);
SetColor(RichEdit1, c4, clBlack, [fsBold]);
SetColor(RichEdit1, c5, clNavy, []);
SetColor(RichEdit1, c6, clNavy, []);
SetColor(RichEdit1, c7, clNavy, []);
SetColor(RichEdit1, c8, clNavy, []);
SetColor(RichEdit1, c9, clNavy, []);
SetColor(RichEdit1, c10, clNavy, []);
SetColor(RichEdit1, c11, clBlack, [fsBold]);
SetColor(RichEdit1, c12, clBlack, [fsBold]);
SetColor(RichEdit1, c13, clNavy, []);
SetColor(RichEdit1, c14, clNavy, []);
SetColor(RichEdit1, c15, clNavy, []);
SetColor(RichEdit1, c16, clNavy, []);
SetColor(RichEdit1, c17, clNavy, []);
SetColor(RichEdit1, c18, clNavy, []);
SetColor(RichEdit1, c19, clNavy, []);
SetColor(RichEdit1, c20, clNavy, []);
SetColor(RichEdit1, c21, clBlack, [fsBold]);
SetColor(RichEdit1, c22, clNavy, []);
SetColor(RichEdit1, c23, clNavy, []);
SetColor(RichEdit1, c24, clNavy, []);
SetColor(RichEdit1, c25, clNavy, []);
SetColor(RichEdit1, c26, clNavy, []);
SetColor(RichEdit1, c27, clNavy, []);
SetColor(RichEdit1, c28, clNavy, []);
SetColor(RichEdit1, cdt, clNavy, []);

//Muestra los gráficos de acumulados.
ShowHourFrequencyPerWeekDays;
ShowDaysFrequency;
ShowHourFrequency;
ShowTypesDevicesCopy;
end;

//-----------------------------------------------------------------------------
// Cierra la ventana.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Para exportar las estadísticas como fichero de texto.
//-----------------------------------------------------------------------------
function TFormViewStatistics.ExportToTXT(FileName: String): Boolean;
var t: TStrings;
begin
Result := False;
try
   t := TStringList.Create;
   t.Text := RichEdit1.Text;
   t.SaveToFile(FileName);
   Result := True;
except
   Result := False;   
end;   
end;

//-----------------------------------------------------------------------------
// Para exportar las estadísticas como fichero que se abre Word.
//-----------------------------------------------------------------------------
function TFormViewStatistics.ExportToRTF(FileName: String): Boolean;
begin
Result := False;
try
   RichEdit1.Lines.SaveToFile(FileName);
   Result := True;
except
   Result := False;   
end;
end;

//-----------------------------------------------------------------------------
// Exporta los datos al formato indicado.
//-----------------------------------------------------------------------------
function TFormViewStatistics.ExportToFile(FType: TFilesTypes): Boolean;
var r: Boolean;
begin
//Configura el diálogo según el tipo de fichero al que se va a exportar.
case FType of
     ftTXT:         //Para exportar como fichero de texto.
        begin
        SaveDialog1.DefaultExt := 'txt';
        SaveDialog1.Filter := 'Fichero TXT|*.txt';
        end;
     ftRTF:         //Para exportar como fichero que se abre con Word.
        begin
        SaveDialog1.DefaultExt := 'rtf';
        SaveDialog1.Filter := 'Fichero RTF|*.rtf';
        end;
     else
        Exit;
     end;

//Exporta el tipo de fichero.
if SaveDialog1.Execute then
   begin
   case FType of
        ftTXT:   r := ExportToTXT(SaveDialog1.FileName);   //Para exportar como fichero de texto.
        ftRTF:   r := ExportToRTF(SaveDialog1.FileName);   //Para exportar como fichero que se abre con Word.
        else r := False;
        end;
   end;
Result := r;
end;

//-----------------------------------------------------------------------------
// Permite guardar las estadísticas en un fichero TXT.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ActionSaveAsTXTExecute(Sender: TObject);
begin
ExportToFile(ftTXT);
end;

//-----------------------------------------------------------------------------
// Permite guardar las estadísticas en un fichero RTF.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ActionSaveAsRTFExecute(Sender: TObject);
begin
ExportToFile(ftRTF);
end;

//-----------------------------------------------------------------------------
// Copia el texto al portaqpapeles.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ActionCopyExecute(Sender: TObject);
begin
RichEdit1.CopyToClipboard;
end;

//-----------------------------------------------------------------------------
// Imprime el texto de la ventana.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ActionPrintExecute(Sender: TObject);
begin
RichEdit1.Print(DecStr(cAppTitle) + '-Estadísticas');
end;

//-----------------------------------------------------------------------------
// Selecciona todo el texto de la ventana.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.ActionSelectAllExecute(Sender: TObject);
begin
RichEdit1.SelectAll;
end;

//-----------------------------------------------------------------------------
// mOdifica los tamaños de los componentes del formulario.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.PageControl1Resize(Sender: TObject);
begin
//Chart2.Width := TabSheet2.ClientWidth div 2;
//Chart2.Height := TabSheet2.ClientHeight div 2;
//Chart3.Height := TabSheet2.ClientHeight div 2;

end;

//-----------------------------------------------------------------------------
// Para que el formulario se libere cuando se cierre.
//-----------------------------------------------------------------------------
procedure TFormViewStatistics.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
Action := caFree;
end;

end.
