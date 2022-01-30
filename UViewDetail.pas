
///////////////////////////////////////////////////////////////////////////////
// Nombre: UViewDetail
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 18/08/2016
// Objetivo: Implementa el formulario que muestra los detalles
//           de la conexión de un dispositivo de almacenamiento.
///////////////////////////////////////////////////////////////////////////////

unit UViewDetail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, UFormStayOnTop, UStoreDataControl, StdCtrls, Mask,
  DBCtrls, ExtCtrls, DB, UAntiReversing, ToolWin, ActnList, UCommon,
  TeeProcs, TeEngine, Chart, Series, Grids;

type
  TFormViewDetail = class(TForm)
    DataSource1: TDataSource;
    ActionList1: TActionList;
    ActionSaveAsText: TAction;
    SaveDialog1: TSaveDialog;
    ActionClose: TAction;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    RichEdit1: TRichEdit;
    TreeView1: TTreeView;
    Chart4: TChart;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    DBNavigator1: TDBNavigator;
    ToolButton1: TToolButton;
    Timer1: TTimer;
    ActionTraverseDat: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
    procedure FormResize(Sender: TObject);
    procedure ActionSaveAsTextExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionSaveAsTextUpdate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ActionTraverseDatExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    vNumber, vDuration: Integer;
    vCapacityGB, vInitialGB, vMinimalGB, vFinalGB, vCopyedGB: Double;
    vUnit, vName, vType, vSerial, vFiles: String;
    vConection: TDateTime;

    procedure GetDate;
    procedure ShowDeviceSpace;
    procedure ShowFiles;
    procedure ShowResume;
    procedure SetColor(RichEdit: TRichEdit; str: String; Color: TColor; Style: TFontStyles);
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses UFormMain;

//-----------------------------------------------------------------------------
// Enlaza los controles a los datos y prepara el formulario.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.FormCreate(Sender: TObject);
begin
//Define la fuente de los datos.
try
   DataSource1.DataSet := MainForm.DataControl.ClientDataSet;
   DBNavigator1.DataSource := DataSource1;
except
   MessageBeep(MB_ICONERROR);
end;
Caption := DecStr(cAppTitle) + ' (Detalles)';
end;

//-----------------------------------------------------------------------------
// Muestra los datos de la conexión sobre la que está parado el cursor.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.FormShow(Sender: TObject);
begin
GetDate;
ShowDeviceSpace;
ShowFiles;
ShowResume;
end;

//-----------------------------------------------------------------------------
// Obtiene los datos de la conexión del dispositivo.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.GetDate;
begin
try
   vNumber := MainForm.DataControl.ClientDataSet.FieldByName(fNumber).AsInteger;
   vUnit := MainForm.DataControl.ClientDataSet.FieldByName(fUnit).AsString;
   vName := MainForm.DataControl.ClientDataSet.FieldByName(fName).AsString;
   vType := MainForm.DataControl.ClientDataSet.FieldByName(fType).AsString;
   vSerial := MainForm.DataControl.ClientDataSet.FieldByName(fSerial).AsString;
   vConection := MainForm.DataControl.ClientDataSet.FieldByName(fConection).AsDateTime;
   vDuration := MainForm.DataControl.ClientDataSet.FieldByName(fDurationMin).AsInteger;
   vCapacityGB := MainForm.DataControl.ClientDataSet.FieldByName(fCapacityGB).AsFloat;
   vCopyedGB := MainForm.DataControl.ClientDataSet.FieldByName(fCopyedGB).AsFloat;
   vInitialGB := MainForm.DataControl.ClientDataSet.FieldByName(fInitialGB).AsFloat;
   vMinimalGB := MainForm.DataControl.ClientDataSet.FieldByName(fMinimalGB).AsFloat;
   vFinalGB := MainForm.DataControl.ClientDataSet.FieldByName(fFinalGB).AsFloat;
except
   MessageBeep(MB_ICONERROR);
end;
end;

//-----------------------------------------------------------------------------
// Muestra el gráfico de los acumulados de los ficheros según su tipo.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.ShowDeviceSpace;
var v: Double;
begin
Chart4.FreeAllSeries;

Chart4.Legend.Visible := True;
Chart4.View3D := True;

TStringList(Chart4.Title.Text).Clear;
TStringList(Chart4.Title.Text).Add('[' + DateTimeToStr(vConection) + '] ' + vSerial + ' (' + vName + ') ' + ToXByte(vCapacityGB));

Chart4.TopAxis.Labels := False;
Chart4.LeftAxis.LabelStyle := talValue;
Chart4.BottomAxis.LabelStyle := talText;

Chart4.AddSeries(TPieSeries.Create(Self));
TPieSeries(Chart4.Series[0]).Marks.Style := smsLabelValue;
TPieSeries(Chart4.Series[0]).Marks.Arrow.Color := clMaroon;
Chart4.Series[0].Clear;
Chart4.Series[0].Title := 'Espacios';
Chart4.Series[0].Identifier := Chart4.Series[0].Title;

Chart4.Series[0].AddY(vMinimalGB, 'GB Previos ', clGreen);
Chart4.Series[0].AddY(vCopyedGB, 'GB Copiados ', clRed);   
v := vCapacityGB - (vFinalGB);
Chart4.Series[0].AddY(v, 'GB Libres ', clWhite);   
end;

//-----------------------------------------------------------------------------
// Muestra los tipos de ficheros y sus cantidades.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.ShowFiles;
var f, FTypes, FCount: TStrings;
    n, m, i, c: Integer;
    ext, N1: String;
    Node, NodeExt: TTreeNode;
begin
TreeView1.Items.Clear;
f := TStringList.Create;                 //Crea el manejador de listas.
f.Text := vFiles;                        //Copia la lista al menejador.
if f.Count > 0 then
   begin
   FTypes := TStringList.Create;         //Inicia los acumulados de
   FCount := TStringList.Create;         //cantidades de tipos de ficheros.
   FTypes.Clear;
   FCount.Clear;
   for n := 0 to f.Count - 1 do
       if f[n] <> '' then                //Si la cadena existe...
          begin
          ext := ExtractFileExt(f[n]);   //Obtiene la extensión del tipo de fichero.
          i := FTypes.IndexOf(ext);      //Obtiene el índice del tipo de fichero.
          if i = -1 then
             begin
             FTypes.Add(ext);            //Agrega el nuevo tipo de fichero.
             FCount.Add(IntToStr(1));    //Lo cuenta como primero.
             end
          else                           //Si el tipo de fichero ya existe...
             begin
             c := StrToInt(FCount[i]);   //Obtiene el contador de la lista.
             Inc(c);                     //Lo incrementa.
             FCount[i] := IntToStr(c);   //Lo coloca nuevamente en la lista.
             end;
          end;

   //Muestra el árbol con los tipos de ficheros.
   N1 := 'Tipos de ficheros = ' + IntToStr(FTypes.Count);  
   Node := TreeView1.Items.Add(TTreeNode.Create(TreeView1.Items), N1);
   if FTypes.Count > 0 then
      for n := 0 to FTypes.Count - 1 do
          begin
          NodeExt := TreeView1.Items.AddChild(Node, UpperCase(FTypes[n]) + ' = ' + FCount[n]);
          for m := 0 to f.Count - 1 do
              begin
              ext := UpperCase(ExtractFileExt(f[m]));       //Obtiene la extensión del tipo de fichero.
              if UpperCase(FTypes[n]) = ext then            //Si el fichero es de la extensión específica...
                 TreeView1.Items.AddChild(NodeExt, f[m]);   //Lo agrega a la lista.
              end;
          end;
   Node.Expand(False);
   FTypes.Free;
   FCount.Free;
   end;
f.Free;
end;

//-----------------------------------------------------------------------------
// Resalta una palabra en el color especificado.
// OJO: Funciona solo con cadenas de más de un caracter.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.SetColor(RichEdit: TRichEdit; str: String; Color: TColor; Style: TFontStyles);
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
// Muestra en un memo la información detallada de la conexión.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.ShowResume;
const c1 = 'Número de evento: ';
const c2 = 'Unidad asignada: ';
const c3 = 'Nombre del dispositivo: ';
const c4 = 'Tipo de dispositivo: ';
const c5 = 'Número de serie: ';
const c6 = 'Capacidad de almacenamiento: ';
const c7 = 'Inicio de la conexión: ';
const c8 = 'Duración de la conexión: ';
const c9 = 'Información copiada al dispositivo: ';
const c10 = 'Contenido al inicio de la conexión: ';
const c11 = 'Contenido mínimo que llegó a tener: ';
const c12 = 'Contenido al final de la conexión: ';
const c13 = 'Ficheros copiados: ';
begin
//muestra la información en el memo...
RichEdit1.Lines.Clear;
RichEdit1.Lines.Add(c1 + IntToStr(vNumber));
RichEdit1.Lines.Add(c2 + UpperCase(vUnit) + ':\');
RichEdit1.Lines.Add(c3 + vName);
RichEdit1.Lines.Add(c4 + vType);
RichEdit1.Lines.Add(c5 + vSerial);
RichEdit1.Lines.Add(c6 + ToXByte(vCapacityGB));
RichEdit1.Lines.Add(c7 + DateTimeToStr(vConection));
RichEdit1.Lines.Add(c8 + ToXTime(vDuration));
RichEdit1.Lines.Add(c9 + ToXByte(vCopyedGB));
RichEdit1.Lines.Add(c10 + ToXByte(vInitialGB));
RichEdit1.Lines.Add(c11 + ToXByte(vMinimalGB));
RichEdit1.Lines.Add(c12 + ToXByte(vFinalGB));
RichEdit1.Lines.Add('');                          //Crea un salto de linea...
RichEdit1.Lines.Add(c13);
RichEdit1.Lines.Add(vFiles);                      //Muestra una lista de los ficheros copiados.

//Resalta los textos con colores...
SetColor(RichEdit1, c1, clNavy, []);
SetColor(RichEdit1, c2, clNavy, []);
SetColor(RichEdit1, c3, clNavy, []);
SetColor(RichEdit1, c4, clNavy, []);
SetColor(RichEdit1, c5, clNavy, []);
SetColor(RichEdit1, c6, clNavy, []);
SetColor(RichEdit1, c7, clNavy, []);
SetColor(RichEdit1, c8, clNavy, []);
SetColor(RichEdit1, c9, clNavy, []);
SetColor(RichEdit1, c10, clNavy, []);
SetColor(RichEdit1, c11, clNavy, []);
SetColor(RichEdit1, c12, clNavy, []);
SetColor(RichEdit1, c13, clNavy, []);
SetColor(RichEdit1, ' Bytes', clNavy, []);
SetColor(RichEdit1, ' KB', clNavy, []);
SetColor(RichEdit1, ' MB', clNavy, []);
SetColor(RichEdit1, ' GB', clNavy, []);
SetColor(RichEdit1, ' TB', clNavy, []);
SetColor(RichEdit1, ' PB', clNavy, []);
SetColor(RichEdit1, ' EB', clNavy, []);
SetColor(RichEdit1, ' ZB', clNavy, []);
SetColor(RichEdit1, ' YB', clNavy, []);
SetColor(RichEdit1, ' Minutos', clNavy, []);
SetColor(RichEdit1, ' Horas', clNavy, []);
end;

//-----------------------------------------------------------------------------
// Muestra los datos de la conexión sobre la que está parado el cursor.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
begin
GetDate;
ShowDeviceSpace;
ShowFiles;
ShowResume;
FormInterface.StatusBar1.Invalidate;
end;

//-----------------------------------------------------------------------------
// Redibuja el RichEdit cada vez que se redimensiona el formulario.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.FormResize(Sender: TObject);
begin
RichEdit1.Invalidate;
end;

//-----------------------------------------------------------------------------
// Guarda el texto del RichEdit en un fichero.
// Abre un diálogo para que el usuario seleccione la ruta.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.ActionSaveAsTextExecute(Sender: TObject);
begin
case PageControl1.ActivePageIndex of
     0: Exit;
     1: begin
        SaveDialog1.Title := 'Guardar lista de ficheros';
        SaveDialog1.DefaultExt := 'txt';
        SaveDialog1.Filter := 'Ficheros de texto|*.txt|Todos los ficheros|*.*';
        if SaveDialog1.Execute then
           TreeView1.SaveToFile(SaveDialog1.FileName);
        end;
     2: begin
        SaveDialog1.Title := 'Guardar resumen';
        SaveDialog1.DefaultExt := 'rtf';
        SaveDialog1.Filter := 'Texto con formato|*.rtf|Todos los ficheros|*.*';
        if SaveDialog1.Execute then
           RichEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        end;
     end;
end;

//-----------------------------------------------------------------------------
// Cierra el formulario.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Activa o desactiva el botón de guardar.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.ActionSaveAsTextUpdate(Sender: TObject);
begin
ActionSaveAsText.Enabled := PageControl1.ActivePageIndex > 0;
end;

//-----------------------------------------------------------------------------
// Se dezplaza una posición en los datos.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.Timer1Timer(Sender: TObject);
begin
if not DataSource1.DataSet.Eof then
   begin
   DataSource1.DataSet.Next;
   GetDate;
   ShowDeviceSpace;
   ShowFiles;
   ShowResume;
   end;
end;

//-----------------------------------------------------------------------------
// Activa o desactiva el recorrido automático de los datos.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.ActionTraverseDatExecute(Sender: TObject);
begin
Timer1.Enabled := ActionTraverseDat.Checked;
end;

//-----------------------------------------------------------------------------
// Termina algunos componentes del formulario.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.FormDestroy(Sender: TObject);
begin
Timer1.Enabled := False;
Timer1.Free;
end;

//-----------------------------------------------------------------------------
// Para que el formulario se libere cuando se cierre.
//-----------------------------------------------------------------------------
procedure TFormViewDetail.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
