unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DateTimePicker, DateUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Calendario: TDateTimePicker;
    Entra_fecha_juliana: TEdit;
    Sale_fecha_gregoriana: TEdit;
    Sale_juliana: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure CalendarioChange(Sender: TObject);
    procedure Entra_fecha_julianaChange(Sender: TObject);
    procedure Entra_fecha_julianaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); // Se agrega para el evento FormCreate
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CalendarioChange(Sender: TObject);
//01.--DEVUELVE GREGORIANO A JULIANO--
var
  FechaGregoriana: TDate;
  AnioGregoriano: Word;
  FechaInicioAnio: TDate; // Renombrado para mayor claridad
  DiaDelAnioGregoriano: Integer;
  FechaJulianaFinal: string;
begin
  // Obtiene la fecha gregoriana
  FechaGregoriana := Calendario.DateTime;

  // Extrae el año
  AnioGregoriano := YearOf(FechaGregoriana);

  // Calcula el primer día del año (1 de enero del año actual)
  FechaInicioAnio := EncodeDate(AnioGregoriano, 1, 1);

  // Calcula el número del día del año (desde 1 de enero)
  // Se suma 1 porque el cálculo de la diferencia de días da 0 para el 1 de enero.
  DiaDelAnioGregoriano := Trunc(FechaGregoriana - FechaInicioAnio) + 1;

  // Concatena año + día del año en formato YYYYDDD (000)
  FechaJulianaFinal := IntToStr(AnioGregoriano) + FormatFloat('000', DiaDelAnioGregoriano);

  // Muestra formato juliano instantáneamente
  Sale_juliana.Text := FechaJulianaFinal;
end;

procedure TForm1.Entra_fecha_julianaChange(Sender: TObject);
// 02.--DEVUELVE JULIANO A GREGORIANO--
var
  AnioJulianoInt, DiaJulianoInt: Integer;
  FechaJulianaStr: string; // Renombrado para evitar confusión con TDateTime
  FechaGregoriana: TDateTime;
begin
  // Elimina espacios en blanco iniciales y finales del texto
  FechaJulianaStr := Trim(Entra_fecha_juliana.Text);

  // Valida la longitud: debe ser exactamente 7 dígitos
  if Length(FechaJulianaStr) = 7 then
  begin
    // Validar que la entrada sea solo numérica
    if not TryStrToInt(Copy(FechaJulianaStr, 1, 4), AnioJulianoInt) or
       not TryStrToInt(Copy(FechaJulianaStr, 5, 3), DiaJulianoInt) then
    begin
      Sale_fecha_gregoriana.Text := 'Formato inválido (solo números)';
      Exit;
    end;

    // Valida rango de día
    // Se utiliza IsLeapYear para contemplar el día 366 en años bisiestos
    if (DiaJulianoInt < 1) or (DiaJulianoInt > 366) or
       ((DiaJulianoInt = 366) and not IsLeapYear(AnioJulianoInt)) then
    begin
      Sale_fecha_gregoriana.Text := 'Día o año inválido';
      Exit;
    end;

    // Convertir a fecha gregoriana
    FechaGregoriana := EncodeDate(AnioJulianoInt, 1, 1) + (DiaJulianoInt - 1);

    // Mostrar la fecha en formato dd/mm/yyyy
    Sale_fecha_gregoriana.Text := FormatDateTime('dd/mm/yyyy', FechaGregoriana);
  end
  else
  begin
    // Si no tiene 7 dígitos, dejamos el campo de salida con mensaje
    Sale_fecha_gregoriana.Text := 'Debe tener 7 dígitos';
  end;
end;

procedure TForm1.Entra_fecha_julianaClick(Sender: TObject);
//03.--LIMPIA CAMPO ON CLICK JULIANO--
begin
  Entra_fecha_juliana.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Al crear el formulario, llama a CalendarioChange para inicializar el campo juliano
  CalendarioChange(Self);
end;

end.
