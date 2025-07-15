unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil;

type

  { TForm1 }
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);

var
  RutaBase: String;
  NombreCarpetaPrincipal: String;
  RutaCompletaPrincipal: String;

//---Procedimiento anidado para creación recursiva de carpetas-----

  procedure CrearSubcarpetasRecursivamente(const RutaPadre: string; NivelActual: Integer);

  var
    NumCarpetasStr: string;
    NumCarpetas: Integer;
    i: Integer;
    NuevaRuta: string;
    NombreSubcarpeta: string;

  begin
    // Fin recursión si se alcanza el nivel máximo de subcarpetas (principal + 5)
    if NivelActual > 6 then
      Exit;

    //Preguntar cuántas carpetas crear en este nivel
    NumCarpetasStr := InputBox(
      'Crear subcarpetas de NIVEL ' + IntToStr(NivelActual),
      '¿Cuántas subcarpetas quieres crear dentro de la ruta:' + LineEnding + RutaPadre + '?' + LineEnding + '(Introduce 0 para no crear ninguna)',
      '0'
    );
    NumCarpetas := StrToIntDef(NumCarpetasStr, 0);
    // Si el usuario no quiere crear carpetas en este nivel, fin de ese nivel
    if NumCarpetas <= 0 then
      Exit;

    //Bucle para pedir nombres y crear cada carpeta
    for i := 1 to NumCarpetas do
    begin
      NombreSubcarpeta := InputBox(
        'Nombre carpeta (NIVEL ' + IntToStr(NivelActual) + ')',
        'Introduce el nombre para la subcarpeta ' + IntToStr(i) + ' de las ' + IntToStr(NumCarpetas) + ' que quieres crear dentro del NIVEL ' + IntToStr(NivelActual)+ ':',
        ''
      );
      // Solo proceder si el usuario introduce un nombre
      if Trim(NombreSubcarpeta) <> '' then
      begin
        // Construir la ruta completa para la nueva subcarpeta
        NuevaRuta := IncludeTrailingPathDelimiter(RutaPadre) + Trim(NombreSubcarpeta);
        // Intentar crear la carpeta
        if ForceDirectories(NuevaRuta) then
            begin
              // Si se crea con éxito, llamar recursivamente para el siguiente nivel
              CrearSubcarpetasRecursivamente(NuevaRuta, NivelActual + 1);
            end
        else
        begin
          ShowMessage('Error: No se pudo crear la carpeta: ' + NuevaRuta);
        end;
      end;
    end;
  end;
//---Fin del procedimiento anidado

begin //PROCEDIMIENTO INICIAL
  //Mostrar el primer diálogo para seleccionar la carpeta base (NIVEL 1)
  if SelectDirectoryDialog1.Execute then
  begin
    RutaBase := SelectDirectoryDialog1.FileName;
    //Pedir al usuario el nombre de la carpeta principal (raíz del proyecto)
    NombreCarpetaPrincipal := InputBox('Carpeta Principal', 'Introduce el nombre de la CARPETA PRINCIPAL (NIVEL 1) del proyecto:', '');
    // Si el usuario cancela o no introduce un nombre, se termina la operación
    if Trim(NombreCarpetaPrincipal) = '' then
    begin
      ShowMessage('Operación cancelada: no se introdujo un nombre para la carpeta principal.');
      Exit;
    end;

    // Si no cancela, se construye la ruta completa de la nueva carpeta principal
    RutaCompletaPrincipal := IncludeTrailingPathDelimiter(RutaBase) + Trim(NombreCarpetaPrincipal);
    //error al crear carpeta principal
    if not ForceDirectories(RutaCompletaPrincipal) then
    begin
      ShowMessage('Error al crear la carpeta principal: ' + RutaCompletaPrincipal);
      Exit;
    end;

    //Inicia el proceso recursivo para crear las subcarpetas de nivel 2
    CrearSubcarpetasRecursivamente(RutaCompletaPrincipal, 2);

    //Mostrar mensaje de finalización
    ShowMessage('¡Creación de estructura de carpetas finalizado con éxito!');
  end
  else
  begin
    ShowMessage('Selección de carpeta cancelada.');
  end;
end;

end.
