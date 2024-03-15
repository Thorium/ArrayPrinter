module ArrayPrinter

open System
open System.Text

[<AutoOpen>]
module Core =
  [<Struct>]
  type NumberType =
    | Hex
    | HexLower
    | Octal
    | Decimal
    | Binary

type PrinterOptions =
  {
    WithHeader : bool
    WithData : bool
    NumberFormat : NumberType
    RowWidth : byte
    DataPlaceholderChar : char
    MaxLength : int64
  }
  
  static member defaults = 
    {
      WithHeader = true
      WithData = true
      NumberFormat = Hex
      RowWidth = 16uy
      DataPlaceholderChar = '.'
      MaxLength = 0L
    }
    
  static member binaryDefaults =
    {
      WithHeader = true
      WithData = true
      NumberFormat = Binary
      RowWidth = 4uy
      DataPlaceholderChar = '.'
      MaxLength = 0L
    }

  static member decimalDefaults =
    {
      WithHeader = true
      WithData = true
      NumberFormat = Decimal
      RowWidth = 10uy
      DataPlaceholderChar = '.'
      MaxLength = 0L
    }

  static member ocalDefaults =
    {
      WithHeader = true
      WithData = true
      NumberFormat = Octal
      RowWidth = 8uy
      DataPlaceholderChar = '.'
      MaxLength = 0L
    }

let private formatHeaderNumber format (value : int64) =
  match format with
  | Hex -> value.ToString("X4")
  | HexLower -> value.ToString("x4")
  | Octal -> 
    let c = Convert.ToString(value, 8)
    '0' |> List.replicate (4 - c.Length)
    |> Seq.toArray
    |> String |> (fun d -> d + c)
  | Decimal -> value.ToString("0000")
  | Binary -> 
    let c = Convert.ToString(value, 2)
    '0' |> List.replicate (8 - c.Length)
    |> Seq.toArray
    |> String |> (fun d -> d + c)

let private formatNumber format (value : byte) =
  match format with
  | Hex -> value.ToString("X2")
  | HexLower -> value.ToString("x2")
  | Octal -> 
    let c = Convert.ToString(value, 8)
    if c.Length = 1 then "00" + c
    elif c.Length = 2 then "0" + c
    else c
  | Decimal -> value.ToString("000")
  | Binary -> 
    let c = Convert.ToString(value, 2)
    '0' |> List.replicate (8 - c.Length)
    |> Seq.toArray
    |> String |> (fun d -> d + c)
    
let private emptyCell = 
  function 
  | Hex 
  | HexLower -> "  "
  | Octal
  | Decimal -> "   "
  | Binary -> "        "

let printArray (options : PrinterOptions) (bytes : byte array) =
  let builder = new StringBuilder ()

  let maxLength = 
    if options.MaxLength = 0L || bytes.LongLength < options.MaxLength then bytes.LongLength
    else options.MaxLength

  for row in [0L..maxLength / (int64 options.RowWidth)] do
    if options.WithHeader 
    then 
      (row * int64 options.RowWidth) |> formatHeaderNumber options.NumberFormat
      |> builder.Append |> ignore
      // After Row Header Padding
      builder.Append " " |> ignore
      
    for col in [0L..(int64 options.RowWidth - 1L)] do
      // Before Cell Padding
      builder.Append " " |> ignore
      let i = row * (int64 options.RowWidth) + col
      if i >= maxLength then emptyCell options.NumberFormat
      else bytes.GetValue i :?> byte |> formatNumber options.NumberFormat
      |> builder.Append
      |> ignore


    if options.WithData then
      // Before Data Padding
      builder.Append "  " |> ignore
      for col in [0L..(int64 options.RowWidth - 1L)] do
        let i = row * (int64 options.RowWidth) + col
        (
          if i >= maxLength then ' '
          else 
            let c = bytes.GetValue i :?> byte |> int |> char
            if Char.IsLetterOrDigit c || Char.IsPunctuation c then c else options.DataPlaceholderChar
        )
        |> builder.Append
        |> ignore

    builder.AppendLine () |> ignore

  builder.ToString ()
