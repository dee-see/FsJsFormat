open System

type State =
    | InSingleLineComment of char list
    | InMultiLineComment of char list
    | InString of char * char list
    | InCode of char list
    | AtBeginningOfLine of char list

type IWriter =
    inherit IDisposable
    abstract member Append: string -> IWriter
    abstract member Append: char -> IWriter
    abstract member AppendLine: string -> IWriter
    abstract member AppendLine: unit -> IWriter

[<AbstractClass>]
type AbstractWriter() = 
    abstract member Stream : IO.TextWriter

    interface IWriter with
        member this.Append (str : string) =
            this.Stream.Write(str)
            this :> IWriter

        member this.Append (c : char) =
            this.Stream.Write(c)
            this :> IWriter

        member this.AppendLine str =
            this.Stream.WriteLine(str)
            this :> IWriter

        member this.AppendLine() =
            (this :> IWriter).Append('\n')
    
    interface IDisposable with
        member this.Dispose() =
            this.Stream.Dispose()

type ConsoleWriter() = 
    inherit AbstractWriter()
    override this.Stream = Console.Out

type FileWriter(fileName) = 
    inherit AbstractWriter()
    let writer = IO.File.CreateText(fileName) :> IO.TextWriter
    override this.Stream = writer

let format (createWriter : unit -> IWriter) (input: string) =
    let newLineChars = [ ';'; ',' ]
    let indentChars = [ '{' ]
    let unindentChars = [ '}' ]
    let stringDelimChars = [ '\''; '`'; '"' ]
    let pad indentLevel = String(' ', (max (indentLevel * 4) 0)) // Shouldn't need the max, but errors happen...
    let newLine = Environment.NewLine

    let rec formatChars (w: IWriter) indentLevel charList =
        match charList with
        | AtBeginningOfLine x ->
            match x with
            | c :: rst when Char.IsWhiteSpace(c) -> formatChars w indentLevel (AtBeginningOfLine rst)
            | _ -> formatChars w indentLevel (InCode x)
        | InCode x ->
            match x with
            | '/' :: '/' :: rst -> formatChars (w.Append("//")) indentLevel (InSingleLineComment rst)
            | '/' :: '*' :: rst -> formatChars (w.Append("/*")) indentLevel (InMultiLineComment rst)
            | c :: rst when List.contains c newLineChars -> formatChars (w.Append((sprintf "%O%s%s" c newLine (pad indentLevel)))) indentLevel (AtBeginningOfLine rst)
            | c :: rst when List.contains c indentChars -> 
                let newIndent = indentLevel + 1
                formatChars (w.Append((sprintf "%O%s%s" c newLine (pad newIndent)))) newIndent (InCode rst)
            | c1 :: rst1 when List.contains c1 unindentChars -> 
                let newIndent = indentLevel - 1
                match rst1 with
                | c2 :: rst2 when List.contains c2 newLineChars -> formatChars (w.Append((sprintf "%s%s%O%O%s%s" newLine (pad newIndent) c1 c2 newLine (pad newIndent)))) newIndent (InCode rst2)
                | c2 :: rst2 -> formatChars (w.Append((sprintf "%s%s%O%s%s%O" newLine (pad newIndent) c1 newLine (pad newIndent) c2))) newIndent (InCode rst2)
                | [] -> ()
            | c :: rst when List.contains c stringDelimChars -> formatChars (w.Append(c)) indentLevel (InString(c, rst))
            | c :: rst -> formatChars (w.Append(c)) indentLevel (InCode rst)
            | [] -> ()
        | InSingleLineComment x ->
            match x with
            | '\n' :: rst -> formatChars (w.AppendLine()) indentLevel (InCode rst)
            | c :: rst -> formatChars (w.Append(c)) indentLevel (InSingleLineComment rst)
            | [] ->()
        | InMultiLineComment x ->
            match x with
            | '*' :: '/' :: rst -> formatChars (w.Append("*/")) indentLevel (InCode rst)
            | c :: rst -> formatChars (w.Append(c)) indentLevel (InMultiLineComment rst)
            | [] -> ()
        | InString (delim, x) ->
            match x with
            | '\\' :: c :: rst when c = delim -> formatChars (w.Append("\\").Append(delim)) indentLevel (InString(delim, rst))
            | c :: rst when c = delim -> formatChars (w.Append(delim)) indentLevel (InCode rst)
            | c :: rst -> formatChars (w.Append(c)) indentLevel (InString(delim, rst))
            | [] -> ()

    use writer = createWriter()
    List.ofSeq input
    |> AtBeginningOfLine
    |> formatChars writer 0

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] -> IO.File.ReadAllText(file) |> format (fun () -> new ConsoleWriter() :> IWriter)
    | [| file; "--save" |] -> IO.File.ReadAllText(file) |> format (fun () -> new FileWriter(file) :> IWriter)
    | _ -> eprintfn "Usage: FsJsFormat file.js [--save]"
    0