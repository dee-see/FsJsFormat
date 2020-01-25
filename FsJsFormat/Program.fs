module FsJsFormat

open System

type State =
    | AtBeginningOfLine of char list
    | InCode of char list
    | InSingleLineComment of char list
    | InMultiLineComment of char list
    | InString of char * char list
    | InRegex of char list
    | InRegexFlags of char list
    | InRegexCharacterClass of char list

type FormatterChars =
    | Empty
    | Letter of char
    | Text of string

let format (textWriter : IO.TextWriter) (input: string) =
    let newLineChars = [ ';'; ',' ]
    let indentChars = [ '{' ]
    let unindentChars = [ '}' ]
    let stringDelimChars = [ '\''; '`'; '"' ]
    let newLine = Environment.NewLine
    let pad indentLevel = String(' ', (max (indentLevel * 4) 0)) // Shouldn't need the max, but errors happen...

    let rec formatChars formattedChars indentLevel charList =
        match formattedChars with
        | Letter l -> textWriter.Write(l)
        | Text t -> textWriter.Write(t)
        | Empty -> ()

        match charList with
        | AtBeginningOfLine x ->
            match x with
            | c :: rst when Char.IsWhiteSpace(c) -> formatChars Empty indentLevel (AtBeginningOfLine rst)
            | _ -> formatChars Empty indentLevel (InCode x)
        | InCode x ->
            match x with
            | '/' :: '/' :: rst -> formatChars (Text "//") indentLevel (InSingleLineComment rst)
            | '/' :: '*' :: rst -> formatChars (Text "/*") indentLevel (InMultiLineComment rst)
            | '/' :: rst -> formatChars (Letter '/') indentLevel (InRegex rst)
            | c :: rst when List.contains c newLineChars -> formatChars (Text (sprintf "%O%s%s" c newLine (pad indentLevel))) indentLevel (AtBeginningOfLine rst)
            | c :: rst when List.contains c indentChars -> 
                let newIndent = indentLevel + 1
                formatChars (Text ((sprintf "%O%s%s" c newLine (pad newIndent)))) newIndent (InCode rst)
            | c1 :: rst1 when List.contains c1 unindentChars -> 
                let newIndent = indentLevel - 1
                match rst1 with
                | c2 :: rst2 when List.contains c2 unindentChars -> formatChars (Text (sprintf "%s%s%O" newLine (pad newIndent) c1)) newIndent (InCode rst1)
                | c2 :: rst2 when List.contains c2 newLineChars -> formatChars (Text (sprintf "%s%s%O%O%s%s" newLine (pad newIndent) c1 c2 newLine (pad newIndent))) newIndent (InCode rst2)
                | c2 :: rst2 -> formatChars (Text ((sprintf "%s%s%O%s%s%O" newLine (pad newIndent) c1 newLine (pad newIndent) c2))) newIndent (InCode rst2)
                | [] -> ()
            | c :: rst when List.contains c stringDelimChars -> formatChars (Letter c) indentLevel (InString(c, rst))
            | c :: rst -> formatChars (Letter c) indentLevel (InCode rst)
            | [] -> ()
        | InSingleLineComment x ->
            match x with
            | '\n' :: rst -> formatChars (Text newLine) indentLevel (InCode rst)
            | c :: rst -> formatChars (Letter c) indentLevel (InSingleLineComment rst)
            | [] ->()
        | InMultiLineComment x ->
            match x with
            | '*' :: '/' :: rst -> formatChars (Text "*/") indentLevel (InCode rst)
            | c :: rst -> formatChars (Letter c) indentLevel (InMultiLineComment rst)
            | [] -> ()
        | InString (delim, x) ->
            match x with
            | '\\' :: c :: rst when c = delim -> formatChars (Text (sprintf "\\%O" delim)) indentLevel (InString(delim, rst))
            | c :: rst when c = delim -> formatChars (Letter delim) indentLevel (InCode rst)
            | c :: rst -> formatChars (Letter c) indentLevel (InString(delim, rst))
            | [] -> ()
        | InRegex x ->
            match x with
            | '\\' :: c :: rst -> formatChars (Text (sprintf "\\%O" c)) indentLevel (InRegex rst)
            | '/' :: c :: rst when Char.IsLetter(c) -> formatChars (Text (sprintf "/%O" c)) indentLevel (InRegexFlags rst)
            | '/' :: rst -> formatChars (Letter '/') indentLevel (InCode rst)
            | '[' :: rst -> formatChars (Letter '[') indentLevel (InRegexCharacterClass rst)
            | c :: rst -> formatChars (Letter c) indentLevel (InRegex rst)
            | [] -> ()
        | InRegexFlags x ->
            match x with
            | c :: rst when Char.IsLetter(c) -> formatChars (Letter c) indentLevel (InRegexFlags rst)
            | _ -> formatChars Empty indentLevel (InCode x)
        | InRegexCharacterClass x ->
            match x with
            | '\\' :: c :: rst -> formatChars (Text (sprintf "\\%O" c)) indentLevel (InRegexCharacterClass rst)
            | ']' :: rst -> formatChars (Letter ']') indentLevel (InRegex rst)
            | c :: rst -> formatChars (Letter c) indentLevel (InRegexCharacterClass rst)
            | [] -> ()

    List.ofSeq input
    |> AtBeginningOfLine
    |> formatChars Empty 0

[<EntryPoint>]
let main argv =
    let usage () = eprintfn "Usage: FsJsFormat file.js [--save] or feed input through stdin"
    match argv with
    | [| |] ->
        match stdin.ReadToEnd() with
        | "" -> usage ()
        | code -> format Console.Out code
    | [| file |] -> IO.File.ReadAllText(file) |> format Console.Out
    | [| file; "--save" |] -> 
        let text = IO.File.ReadAllText(file)
        use writer = IO.File.CreateText(file)
        format writer text
    | _ -> usage()
    0
