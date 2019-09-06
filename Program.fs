type State =
    | InSingleLineComment of char list
    | InMultiLineComment of char list
    | InString of char * char list
    | InCode of char list

let format (input: string) =
    let newLineChars = [ ';'; ',' ]
    let indentChars = [ '{' ]
    let unindentChars = [ '}' ]
    let stringDelimChars = [ '\''; '`'; '"' ]
    let pad indentLevel = System.String(' ', (indentLevel * 4))

    let rec formatChars (sb: System.Text.StringBuilder) indentLevel charList =
        match charList with
        | InCode x ->
            match x with
            | '/' :: '/' :: rst -> formatChars (sb.Append("//")) indentLevel (InSingleLineComment rst)
            | '/' :: '*' :: rst -> formatChars (sb.Append("/*")) indentLevel (InMultiLineComment rst)
            | c :: rst when List.contains c newLineChars ->
                formatChars ((sb.Append(c)).AppendLine((pad indentLevel))) indentLevel (InCode rst)
            | c :: rst when List.contains c indentChars ->
                formatChars (((sb.Append(c)).AppendLine()).Append((pad (indentLevel + 1)))) (indentLevel + 1)
                    (InCode rst)
            | c :: rst when List.contains c unindentChars ->
                formatChars (((sb.AppendLine()).Append((pad (indentLevel - 1))).Append(c))) (indentLevel - 1)
                    (InCode rst)
            | c :: rst when List.contains c stringDelimChars ->
                formatChars (sb.Append(c)) indentLevel (InString(c, rst))
            | c :: rst -> formatChars (sb.Append(c)) indentLevel (InCode rst)
            | [] -> sb.ToString()
        | InSingleLineComment x ->
            match x with
            | '\n' :: rst -> formatChars (sb.AppendLine()) indentLevel (InCode rst)
            | c :: rst -> formatChars (sb.Append(c)) indentLevel (InSingleLineComment rst)
            | [] -> sb.ToString()
        | InMultiLineComment x ->
            match x with
            | '*' :: '/' :: rst -> formatChars (sb.Append("*/")) indentLevel (InCode rst)
            | c :: rst -> formatChars (sb.Append(c)) indentLevel (InMultiLineComment rst)
            | [] -> sb.ToString()
        | InString(delim, x) ->
            match x with
            | '\\' :: c :: rst when c = delim ->
                formatChars (sb.Append("\\").Append(delim)) indentLevel (InString(delim, rst))
            | c :: rst when c = delim -> formatChars (sb.Append(delim)) indentLevel (InCode rst)
            | c :: rst -> formatChars (sb.Append(c)) indentLevel (InString(delim, rst))
            | [] -> sb.ToString()
    List.ofSeq input
    |> InCode
    |> formatChars (System.Text.StringBuilder()) 0

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        System.IO.File.ReadAllText(file)
        |> format
        |> printfn "%s"
    | _ -> eprintfn "Usage: FsJsFormat file.js"
    0
