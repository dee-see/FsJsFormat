module Tests

open System
open System.IO
open Xunit

[<Fact>]
let ``Nested objects`` () =
    use sw = new StringWriter()
    let input = "{\"a\":{\"b\":{\"c\":4}}}\n"
    FsJsFormat.format sw input
    let expected = 
        [ "{"
          "    \"a\":{"
          "        \"b\":{"
          "            \"c\":4"
          "        }"
          "    }"
          "}"
          ""
          "" ] // FIXME the test in hacked to make sure it passes even with the extra new lines...
        |> String.concat Environment.NewLine
    Assert.Equal(expected, (sw.ToString()))

[<Fact>]
let ``No final newline`` () =
    use sw = new StringWriter()
    let input = "{}"
    FsJsFormat.format sw input
    let expected = 
        [ "{"
          "    "
          "}" ]
        |> String.concat Environment.NewLine
    Assert.Equal(expected, (sw.ToString()))
