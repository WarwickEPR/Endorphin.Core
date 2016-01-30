// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

open System.IO

let applyHeader path comment formatComment =
    Seq.collect (fun ext -> Directory.EnumerateFiles(path, sprintf "*.%s" ext, SearchOption.AllDirectories))
    >> Seq.map (fun filename -> 
        use file = new StreamReader (filename)
        let content = file.ReadToEnd()
        let header = formatComment comment
        if content.StartsWith header
        then (filename, content)
        else (filename, header + content))
    >> Seq.iter (fun (filename, content) ->
        use file = new StreamWriter (filename)
        file.Write content)

let path = failwith "Enter path to the solution root here before running the script."
let comment = "Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information."

[ "fs" ; "fsx" ; "cs" ] |> applyHeader path comment (sprintf "// %s\r\n\r\n")
[ "xaml" ]              |> applyHeader path comment (sprintf "<!-- %s -->\r\n\r\n")