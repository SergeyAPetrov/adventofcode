module types

//record
type Build = 
    {
        Id : int 
        Url : string
    }

let build = {Id = 1; Url = "1"}
let build2 = {build with Id = 2}

//option types

type BuildStatus =
    {
        Status : string
        FailedBuilds : string list option
    } 

let buildStatus1 = {Status = "OK"; FailedBuilds = None}
let buildStatus2 = {buildStatus1 with FailedBuilds = Some ["a";"b"]}

let PrintBuild build = 
    match build.FailedBuilds with 
    | Some builds -> List.iter (printfn "%s")
    | None -> (fun _ -> ())

