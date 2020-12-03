module Logging

type Level =
    | Trace
    | Debug
    | Info
    | Warning
    | Err
    | Fatal

type Entry = {
    Level: Level
    Message: string
}

let log f level msg = f {Level=level; Message = msg}

let logf f level msg =
    let continuation str = f {Level=level; Message=str}
    Printf.ksprintf continuation msg

let logTrace f msg = log f Trace msg
let logfTrace f msg = logf f Trace msg

let logDebug f msg = log f Debug msg
let logfDebug f msg = logf f Debug msg

let logInfo f msg = log f Info msg
let logfInfo f msg = logf f Info msg

let logWarning f msg = log f Warning msg
let logfWarning f msg = logf f Warning msg

let logError f msg = log f Err msg
let logfError f msg = logf f Err msg

let logFatal f msg = log f Fatal msg
let logfFatal f msg = logf f Fatal msg

let annotate f entry = f entry

let prepend text f =
    annotate (fun x -> { x with Message = sprintf "%s %s" text x.Message })
    >> f

let append text f =
    annotate (fun x -> { x with Message = sprintf "%s %s" x.Message text })
    >> f

