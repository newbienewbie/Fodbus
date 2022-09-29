
open Itminus.Fiddlewares
open System.Threading.Tasks


open Itminus.Fodbus
open Itminus.Fodbus.Indication
open Itminus.Fiddlewares.Middleware
open FsToolkit.ErrorHandling


let handleBtnPressedMiddleware hintPath btnPin perform ctrl = 
    let h = handleBtnPressed hintPath btnPin ctrl perform 
    toContinuation ctrl h

let HintPin_放行_提示灯 = DOPinAddr.DO5
let Btn_放行_执行键 = DIPinAddr.DI2

let MW_放行_处理  = fun ctrl ->
    let perform: Perform<unit, string> = fun ctrl ->
        printfn "已执行"
        task { return Ok ()} 
    handleBtnPressedMiddleware HintPin_放行_提示灯 Btn_放行_执行键  perform ctrl

let MW_放行_提示 = fun (ctrl: ZLanCtrl) ->
    let preflight: Preflight<string> = fun ctrl -> 
        printfn "正在飞检"
        task { return Ok () }
    setHintOnWhen HintPin_放行_提示灯 Btn_放行_执行键 ctrl preflight
    |> toContinuation ctrl 

let mw_set_toggle pin ctrl = 
    let toggle (ctx: MsgCtx) =
        ctx.Evovle(fun m -> 
            let p = ctx.GetPendingDOs();
            let curr = p.Pin(pin)
            m.SetPin(pin, not curr) 
        ) 
        |> Some
        |> Task.FromResult
    toContinuation ctrl toggle


let makeProcessor ctrl = 
    MW_放行_提示 ctrl
    >=> 
    MW_放行_处理 ctrl
    >=> mw_set_toggle DOPinAddr.DO2 ctrl
    >=> mw_set_toggle DOPinAddr.DO3 ctrl
    //>=> mw_set_toggle DOPinAddr.DO4
    

task {
    let scanOpt: ZLanCtrlScanOptions = {
        IpAddr = "192.168.1.200"
        Port = 502
        ReadTimeout = 1000
        WriteTimeout = 1000
        ScanInterval = 20
        ErrorInterval = 1000
        SlaveAddr = 1uy
        ConnectionTimetout = 2000
    }
    do! Process.runMainLoopAsync scanOpt makeProcessor 
}
|> ignore


System.Console.ReadLine()
