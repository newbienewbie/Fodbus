
open Itminus.Fiddlewares
open System.Threading.Tasks


open Itminus.Fodbus
open Itminus.Fodbus.Indication
open Itminus.Fiddlewares.Middleware


let handleBtnPressedMiddleware hintPath btnPin perform ctrl = 
    let h = handleBtnPressed hintPath btnPin ctrl perform 
    toContinuation ctrl h


[<CLIMutableAttribute>]
type ZLanPinsConfiguration = {

    AndonPinRed    : DOPinAddr
    AndonPinYellow : DOPinAddr
    AndonPinRight  : DOPinAddr
    AndonPinSound  : DOPinAddr


    ResetPin_提示灯 : DOPinAddr
    ResetBtn_执行键 : DIPinAddr
    

    HintPin_放行_提示灯 : DOPinAddr
    Btn_放行_执行键 : DIPinAddr
}


type Alarm = 
    | Critical
    | Warning

type Model = {
    Alarms: Alarm option
}

let PINS_CONFIG: ZLanPinsConfiguration = {
    AndonPinRed    = DOPinAddr.DO1
    AndonPinYellow = DOPinAddr.DO2
    AndonPinRight  = DOPinAddr.DO3
    AndonPinSound  = DOPinAddr.DO4

    ResetPin_提示灯 = DOPinAddr.DO5
    ResetBtn_执行键 = DIPinAddr.DI2

    HintPin_放行_提示灯 = DOPinAddr.DO6
    Btn_放行_执行键 = DIPinAddr.DI6
}


let MW_放行_处理  = fun ctrl ->
    let perform: Perform<unit, string> = fun ctrl ->
        printfn "已执行"
        task { return Ok ()} 
    handleBtnPressedMiddleware 
        PINS_CONFIG.HintPin_放行_提示灯 
        PINS_CONFIG.Btn_放行_执行键  
        perform 
        ctrl

let MW_放行_提示 = fun (ctrl: ZLanCtrl) ->
    let preflight: Preflight<string> = fun ctrl -> 
        printfn "正在飞检"
        task { return Ok () }
    setHintOnWhen 
        PINS_CONFIG.HintPin_放行_提示灯 
        PINS_CONFIG.Btn_放行_执行键 
        ctrl 
        preflight
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
