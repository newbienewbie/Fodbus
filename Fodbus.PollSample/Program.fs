
open Itminus.Fiddlewares
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection


open Itminus.Fodbus
open Itminus.Fodbus.Indication
open Itminus.Fiddlewares.Middleware
open Microsoft.Extensions.Logging
open System


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


type State () =
    member val Alarms:Alarm option = None with get, set

module Middlewares =

    let mw_报警_复位 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
        let perform: Perform<unit, string> = fun ctrl ctx ->
            task { 
                let state = ctx.ServiceProvider.GetRequiredService<State>();
                logger.LogInformation("警报已复位");
                state.Alarms <- None
                // publish events ?
                return Ok()
            } 
        handleBtnPressedMiddleware 
            config.ResetPin_提示灯
            config.ResetBtn_执行键
            perform 
            ctrl


    let mw_报警_置位 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
        let proc (ctx: MsgCtx) = task {
            let state = ctx.ServiceProvider.GetRequiredService<State>();
            let hasAlarm = match state.Alarms with Some alarm -> true | None -> false
            if hasAlarm && not (ctx.DOs.Pin(config.AndonPinRed)) then
                let pinname = Enum.GetName<DOPinAddr>(config.AndonPinRed);
                logger.LogInformation("开始输出报警到{pinname}", pinname)
            let dos = 
                ctx.DOs.Copy()
                    .SetPin(config.AndonPinRed, hasAlarm)
                    .SetPin(config.AndonPinSound, hasAlarm)
                    .SetPin(config.ResetPin_提示灯, hasAlarm) 
            let ctx' = Some dos |> MsgCtx.compareWithMsgCtx ctx
            return ctx'
        }
        toContinuation ctrl proc

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



//let MW_放行_处理  = fun ctrl ->
//    let perform: Perform<unit, string> = fun ctrl ctx ->
//        printfn "已执行"
//        task { return Ok ()} 
//    handleBtnPressedMiddleware 
//        PINS_CONFIG.HintPin_放行_提示灯 
//        PINS_CONFIG.Btn_放行_执行键  
//        perform 
//        ctrl

//let MW_放行_提示 = fun (ctrl: ZLanCtrl) ->
//    let preflight: Preflight<string> = fun ctrl ctx -> 
//        printfn "正在飞检"
//        task { return Ok () }
//    setHintOnWhen 
//        PINS_CONFIG.HintPin_放行_提示灯 
//        PINS_CONFIG.Btn_放行_执行键 
//        ctrl 
//        preflight
//    |> toContinuation ctrl 


        

let makeProcessor (loggerFactory :ILoggerFactory)  (config: ZLanPinsConfiguration) ctrl = 
    //MW_放行_提示 ctrl
    //>=> MW_放行_处理 ctrl
    //mw_set_toggle DOPinAddr.DO1 ctrl
    //>=> mw_set_toggle DOPinAddr.DO2 ctrl
    //>=> mw_set_toggle DOPinAddr.DO3 ctrl
    //>=> 
    Middlewares.mw_报警_置位 (loggerFactory.CreateLogger("【报警】【置位】")) config ctrl
    >=> Middlewares.mw_报警_复位 (loggerFactory.CreateLogger("【报警】【复位】")) config ctrl 
    //>=> mw_set_toggle DOPinAddr.DO4
    

let services = new ServiceCollection()
services.AddSingleton<State>() |> ignore;
services.AddLogging(fun lb -> 
    lb.AddConsole() 
    |> ignore
) |> ignore
let sp = services.BuildServiceProvider();

task {
    let loggerFactory = sp.GetRequiredService<ILoggerFactory>();
    let ssf = sp.GetRequiredService<IServiceScopeFactory>();

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

    do! Process.runMainLoopAsync scanOpt ssf (makeProcessor loggerFactory PINS_CONFIG )
}
|> ignore

while true do
    let input = System.Console.ReadLine()
    if input = "EXIT" then
        exit(0)
    else if input = "ALARM" then
        let state = sp.GetRequiredService<State>();
        state.Alarms <- Some Alarm.Critical
    else if input = "RESET" then
        let state = sp.GetRequiredService<State>();
        state.Alarms <- None
    else 
        ()

