
open Itminus.Fiddlewares
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection


open Itminus.Fodbus
open Itminus.Fodbus.Indication
open Itminus.Fiddlewares.Middleware
open Microsoft.Extensions.Logging
open System




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

