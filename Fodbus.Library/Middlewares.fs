namespace Itminus.Fodbus

open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection


open Itminus.Fodbus
open Itminus.Fodbus.Indication
open Microsoft.Extensions.Logging
open System



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


type WhenGoNextBtnPressed = MsgCtx -> Task<Result<unit, string>>

type State (goNextBtnPressed: WhenGoNextBtnPressed) =

    member val Alarms:Alarm option = None with get, set

    member val Allow: bool = false with get, set

    member this.WhenGoNextBtnPressed with get() = goNextBtnPressed

        

module Middlewares =

    // 这里的中间件核心总是会接受一个 ctx 参数，返回 DOsMsg option
    // 然后由 toContinuation 转成一个 ctx -> next -> result 类型

    let mw_报警灯_输出 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
        let proc (ctx: MsgCtx) = task {
            let state = ctx.ServiceProvider.GetRequiredService<State>();
            let hasAlarm = match state.Alarms with Some alarm -> true | None -> false

            let dos = ctx |> MsgCtx.getCurrentDOs
            let dos = 
                dos.Copy()
                    .SetPin(config.AndonPinRed, hasAlarm)
                    //.SetPin(config.AndonPinSound, hasAlarm)
                    .SetPin(config.ResetPin_提示灯, hasAlarm) 

            return 
                // 毕竟设备缓存里是否已经有报警输出、和当前状态中是否需要报警输出
                match ctx.DOs.Pin(config.AndonPinRed), hasAlarm with
                | true, true -> None
                | true, false -> Some dos
                | false, true -> Some dos
                | false, false -> None
        }

        let log = fun (ctx: MsgCtx)->
            let pinname = Enum.GetName<DOPinAddr>(config.AndonPinRed);
            match ctx.Pending with
            | Some p -> logger.LogInformation("输出<报警安灯>{pinname}={value}", pinname, p.Pin(config.AndonPinRed)) 
            | None -> ()

        toContinuation ctrl log proc 

    let mw_复位按钮_执行 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
        let perform: Perform<unit, string> = fun ctrl ctx ->
            task { 
                logger.LogInformation("输入<复位按钮>被按下")
                let state = ctx.ServiceProvider.GetRequiredService<State>();
                state.Alarms <- None
                return Ok()
            } 

        let log = fun (ctx: MsgCtx)->
            let pinname = Enum.GetName<DOPinAddr>(config.ResetPin_提示灯);
            match ctx.Pending with
            | Some p -> logger.LogInformation("输出<复位提示灯>{pinname}={value}", pinname, p.Pin(config.ResetPin_提示灯)) 
            | None -> ()

        let whenOk: WhenPerformOk<unit> = fun () ctx -> 
            None
        let whenNg: WhenPerformNg<string> = fun reason ctx ->
            None

        handleBtnPressedCore 
            config.ResetPin_提示灯
            config.ResetBtn_执行键
            ctrl
            perform 
            whenNg
            whenOk
        |> toContinuation ctrl log


    let mw_放行提示灯_输出 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
        let proc (ctx: MsgCtx) = task {
            let state = ctx.ServiceProvider.GetRequiredService<State>();
            let allowGoNext = state.Allow 

            let computeDOS () =
                let dos = ctx |> MsgCtx.getCurrentDOs
                dos.Copy().SetPin(config.HintPin_放行_提示灯, allowGoNext)

            return 
                // 设备缓存里是否已经有提示灯输出、和当前状态中是否需要提示灯输出
                match ctx.DOs.Pin(config.HintPin_放行_提示灯), allowGoNext with
                | true, true -> None
                | true, false -> computeDOS () |> Some
                | false, true -> computeDOS () |> Some
                | false, false -> None
        }
        let log ctx = 
            let pinname = Enum.GetName<DOPinAddr>(config.HintPin_放行_提示灯);
            match ctx.Pending with
            | Some p -> logger.LogInformation("输出<放行提示灯>{pinname}={value}", pinname, p.Pin(config.HintPin_放行_提示灯)) 
            | None -> ()
        toContinuation ctrl log proc



    let mw_放行按钮_执行 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
        let perform: Perform<unit, string>  = fun ctrl ctx ->
            task {
                let state = ctx.ServiceProvider.GetRequiredService<State>();
                return! state.WhenGoNextBtnPressed ctx 
            }
        /// 失败之后无需输出，只需要改变报警状态，下一轮次会自动报警
        let whenNg: WhenPerformNg<string> = fun reason ctx ->
            logger.LogInformation("调用放行钩子失败{err}", reason);
            let dos = ctx |> MsgCtx.getCurrentDOs
            let state = ctx.ServiceProvider.GetRequiredService<State>();
            state.Alarms <- Some Alarm.Critical
            None
        /// 成功之后应熄灭放行提示灯
        let whenOk: WhenPerformOk<unit> = fun () ctx ->
            logger.LogInformation("调用放行钩子成功");
            let state = ctx.ServiceProvider.GetRequiredService<State>();
            let dos = ctx |> MsgCtx.getCurrentDOs
            dos.Copy().SetPin(config.HintPin_放行_提示灯, false) |> Some
        let log ctx = 
            let pinname = Enum.GetName<DOPinAddr>(config.HintPin_放行_提示灯);
            logger.LogInformation("开始执行放行....复位针脚{pinname}",pinname);
        handleBtnPressedCore 
            config.HintPin_放行_提示灯
            config.Btn_放行_执行键
            ctrl
            perform
            whenNg
            whenOk
        |> toContinuation ctrl log


    let mw_set_toggle pin ctrl = 
        let log ctx = ()
        let toggle (ctx: MsgCtx) =
            let msg = ctx |> MsgCtx.getCurrentDOs
            let msg= msg.Copy()
            let curr = msg.Pin(pin)
            task {
                return msg.SetPin(pin, not curr) |> Some
            }
        toContinuation ctrl log toggle 


