﻿namespace Itminus.Fodbus

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


type State () =
    member val Alarms:Alarm option = None with get, set

    member val Allow: bool = false with get, set
        

module Middlewares =

    // 这里的中间件核心总是会接受一个 ctx 参数，返回 DOsMsg option
    // 然后由 toContinuation 转成一个 ctx -> next -> result 类型


    let mw_报警_置位 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
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

    let mw_报警_复位 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
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
        handleBtnPressed 
            config.ResetPin_提示灯
            config.ResetBtn_执行键
            ctrl
            perform 
        |> toContinuation ctrl log


    let mw_放行_提示灯亮 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =

        let preflight:Preflight<unit> = fun ctrl (ctx: MsgCtx) -> task {
            let state = ctx.ServiceProvider.GetRequiredService<State>()
            return 
                if state.Allow then 
                    let pinname = Enum.GetName<DOPinAddr>(config.HintPin_放行_提示灯);
                    logger.LogInformation("【放行】【置位】针脚{pinname}",pinname);
                    Ok() 
                else Error ()
        }
        setPinOnWhen config.HintPin_放行_提示灯 preflight ctrl

    let mw_放行_按钮执行 (logger: ILogger) (config: ZLanPinsConfiguration) ctrl =
        let perform: Perform<unit, string>  = fun ctrl ctx ->
            task {
                let state = ctx.ServiceProvider.GetRequiredService<State>();
                state.Allow <- false
                return Ok()
            }
        let log ctx = 
            let pinname = Enum.GetName<DOPinAddr>(config.HintPin_放行_提示灯);
            logger.LogInformation("开始执行放行....复位针脚{pinname}",pinname);
        handleBtnPressed 
            config.HintPin_放行_提示灯
            config.Btn_放行_执行键
            ctrl
            perform
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

