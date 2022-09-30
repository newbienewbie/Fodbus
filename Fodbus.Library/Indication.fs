namespace Itminus.Fodbus

open System
open Microsoft.Extensions.Logging



module Indication =

    open System.Threading.Tasks
    open FsToolkit.ErrorHandling


    /// 飞检
    type Preflight<'TError> = ZLanCtrl -> MsgCtx -> Task<Result<unit,'TError>>
    /// 执行一个决议
    type Perform<'TOk,'TError> = ZLanCtrl -> MsgCtx -> Task<Result<'TOk,'TError>>
    /// 当执行NG需要返回新的Ctx（None表示未变化）
    type WhenPerformNg<'Ng> = 'Ng -> MsgCtx -> DOsMsg option
    /// 当执行OK需要返回新的Ctx（None表示未变化）
    type WhenPerformOk<'Ok> = 'Ok -> MsgCtx -> DOsMsg option


    /// 检查提示灯是否已经点亮
    let private hasHint (hintPin: DOPinAddr) (ctx: MsgCtx) =
        let pending = MsgCtx.getCurrentDOs ctx
        pending.Pin(hintPin)

    /// 检查执行键是否已经被按下
    let private hasBtnPressed (btnPin: DIPinAddr) (ctx: MsgCtx) =  
        ctx.DIs.Pin(btnPin)



    /// 把一个处理函数转成接受连续子的中间件函数。
    let toContinuation (ctrl: ZLanCtrl) (log: MsgCtx -> unit) (f: MsgCtx -> Task<DOsMsg option>) =
        fun (ctx: MsgCtx) (next) -> task {
            let! msgopt = f ctx
            match msgopt with
            | Some msg -> 
                // 更新当前ZLAN里的缓存缓存
                ctrl.UpdateDOsCache(msg)
                // 构造新的上下文
                let ctx' = Some msg |> MsgCtx.withPendingDOs ctx
                do log ctx'
                // 调用后续中间件
                return! next ctx'
            | None -> return! next ctx
        }


    type SetHintOnError<'TPreflightNg> =
        | HintAlreadyOn
        | BtnAlreadyOn
        | PreflightError of 'TPreflightNg

    let mapPinOnOff 
        (pin: DOPinAddr)
        (mapper: MsgCtx -> Task<bool>)
        (ctrl: ZLanCtrl)
        = fun (ctx: MsgCtx) -> task{
            let! x = mapper ctx 
            let msg = MsgCtx.getCurrentDOs ctx
            // 要是当前针脚和映射值不一样，则更新
            if msg.Pin(pin) <> x then
                let msg = msg.Copy().SetPin(pin, x)
                return Some msg
            else 
                return None
        }

    /// 点亮提示灯
    let setPinOnWhen 
        (pin: DOPinAddr) 
        (preflight: Preflight<'Ng>)
        (ctrl: ZLanCtrl) 
        = fun (ctx: MsgCtx) -> task {
            match! preflight ctrl ctx with
            | Error e -> return None
            | Ok () -> 
                let msg = MsgCtx.getCurrentDOs ctx
                let msg = msg.Copy().SetPin(pin, true)
                return Some msg 
        }


    /// 点亮提示灯
    let setHintOnWhen 
        (hintPin: DOPinAddr) 
        (btnPin: DIPinAddr)
        (ctrl: ZLanCtrl) 
        (preflight: Preflight<'Ng>)
        = fun (ctx: MsgCtx) -> 

        let check = taskResult{
            do! hasHint hintPin ctx |> Result.requireFalse HintAlreadyOn
            do! hasBtnPressed btnPin ctx |> Result.requireFalse BtnAlreadyOn
            let! res = preflight ctrl ctx |> TaskResult.mapError PreflightError
            return res
        }
        task{
            match! check with
            | Error e -> return None
            | Ok () -> 
                let msg = MsgCtx.getCurrentDOs ctx
                let msg = msg.Copy().SetPin(hintPin, true)
                return Some msg
        }


    type HandleBtnPressedError<'TPerformError> =
        | HintNotOn
        | BtnNotPressed
        | PerformError of 'TPerformError


    /// 在提示灯亮(hint)的情况下，如果用户按下了按钮(btn)，就尝试执行相关动作，最后关闭提示灯
    let handleBtnPressedCore 
        (hintPin: DOPinAddr) 
        (btnPin: DIPinAddr) 
        (ctrl: ZLanCtrl)  
        (perform: Perform<'TPerformOk,'TPerformNg>)              // 执行动作，比如修改数据库
        (whenError: WhenPerformNg<'TPerformNg>)                  // 当错误发生
        (whenOk : WhenPerformNg<'TPerformOk>)                    // 当OK
        =  fun (ctx: MsgCtx) -> 
            let x = taskResult{
                do! hasHint hintPin ctx |> Result.requireTrue HintNotOn
                do! hasBtnPressed btnPin ctx |> Result.requireTrue BtnNotPressed
                let! res = perform ctrl ctx |> TaskResult.mapError PerformError
                return res
            }
            task {
                match! x with
                | Error ng -> 
                    return
                        match ng with
                        | HintNotOn -> None
                        | BtnNotPressed -> None
                        | PerformError reason -> whenError reason ctx
                | Ok ok -> return whenOk ok ctx
            }

    let handleBtnPressed hintPin btnPin ctrl perform ctx =
        let whenError (reason) ctx = 
            printfn "%s" reason
            None
        let whenOk ok (ctx:MsgCtx)=
            let msg = ctx |> MsgCtx.getCurrentDOs
            let msg = msg.Copy().SetPin(hintPin, false)
            Some msg

        handleBtnPressedCore hintPin btnPin ctrl perform whenError whenOk ctx



