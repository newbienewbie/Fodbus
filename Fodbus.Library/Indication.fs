namespace Itminus.Fodbus



module Indication =

    open System.Threading.Tasks
    open FsToolkit.ErrorHandling

    type XError = 
        | E_PreflightError of string
        | E_PerformError of string

    type Preflight<'TError> = ZLanCtrl -> Task<Result<unit,'TError>>
    type Perform<'TError> = ZLanCtrl -> Task<Result<unit,'TError>>

    type FlushDOs = ZLanCtrl -> Task<Result<DOsMsg, string>>

    let hasHint (hintPin: DOPinAddr) (ctx: MsgCtx) =
        let pending = ctx.GetPendingDOs();
        pending.Pin(hintPin)

    let hasBtnPressed (btnPin: DIPinAddr) (ctx: MsgCtx) =  
        ctx.DIs.Pin(btnPin)


    /// 设置提示灯
    let setHintOn 
        (hintPin: DOPinAddr) 
        (ctrl: ZLanCtrl) 
        (preflight)
        = fun (ctx: MsgCtx) next -> taskResult {
            do! preflight ctrl |> TaskResult.mapError (fun _ -> ())
            return 
                fun (msg: DOsMsg) -> msg.SetPin(hintPin, true)
                |> ctx.Evovle
                |> next
        }

    /// 在提示灯亮(hint)的情况下，如果用户按下了按钮(btn)，就尝试执行相关动作，最后关闭提示灯
    let handleBtnPressed 
        (procName: string)
        (hintPin: DOPinAddr) 
        (btnPin: DIPinAddr) 
        (ctrl: ZLanCtrl)  
        (preflight: Preflight<'TPreflightError>) 
        (perform: Perform<'TPerformError>) 
        =  fun ctx next ->
        taskResult{
            let hint = hasHint hintPin ctx
            let btn = hasBtnPressed btnPin ctx
            match hint , btn with
            | true, true ->
                do! preflight ctrl |> TaskResult.mapError (fun e -> sprintf "%s：%A" procName e |> E_PerformError)
                do! perform ctrl |> TaskResult.mapError (fun e -> sprintf "%s：%A" procName e |> E_PerformError)
                return
                    fun (msg: DOsMsg) -> msg.SetPin(hintPin, false) 
                    |> ctx.Evovle
                    |> next
            | _ -> 
                return next ctx
        }


    type Commit = MsgCtx -> Task<Result<unit,string>>


    let commitAndFlush 
        (ctrl: ZLanCtrl) 
        (commit: Commit)
        = fun (ctx: MsgCtx) next -> taskResult {
            do! commit ctx
            match ctx.Pending with
            | Some msg ->
                let pins: bool[] = msg.CopyValues()
                do! ctrl.WriteDOsAsync(DOPinAddr.DO1,pins) 
                return next ctx
            | None -> return next ctx
        }
