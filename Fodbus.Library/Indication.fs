namespace Itminus.Fodbus



module Indication =

    open System.Threading.Tasks
    open FsToolkit.ErrorHandling


    /// 飞检
    type Preflight<'TError> =ZLanCtrl -> Task<Result<unit,'TError>>
    /// 执行一个决议
    type Perform<'TOk,'TError> = ZLanCtrl -> Task<Result<'TOk,'TError>>
    /// 当执行NG需要返回新的Ctx（None表示未变化）
    type WhenPerformNg<'Ng> = 'Ng -> MsgCtx -> MsgCtx option
    /// 当执行OK需要返回新的Ctx（None表示未变化）
    type WhenPerformOk<'Ok> = 'Ok -> MsgCtx -> MsgCtx option


    /// 检查提示灯是否已经点亮
    let private hasHint (hintPin: DOPinAddr) (ctx: MsgCtx) =
        let pending = ctx.GetPendingDOs();
        pending.Pin(hintPin)

    /// 检查执行键是否已经被按下
    let private hasBtnPressed (btnPin: DIPinAddr) (ctx: MsgCtx) =  
        ctx.DIs.Pin(btnPin)



    /// 把一个处理函数转成接受连续子的中间件函数
    let toContinuation (ctrl: ZLanCtrl) (f: MsgCtx -> Task<MsgCtx option>)=
        fun (ctx: MsgCtx) (next) -> task {
            match! f ctx with
            | Some ctx' -> 
                // 更新当前ZLAN里的缓存缓存
                match ctx'.Pending with
                | Some msg -> ctrl.UpdateDOsCache(msg)
                | None -> ()
                // 调用后续中间件
                return! next ctx'
            | None -> return! next ctx
        }


    type SetHintOnError<'TPreflightNg> =
        | HintAlreadyOn
        | BtnAlreadyOn
        | PreflightError of 'TPreflightNg

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
            let! res = preflight ctrl |> TaskResult.mapError PreflightError
            return res
        }
        task{
            match! check with
            | Error e -> return None
            | Ok () -> 
                let ctx' = 
                    fun (msg: DOsMsg) -> msg.SetPin(hintPin, true)
                    |> ctx.Evovle
                    |> Some 
                return ctx'
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
                let! res = perform ctrl |> TaskResult.mapError PerformError
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
            ctx.Evovle(fun msg -> msg.Copy().SetPin(hintPin, false))
            |> Some
        handleBtnPressedCore hintPin btnPin ctrl perform whenError whenOk ctx


    /// 把计算出的输出刷入设备
    let flushDOs (ctrl: ZLanCtrl) (dosMsgOpt: DOsMsg option) =
        task{
            match dosMsgOpt with
            | Some msg ->
                let pins: bool[] = msg.CopyValues()
                let! written = ctrl.WriteDOsAsync(DOPinAddr.DO1,pins) 
                match written with
                | Error e -> failwith $"刷写失败: {e}"
                | Ok _ -> ()
            | None -> ()
        }
