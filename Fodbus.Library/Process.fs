namespace Itminus.Fodbus

open System.Threading.Tasks
open FsToolkit.ErrorHandling


[<CLIMutable>]
type ZLanCtrlScanOptions = {
    IpAddr: string
    Port: int
    ReadTimeout: int
    WriteTimeout: int
    SlaveAddr: byte
    ConnectionTimetout: int
    ScanInterval: int
    ErrorInterval: int
}

module Process =

    /// 上下文处理的最后一个延续子，返回本轮要刷入设备的数据
    type Final = MsgCtx -> Task<DOsMsg option>

    /// 消息上下文处理器: 处理上下文，返回本轮要刷入设备的数据
    type MsgCtxProcessor = MsgCtx -> Final -> Task<DOsMsg option>

    /// 构建消息上下文处理器
    type MakeProcessor = ZLanCtrl -> MsgCtxProcessor 

    /// 会把要刷入设备的数据刷入数据
    type FlushZlanCtrl = ZLanCtrl -> (Result<DOsMsg option,string>) -> Task<unit>

    let private final: Final = fun (ctx: MsgCtx) -> task { return ctx.Pending}

    /// 把计算结果刷入设备
    let private flushDOs : FlushZlanCtrl = fun (ctrl: ZLanCtrl) (res: Result<DOsMsg option,string>) ->
        task{
            match res with
            | Error e -> failwith e
            | Ok None -> ()
            | Ok (Some msg) ->
                let pins: bool[] = msg.CopyValues()
                let! written = ctrl.WriteDOsAsync(DOPinAddr.DO1,pins) 
                match written with
                | Error e -> failwith $"向ZLAN(ip={ctrl.IpAddr})刷写失败: {e}"
                | Ok _ -> ()
        }

    /// 执行函数：负责建立连接、串行执行相关逻辑、并将计算结果刷入设备。此函数不会抛出异常。在处理过程中抛出的异常会导致连接断开、结束循环。
    let executeAsync 
        (opt: ZLanCtrlScanOptions) 
        (processor: MsgCtxProcessor) 
        (flushDOsMsgToZLan: FlushZlanCtrl)  
        (ctrl: ZLanCtrl)  =
        task {
            try 
                // 建立连接
                do! ctrl.EnsureConnectedAsync(opt.ConnectionTimetout);
                // 处理连接
                while true do
                    // 处理上下文消息
                    let! dosResult = taskResult{
                        // make msg ctx
                        let! dis = ctrl.ScanDIAsync() 
                        let! dos = ctrl.GetDOsFromCache();
                        let ctx = MsgCtx.createNew dis dos
                        let! msg = processor ctx final
                        return msg
                    }
                    // 写入设备(此处可能会抛出异常，而后被捕获)
                    do! flushDOsMsgToZLan ctrl dosResult
                    do! Task.Delay(opt.ScanInterval)
            with
                // 遇到问题，尝试断开当前连接
                | ex -> 
                    printfn "%s" ex.Message
                    do! Task.Delay(opt.ErrorInterval)
                    try 
                        do! ctrl.DisconectAsync(1000)
                    with
                        | ex -> ()
        }


    /// 运行主循环
    let runMainLoopAsync (scanOpt: ZLanCtrlScanOptions) (makeProcessor: MakeProcessor) = 
        let ctrl = ZLanCtrl(scanOpt.IpAddr, scanOpt.Port, scanOpt.ReadTimeout, scanOpt.WriteTimeout, scanOpt.SlaveAddr);
        let processor = makeProcessor ctrl
        task {
            while true do 
                do! executeAsync scanOpt processor flushDOs ctrl
        }
