
open Itminus.Fiddlewares
open System.Threading.Tasks


open Itminus.Fodbus
open Itminus.Fodbus.Indication
open Itminus.Fiddlewares.Middleware
open FsToolkit.ErrorHandling

let mutable DOMSG_CACHE : DOsMsg option = None

let cacheCtx (ctx: MsgCtx) = 
    match ctx.Pending with
    | Some _ -> DOMSG_CACHE <- ctx.Pending
    | None -> ()

let handleBtnPressedMiddleware hintPath btnPin perform ctrl = 
    let h = handleBtnPressed hintPath btnPin ctrl perform 
    toContinuation cacheCtx h

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
    |> toContinuation cacheCtx

let mw_set_toggle pin = 
    let toggle (ctx: MsgCtx) =
        ctx.Evovle(fun m -> 
            let p = ctx.GetPendingDOs();
            let curr = p.Pin(pin)
            m.SetPin(pin, not curr) 
        ) 
        |> Some
        |> Task.FromResult
    toContinuation cacheCtx toggle


let makeProcessor ctrl = 
    MW_放行_提示 ctrl
    >=> 
    MW_放行_处理 ctrl
    //>=> mw_set_toggle DOPinAddr.DO2
    //>=> mw_set_toggle DOPinAddr.DO3
    //>=> mw_set_toggle DOPinAddr.DO4
    

let final =fun (ctx: MsgCtx) -> task { return ctx}

let proc (ctrl: ZLanCtrl) = taskResult{
    // make msg ctx
    let! dis = ctrl.ScanDIAsync() 
    if DOMSG_CACHE = None then
        let! dos = ctrl.ScanDOAsync()
        DOMSG_CACHE <- Some dos
    let dos = DOMSG_CACHE.Value
    let ctx = MsgCtx.createNew dis dos
    let processor = makeProcessor ctrl
    let! ctx' = processor ctx final
    return ctx'
}


let handle (ctrl: ZLanCtrl) = 
    task{
        do! ctrl.EnsureConnectedAsync(3000)
        let! res = proc ctrl
        match res with
        | Error e -> printfn "%s" e
        | Ok ctx' -> do! flushDOs ctrl ctx'
        do! Task.Delay(20)
    }


let runZlanCtrl (ip, port, readTimeout, writeTimeout, slaveAddr) =
    task {
        let ctrl = ZLanCtrl(ip, port, readTimeout, writeTimeout, slaveAddr);
        try 
            do! ctrl.EnsureConnectedAsync(1000);
            while true do
                do! handle ctrl 
        with
            | ex -> 
                printfn "%s" ex.Message
                try 
                    do! ctrl.DisconectAsync(1000)
                with
                    | ex -> ()
    }

task {
    while true do 
        do! runZlanCtrl("192.168.1.200",502,1000,1000,1uy)
}
|> ignore


System.Console.ReadLine()
