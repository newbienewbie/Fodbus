namespace Itminus.Fodbus

open Microsoft.Extensions.DependencyInjection

module Tests =

    open System
    open Xunit
    open Itminus.Fiddlewares
    open Itminus.Fiddlewares.Middleware

    let services = new ServiceCollection()
    services.AddLogging() |> ignore;
    let sp = services.BuildServiceProvider();

    [<Fact>]
    let ``Test All Executed`` () =

        let dis = DIsMsg([| true; true; true; true; true; true; true; true; |])
        let dos = DOsMsg([| true; true; true; true; true; true; true; true; |])
        let ctx : MsgCtx  =  MsgCtx.createNew dis dos sp

        let resetPin1 = fun ctx next ->
            let dos = MsgCtx.getCurrentDOs ctx
            dos.SetPin(DOPinAddr.DO1, false) |> ignore
            let ctx' = Some dos |> MsgCtx.withPendingDOs ctx
            next ctx'
            
        let resetPin3 = fun ctx next ->
            let dos = MsgCtx.getCurrentDOs ctx
            dos.SetPin(DOPinAddr.DO3, false) |> ignore
            let ctx' = Some dos |> MsgCtx.withPendingDOs ctx
            next ctx'

        let handle = resetPin1 >=> resetPin3
        let final = fun ctx -> ctx.Pending
        let msg = handle ctx final

        match msg with
        | Some msg -> 
            Assert.False(msg.Pin1)
            Assert.True(msg.Pin2)
            Assert.False(msg.Pin3)
        | None -> failwith "Ӧ��ֵ"


    [<Fact>]
    let ``Test Someone not Executed`` () =

        let dis = DIsMsg([| true; true; true; true; true; true; true; true; |])
        let dos = DOsMsg([| true; true; true; true; true; true; true; true; |])
        let ctx : MsgCtx  =  MsgCtx.createNew dis dos sp

        let resetPinWhen pin condition ctx = fun next ->
            match condition with
            | true ->
                let dos = MsgCtx.getCurrentDOs ctx
                DOsMsg.setPin pin false dos
                let ctx' = Some dos |> MsgCtx.withPendingDOs ctx
                next ctx'
            | false -> next ctx

        let resetPin1 = resetPinWhen DOPinAddr.DO1 true
        let resetPin3 = resetPinWhen DOPinAddr.DO3 true
        let resetPin7When = resetPinWhen DOPinAddr.DO7
        let resetPin8 = resetPinWhen DOPinAddr.DO8 true

        let handle = resetPin1 >=> resetPin3 >=> resetPin7When false>=> resetPin8
        let final = fun ctx -> ctx.Pending 
        let x = handle ctx final
        match x with
        | Some msg -> 
            Assert.False(msg.Pin1)
            Assert.False(msg.Pin3)
            Assert.True(msg.Pin7)
            Assert.False(msg.Pin8)
            Assert.True(msg.Pin2)
        | None -> failwith "Ӧ�з���ֵ"

        

    [<Fact>]
    let ``Test The Last one not Executed`` () =

        let dis = DIsMsg([| true; true; true; true; true; true; true; true; |])
        let dos = DOsMsg([| true; true; true; true; true; true; true; true; |])
        let ctx : Itminus.Fodbus.MsgCtx  = MsgCtx.createNew dis dos sp

        let resetPinWhen pin condition ctx = fun next ->
            match condition with
            | true ->
                let pending = MsgCtx.getCurrentDOs ctx
                DOsMsg.setPin pin false pending
                let ctx' = Some dos |> MsgCtx.withPendingDOs ctx
                next ctx'
            | false -> next ctx

        let resetPin1 = resetPinWhen DOPinAddr.DO1 true
        let resetPin3 = resetPinWhen DOPinAddr.DO3 true
        let resetPin7When = resetPinWhen DOPinAddr.DO7
        let resetPin8When = resetPinWhen DOPinAddr.DO8 

        let handle = resetPin1 >=> resetPin3 >=> resetPin7When false>=> resetPin8When true
        let final = fun ctx -> ctx.Pending
        let x = handle ctx final

        match x with
        | Some msg -> 
            Assert.False(msg.Pin1)
            Assert.True(msg.Pin2)
            Assert.False(msg.Pin3)
            Assert.True(msg.Pin7)
            Assert.False(msg.Pin8)
        | None -> failwith "Ӧ�з���ֵ"

        let ctx = MsgCtx.createNew dis dos sp
        let handle = resetPin1 >=> resetPin3 >=> resetPin7When true >=> resetPin8When false
        let final = fun ctx -> ctx.Pending
        let x = handle ctx final
        match x with
        | Some msg -> 
            Assert.False(msg.Pin1)
            Assert.True(msg.Pin2)
            Assert.False(msg.Pin3)
            Assert.False(msg.Pin7)
            Assert.True(msg.Pin8)
        | None -> failwith "Ӧ�з���ֵ"
