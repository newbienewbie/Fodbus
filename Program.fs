open System.Net.Sockets

module ZLanCtrl = 

    open NModbus

    [<Struct>]
    type CoilAddr = {
        SlaveAddr: byte
        CoilAddr : uint16
    }

    [<Struct>]
    type PointsAddr = {
        SlaveAddr: byte
        Offset: uint16
        Count: uint16
    }

    [<RequireQualifiedAccess>]
    type OnOffOutPin = 
        | On of CoilAddr 
        | Off of CoilAddr

    type Message = 
        | ScanDI of PointsAddr * AsyncReplyChannel<Result<bool[], string>>
        | ScanAI of PointsAddr * AsyncReplyChannel<Result<uint16[], string>>
        | ScanHoldingRegisters of PointsAddr * AsyncReplyChannel<Result<uint16[], string>>
        | WriteDO of  OnOffOutPin * AsyncReplyChannel<Result<unit, string>>
        | WriteHoldingRegisters of PointsAddr * uint16[] * AsyncReplyChannel<Result<unit, string>>

    let createAgent (ip: string, port: int) = 
        task {
            let tcpClient = new TcpClient()
            do! tcpClient.ConnectAsync(ip, port)
            let factory = ModbusFactory()
            let master = factory.CreateMaster( tcpClient)

            let agent = MailboxProcessor.Start (fun agent ->
                let rec loop ()= async {
                    let! message = agent.Receive() 
                    match message with
                    | ScanDI (addr, channel ) -> 
                        try 
                            let! inputs =  master.ReadInputsAsync(addr.SlaveAddr, addr.Offset, addr.Count) |> Async.AwaitTask
                            channel.Reply(Ok inputs)
                            return! loop ()
                        with 
                            | msg -> 
                                channel.Reply(Error msg.Message)
                                return! loop();
                    | ScanAI (addr, channel ) ->  
                        try 
                            let! inputs =  master.ReadInputRegistersAsync(addr.SlaveAddr, addr.Offset, addr.Count) |> Async.AwaitTask
                            channel.Reply(Ok inputs)
                            return! loop ()
                        with 
                            | msg -> 
                                channel.Reply(Error msg.Message)
                                return! loop();
                    | ScanHoldingRegisters (addr, channel) -> 
                        try 
                            let! inputs =  master.ReadHoldingRegistersAsync(addr.SlaveAddr, addr.Offset, addr.Count) |> Async.AwaitTask
                            channel.Reply(Ok inputs)
                            return! loop ()
                        with 
                            | msg -> 
                                channel.Reply(Error msg.Message)
                                return! loop();
                    | WriteDO (action, channel) ->
                        try 
                            match action with
                            | OnOffOutPin.On addr -> 
                                do! master.WriteSingleCoilAsync(addr.SlaveAddr, addr.CoilAddr, true) |> Async.AwaitTask
                                channel.Reply(Ok ())
                            | OnOffOutPin.Off addr -> 
                                do! master.WriteSingleCoilAsync(addr.SlaveAddr, addr.CoilAddr, false) |> Async.AwaitTask
                                channel.Reply(Ok ())
                            return! loop ()
                        with 
                            | msg -> 
                                channel.Reply(Error msg.Message)
                                return! loop();
                    | WriteHoldingRegisters (addr, data, channel) -> 
                        try 
                            do! master.WriteMultipleRegistersAsync(addr.SlaveAddr, addr.Offset, data) |> Async.AwaitTask
                            channel.Reply(Ok ())
                            return! loop ()
                        with 
                            | msg -> 
                                channel.Reply(Error msg.Message)
                                return! loop();
                }
                loop ()
            )
            return (tcpClient, agent)
        }



    type CoilPin =
        | Out1 = 0x10us
        | Out2 = 0x11us
        | Out3 = 0x12us
        | Out4 = 0x13us
        | Out5 = 0x14us
        | Out6 = 0x15us
        | Out7 = 0x16us
        | Out8 = 0x17us

    type Ctrl (ip: string, port: int, readTimeout: int, writeTimeout: int, slaveAddr: byte) =

        let mutable _tcpClient = Unchecked.defaultof<TcpClient>
        let mutable _agent: MailboxProcessor<Message> option = None

        let createAddr (coilAdrr) = 
            { SlaveAddr= slaveAddr; CoilAddr = coilAdrr}

        member this.IpAddr with get() = ip
        member this.Port with get() = port
        member this.ReadTimeout with get() = readTimeout
        member this.WriteTimeout with get() = writeTimeout

        member this.Connected with get () =
            match _tcpClient with
            | null -> false
            | _ when isNull(_tcpClient.Client) -> false
            | _ when _tcpClient.Client.Connected |> not -> false
            | _ -> 
                let s = _tcpClient.Client;
                let part1 = s.Poll(1000, SelectMode.SelectRead)
                let part2 = s.Available = 0
                match part1, part2 with
                | true, true -> false
                | _ -> true
                 
        member this.InitializeAsync() =
            task {
                let! ( tcpClient, agent )= createAgent (ip, port)
                _tcpClient <- tcpClient
                _agent <- Some agent
            }

        member this.OnAsync( coilAdrr ) =
            match _agent with
            | Some agent -> 
                let input = coilAdrr |> createAddr |> OnOffOutPin.On 
                agent.PostAndAsyncReply(fun channel -> Message.WriteDO (input, channel)) |> Async.StartAsTask
            | None -> 
                task { return Error "尚未初始化"}
             
        member this.OffAsync( coilAdrr ) =
            match _agent with
            | Some agent -> 
                let input = coilAdrr |> createAddr |> OnOffOutPin.Off
                agent.PostAndAsyncReply(fun channel ->  Message.WriteDO (input, channel)) |> Async.StartAsTask
            | None -> 
                task{ return Error "尚未初始化"}
    
        member this.On(pin :CoilPin) = uint16 pin |> this.OnAsync
        member this.Off(pin :CoilPin) = uint16 pin |> this.OffAsync


        member this.ScanDIAsync(offset: uint16, count: uint16) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
                _agent.PostAndAsyncReply(fun channel ->  ScanDI (addr, channel) ) |> Async.StartAsTask
            | None -> task { return Error "尚未初始化"}

        member this.ScanAIAsync(offset: uint16, count: uint16) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
                _agent.PostAndAsyncReply(fun channel ->  ScanAI (addr, channel) ) |> Async.StartAsTask
            | None -> task { return Error "尚未初始化"}

        member this.ScanHoldingRegistersAsync(offset: uint16, count: uint16) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
                _agent.PostAndAsyncReply(fun channel ->  ScanHoldingRegisters (addr, channel) ) |> Async.StartAsTask
            | None -> task { return Error "尚未初始化"}

        member this.WriteHoldingRegistersAsync(offset: uint16, count: uint16, data: uint16[]) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
                _agent.PostAndAsyncReply(fun channel ->  WriteHoldingRegisters (addr, data, channel) ) |> Async.StartAsTask
            | None -> task { return Error "尚未初始化"}



task{
    let ctrl = new ZLanCtrl.Ctrl("192.168.1.200",502, 1000, 1000, 1uy)
    do! ctrl.InitializeAsync()
    let mutable counter = 1
    while true do
        let! s = ctrl.On(ZLanCtrl.CoilPin.Out2)
        do! Async.Sleep 1000
        let! s = ctrl.Off(ZLanCtrl.CoilPin.Out2)
        do! Async.Sleep 1000
        counter <- counter + 1
}
|> ignore

System.Console.Read()
