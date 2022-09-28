namespace Fodbus

open System.Net.Sockets
open System.Threading

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
        | ScanDO of PointsAddr * AsyncReplyChannel<Result<bool[], string>>
        | WriteDO of  OnOffOutPin * AsyncReplyChannel<Result<unit, string>>
        | WriteHoldingRegisters of PointsAddr * uint16[] * AsyncReplyChannel<Result<unit, string>>

    let createAgent (ip: string, port: int, readTimeout: int, writeTimeout: int) = 
        task {
            let tcpClient = new TcpClient()
            do! tcpClient.ConnectAsync(ip, port)
            let factory = ModbusFactory()
            let master = factory.CreateMaster( tcpClient)
            master.Transport.ReadTimeout <- readTimeout;
            master.Transport.WriteTimeout <- writeTimeout;

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
                    | ScanDO (addr, channel ) ->  
                        try 
                            let! inputs =  master.ReadCoilsAsync(addr.SlaveAddr, addr.Offset, addr.Count) |> Async.AwaitTask
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
                                System.Console.WriteLine($"ON {addr.CoilAddr}")
                                channel.Reply(Ok ())
                                System.Console.WriteLine($"On {addr.CoilAddr} Reply")
                            | OnOffOutPin.Off addr -> 
                                do! master.WriteSingleCoilAsync(addr.SlaveAddr, addr.CoilAddr, false) |> Async.AwaitTask
                                System.Console.WriteLine($"Off {addr.CoilAddr}")
                                channel.Reply(Ok ())
                                System.Console.WriteLine($"Off {addr.CoilAddr} Reply")
                            return! loop ()
                        with 
                            | msg -> 
                                System.Console.WriteLine($"ERR:{msg}")
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



    type DOPinAddr =
        | DO1 = 0x10us
        | DO2 = 0x11us
        | DO3 = 0x12us
        | DO4 = 0x13us
        | DO5 = 0x14us
        | DO6 = 0x15us
        | DO7 = 0x16us
        | DO8 = 0x17us

    type DIPinAddr =
        | DI1 = 0x00us
        | DI2 = 0x01us
        | DI3 = 0x02us
        | DI4 = 0x03us
        | DI5 = 0x04us
        | DI6 = 0x05us
        | DI7 = 0x06us
        | DI8 = 0x07us

    type Ctrl (ip: string, port: int, readTimeout: int, writeTimeout: int, slaveAddr: byte) =

        let mutable _tcpClient = Unchecked.defaultof<TcpClient>
        let mutable _agent: MailboxProcessor<Message> option = None

        let _sema = new SemaphoreSlim(1,1)

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
                let! ( tcpClient, agent )= createAgent (ip, port, readTimeout, writeTimeout)
                _tcpClient <- tcpClient
                _agent <- Some agent
            }


        member this.EnsureConnectedAsync(timeout: int) = task{
            let! entered = _sema.WaitAsync(timeout)
            if entered then
                try
                     if this.Connected then 
                         ()
                     else 
                         do! this.InitializeAsync()
                finally
                    _sema.Release() |> ignore
            else 
                failwith $"获取连接ZLAN CTRL的信号锁超时(超时时间{timeout})"
        }


        member this.DisconectAsync(timeout: int) = task {
            let! entered = _sema.WaitAsync(timeout)
            if entered then
                try
                    _agent <- None
                    _tcpClient.Close()
                    _tcpClient <- null
                finally
                    _sema.Release() |> ignore
            else 
                failwith $"获取连接ZLAN CTRL的信号锁超时(超时时间{timeout})"
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
    
        member this.OnAsync(pin :DOPinAddr) = uint16 pin |> this.OnAsync
        member this.OffAsync(pin :DOPinAddr) = uint16 pin |> this.OffAsync


        member this.ScanDIAsync(offset: uint16, count: uint16) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
                _agent.PostAndAsyncReply(fun channel ->  ScanDI (addr, channel) ) |> Async.StartAsTask
            | None -> task { return Error "尚未初始化"}

        /// 单独扫描1个
        member this.ScanDIAsync(pin: DIPinAddr) =
            let offset = uint16 pin
            let count = 1us
            task { 
                let! r = this.ScanDIAsync(offset, count) 
                let res = match r with | Ok x -> Ok x.[0] | Error e -> Error e
                return res
            }

        /// 一次性扫描8个
        member this.ScanDIAsync() =
            let offset = uint16 DIPinAddr.DI1
            let count = 8us
            this.ScanDIAsync(offset, count) 


        member this.ScanAIAsync(offset: uint16, count: uint16) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
                _agent.PostAndAsyncReply(fun channel ->  ScanAI (addr, channel) ) |> Async.StartAsTask
            | None -> task { return Error "尚未初始化"}


        member this.ScanDOAsync(offset: uint16, count: uint16) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
                _agent.PostAndAsyncReply(fun channel ->  ScanDO (addr, channel) ) |> Async.StartAsTask
            | None -> task { return Error "尚未初始化"}

        member this.ScanDOAsync(pin: DOPinAddr) =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = uint16 pin; Count = 1us} 
                task {
                    let! r = _agent.PostAndAsyncReply(fun channel ->  ScanDO (addr, channel) ) 
                    let res = match r with | Ok r -> Ok r[0] | Error s -> Error s 
                    return res
                }
            | None -> task { return Error "尚未初始化"}

        member this.ScanDOAsync() =
            match _agent with
            | Some _agent ->
                let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = uint16  DOPinAddr.DO1; Count = 8us} 
                task {
                    let! r = _agent.PostAndAsyncReply(fun channel ->  ScanDO (addr, channel) ) 
                    let res = match r with | Ok r -> Ok r | Error s -> Error s 
                    return res
                }
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



