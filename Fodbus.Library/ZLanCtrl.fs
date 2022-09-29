namespace Itminus.Fodbus

open System.Net.Sockets
open System.Threading
open System.Threading.Tasks





/// 线圈地址
type [<Struct>]CoilAddr = {
    SlaveAddr: byte
    CoilAddr : uint16
}
/// 线圈开关
[<RequireQualifiedAccess>]
type DOPinOnOff = 
    | On of CoilAddr 
    | Off of CoilAddr

/// 点位地址
[<Struct>]
type PointsAddr = {
    SlaveAddr: byte
    Offset: uint16
    Count: uint16
}

type Message = 
    | ScanDI of PointsAddr * AsyncReplyChannel<Result<bool[], string>>
    | ScanAI of PointsAddr * AsyncReplyChannel<Result<uint16[], string>>
    | ScanHoldingRegisters of PointsAddr * AsyncReplyChannel<Result<uint16[], string>>
    | ScanDO of PointsAddr * AsyncReplyChannel<Result<bool[], string>>
    | WriteDOPin of  DOPinOnOff * AsyncReplyChannel<Result<unit, string>>
    | WriteDOPins of PointsAddr * bool[] * AsyncReplyChannel<Result<unit, string>>
    | WriteHoldingRegisters of PointsAddr * uint16[] * AsyncReplyChannel<Result<unit, string>>



/// 执行动作
type PerformAgentAction<'a> = MailboxProcessor<Message> -> Task<Result<'a, string>>

module ZLanCtrl = 
    open NModbus

    let internal createAgent (ip: string, port: int, readTimeout: int, writeTimeout: int) = 
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
                    | WriteDOPin (action, channel) ->
                        try 
                            match action with
                            | DOPinOnOff.On addr -> 
                                do! master.WriteSingleCoilAsync(addr.SlaveAddr, addr.CoilAddr, true) |> Async.AwaitTask
                                System.Console.WriteLine($"ON {addr.CoilAddr}")
                                channel.Reply(Ok ())
                                System.Console.WriteLine($"On {addr.CoilAddr} Reply")
                            | DOPinOnOff.Off addr -> 
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
                    | WriteDOPins (addr, data, channel) ->
                        try 
                            do! master.WriteMultipleCoilsAsync(addr.SlaveAddr, addr.Offset, data) |> Async.AwaitTask
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


type ZLanCtrl (ip: string, port: int, readTimeout: int, writeTimeout: int, slaveAddr: byte) =

    let mutable _tcpClient = Unchecked.defaultof<TcpClient>
    let mutable _agent: MailboxProcessor<Message> option = None

    let _sema = new SemaphoreSlim(1,1)

    let createAddr (coilAdrr) = 
        { SlaveAddr= slaveAddr; CoilAddr = coilAdrr}

    let performAgentIO (action: PerformAgentAction<'a>) =
        match _agent with
        | Some agent -> action agent
        | None -> task { return Error "ZLAN CTRL尚未初始化"}

    member this.IpAddr with get() = ip
    member this.Port with get() = port
    member this.ReadTimeout with get() = readTimeout
    member this.WriteTimeout with get() = writeTimeout



    /// 是否连接
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
             
    /// 初始化
    member private this.InitializeAsync() =
        task {
            let! ( tcpClient, agent )= ZLanCtrl.createAgent (ip, port, readTimeout, writeTimeout)
            _tcpClient <- tcpClient
            _agent <- Some agent
        }


    /// 确保连接，若未能在规定时间内成功连接上，向外抛出异常
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


    /// 断开连接，若未能在规定时间内完成断开，向外抛出异常
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



    /// 对指定PIN输出ON信号
    member private this.OnAsync( coilAdrr ) =
        let action : PerformAgentAction<unit>  = fun agent ->
            let input = coilAdrr |> createAddr |> DOPinOnOff.On 
            agent.PostAndAsyncReply(fun channel -> Message.WriteDOPin (input, channel)) |> Async.StartAsTask
        performAgentIO action
         
    /// 对指定PIN输出ON信号
    member this.OnAsync(pin :DOPinAddr) = uint16 pin |> this.OnAsync

    /// 对指定PIN输出OFF信号
    member private this.OffAsync( coilAdrr ) =
        let action : PerformAgentAction<unit>  = fun agent ->
            let input = coilAdrr |> createAddr |> DOPinOnOff.Off
            agent.PostAndAsyncReply(fun channel ->  Message.WriteDOPin (input, channel)) |> Async.StartAsTask
        performAgentIO action

    /// 对指定PIN输出OFF信号
    member this.OffAsync(pin :DOPinAddr) = uint16 pin |> this.OffAsync


    /// 写DO
    member private this.WriteDOsAsync(offset: uint16, data: bool[]) =
        let action : PerformAgentAction<unit>  = fun agent ->
            let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = uint16 data.Length} 
            agent.PostAndAsyncReply(fun channel ->  WriteDOPins(addr, data, channel) ) |> Async.StartAsTask
        performAgentIO action


    /// 写DO
    member this.WriteDOsAsync(offset: DOPinAddr, data: bool[]) =
        let offset' = uint16 offset
        this.WriteDOsAsync(offset', data);

    /// 连续扫描1个
    member private this.ScanAIAsync(offset: uint16, count: uint16) =
        let action : PerformAgentAction<uint16[]>  = fun agent ->
            let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
            agent.PostAndAsyncReply(fun channel ->  ScanAI (addr, channel) ) |> Async.StartAsTask
        performAgentIO action





    /// 扫从offset开始，连续count个DI。其中DI1的offset为0us
    member private this.ScanDIAsync(offset: uint16, count: uint16) =

        let action : PerformAgentAction<bool[]>  = fun agent ->
            let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
            agent.PostAndAsyncReply(fun channel ->  ScanDI (addr, channel) ) |> Async.StartAsTask
        performAgentIO action

    /// 一次性扫描8个
    member this.ScanDIAsync() =
        let offset = uint16 DIPinAddr.DI1
        let count = 8us
        task {
            let! dis = this.ScanDIAsync(offset, count) 
            return 
                match dis with
                | Ok msg -> DIsMsg msg |> Ok
                | Error e -> Error e
        }

    /// 扫指定PIN的DI
    member this.ScanDIAsync(pin: DIPinAddr) =
        let offset = uint16 pin
        let count = 1us
        task { 
            let! r = this.ScanDIAsync(offset, count) 
            let res = match r with | Ok x -> Ok x.[0] | Error e -> Error e
            return res
        }





    /// 扫从offset开始，连续count个DO。其中DO1的offset为16
    member private this.ScanDOAsync(offset: uint16, count: uint16) =
        let action : PerformAgentAction<bool[]>  = fun agent ->
            let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
            agent.PostAndAsyncReply(fun channel ->  ScanDO (addr, channel) ) |> Async.StartAsTask
        performAgentIO action

    /// 扫全部DO
    member this.ScanDOAsync() =
        let offset = uint16 DOPinAddr.DO1
        let count = 8us
        task {
            let! dos= this.ScanDOAsync(offset, count)
            return 
                match dos with
                | Ok msg -> DOsMsg msg |> Ok
                | Error e -> Error e
        }

    /// 扫单个DO
    member this.ScanDOAsync(pin: DOPinAddr) =
        let offset = uint16 pin
        let count = 1us
        task {
            let! r= this.ScanDOAsync(offset, count)
            let res = match r with | Ok r -> Ok r[0] | Error s -> Error s 
            return res
        }


    member this.ScanHoldingRegistersAsync(offset: uint16, count: uint16) =
        let action : PerformAgentAction<uint16[]>  = fun agent ->
            let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
            agent.PostAndAsyncReply(fun channel ->  ScanHoldingRegisters (addr, channel) ) |> Async.StartAsTask
        performAgentIO action


    member this.WriteHoldingRegistersAsync(offset: uint16, count: uint16, data: uint16[]) =
        let action : PerformAgentAction<unit>  = fun agent ->
            let addr : PointsAddr = { SlaveAddr = slaveAddr; Offset = offset; Count = count } 
            agent.PostAndAsyncReply(fun channel ->  WriteHoldingRegisters (addr, data, channel) ) |> Async.StartAsTask
        performAgentIO action

