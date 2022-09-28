namespace Itminus.Fodbus


/// DI 针脚地址
type DIPinAddr =
    | DI1 = 0x00us
    | DI2 = 0x01us
    | DI3 = 0x02us
    | DI4 = 0x03us
    | DI5 = 0x04us
    | DI6 = 0x05us
    | DI7 = 0x06us
    | DI8 = 0x07us


/// DO针脚地址
type DOPinAddr =
    | DO1 = 0x10us
    | DO2 = 0x11us
    | DO3 = 0x12us
    | DO4 = 0x13us
    | DO5 = 0x14us
    | DO6 = 0x15us
    | DO7 = 0x16us
    | DO8 = 0x17us


/// DI消息
[<Struct>]
type DIsMsg (pins: bool[]) = 
    member this.Pin1 with get() = pins[0]
    member this.Pin2 with get() = pins[1]
    member this.Pin3 with get() = pins[2]
    member this.Pin4 with get() = pins[3]
    member this.Pin5 with get() = pins[4]
    member this.Pin6 with get() = pins[5]
    member this.Pin7 with get() = pins[6]
    member this.Pin8 with get() = pins[7]

    member this.Pin(pin :DIPinAddr) = 
        match pin with
        | DIPinAddr.DI1 -> this.Pin1
        | DIPinAddr.DI2 -> this.Pin2
        | DIPinAddr.DI3 -> this.Pin3
        | DIPinAddr.DI4 -> this.Pin4
        | DIPinAddr.DI5 -> this.Pin5
        | DIPinAddr.DI6 -> this.Pin6
        | DIPinAddr.DI7 -> this.Pin7
        | DIPinAddr.DI8 -> this.Pin8
        | _ -> failwith $"未知的PIN号(Value={pin}"

/// DO消息
[<Struct>]
type DOsMsg (pins: bool[]) = 
    member this.Pin1 with get() = pins[0] 
    member this.Pin2 with get() = pins[1] 
    member this.Pin3 with get() = pins[2] 
    member this.Pin4 with get() = pins[3] 
    member this.Pin5 with get() = pins[4] 
    member this.Pin6 with get() = pins[5] 
    member this.Pin7 with get() = pins[6] 
    member this.Pin8 with get() = pins[7] 

    /// 获取针脚状态
    member this.Pin(pin:DOPinAddr) = 
        match pin with
        | DOPinAddr.DO1 -> pins[0] 
        | DOPinAddr.DO2 -> pins[1] 
        | DOPinAddr.DO3 -> pins[2] 
        | DOPinAddr.DO4 -> pins[3] 
        | DOPinAddr.DO5 -> pins[4] 
        | DOPinAddr.DO6 -> pins[5] 
        | DOPinAddr.DO7 -> pins[6] 
        | DOPinAddr.DO8 -> pins[7] 
        | _ -> failwith $"未知的PIN号(Value={pin}"

    /// 警告：会直接修改原值
    member this.SetPin(pin:DOPinAddr, onoff: bool) = 
        match pin with
        | DOPinAddr.DO1 -> pins[0] <- onoff  ; this
        | DOPinAddr.DO2 -> pins[1] <- onoff  ; this
        | DOPinAddr.DO3 -> pins[2] <- onoff  ; this
        | DOPinAddr.DO4 -> pins[3] <- onoff  ; this
        | DOPinAddr.DO5 -> pins[4] <- onoff  ; this
        | DOPinAddr.DO6 -> pins[5] <- onoff  ; this
        | DOPinAddr.DO7 -> pins[6] <- onoff  ; this
        | DOPinAddr.DO8 -> pins[7] <- onoff  ; this
        | _ -> failwith $"未知的PIN号(Value={pin}"

    /// 返回一份拷贝
    member this.CopyValues() = pins |> Array.map id 

    /// 返回一份拷贝
    member this.Copy() = this.CopyValues() |> DOsMsg 

module DOsMsg =

    let setPin (pin: DOPinAddr) (onoff: bool) (msg: DOsMsg) =
        msg.SetPin(pin, onoff)
        ()
        

type MsgCtx = {
    DIs : DIsMsg
    DOs : DOsMsg
    Pending: DOsMsg option
}

module MsgCtx = 

    let createNew disMsg dosMsg = 
        { DIs = disMsg; DOs = dosMsg; Pending = None }

    let getPendingDOs (ctx: MsgCtx) =
        match ctx.Pending with
        | None -> ctx.DOs.Copy()
        | Some dos -> dos

    let withPendingDOs (pending: DOsMsg) (ctx: MsgCtx) =
        {ctx with Pending = Some pending}

type MsgCtx with

    member this.GetPendingDOs() = 
        MsgCtx.getPendingDOs(this)

    member this.WithPendingDOs(pending: DOsMsg) = 
        MsgCtx.withPendingDOs pending

    /// 计算返回新的ctx
    member this.Evovle(evolve) : MsgCtx= 
        let pending = this.GetPendingDOs()
        let pending' = evolve pending
        MsgCtx.withPendingDOs pending' this


