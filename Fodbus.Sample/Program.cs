// See https://aka.ms/new-console-template for more information
using static Fodbus.ZLanCtrl;


async Task PinIO( Ctrl ctrl ,DOPinAddr pin)
{
    try{

        await ctrl.EnsureConnectedAsync(-1);

        var s = await ctrl.OnAsync(pin);
        if(s.IsError)
        {
            throw new Exception($"读PIN{pin}错误");
        }
        s = await ctrl.OffAsync(pin);
        if(s.IsError)
        {
            throw new Exception($"写PIN{pin}错误");
        }
    }
    catch(Exception ex)
    {
        Console.WriteLine(ex.ToString());
        try
        {
            await ctrl.DisconectAsync(-1);
        }
        catch { 
        }

    }
}

async Task IoLoop(Ctrl ctrl, int mode)
{
    while(true)
    {
        if (mode == 0)
        {
            await PinIO(ctrl, DOPinAddr.DO1);
            await PinIO(ctrl, DOPinAddr.DO2);
        }
        else if (mode == 1)
        { 
            await PinIO(ctrl, DOPinAddr.DO4);
            await PinIO(ctrl, DOPinAddr.DO5);
        }
    }
}

try{

    var  ctrl = new Ctrl("192.168.1.200",502, 1000,1000, 1);
    await ctrl.InitializeAsync();

    for(var i =0 ;i < 2; i++)
    {
        var mode = i;
        Task.Run(() => IoLoop(ctrl, mode));
    }
    Console.ReadLine();
}
catch(Exception ex)
{

    Console.ReadLine();
}

