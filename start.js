// Meadow 付属の fiber.exe を GNU emacs で使うと xlsx の扱いがうまくいかないようなので
// fiber.exe 同等の機能を javascript で実装

if (WScript.arguments.length ==0)
{
	WScript.echo("no parameter.");
	WScript.Quit(0);
}

var wsh = new ActiveXObject("WScript.Shell");
// Run に直接パラメータを渡すと起動しないファイルがある (xlsx など)
// なので、 cmd.exe から start 付きで呼んでやる
wsh.Run("%ComSpec% /c start " + WScript.arguments(0), false);
