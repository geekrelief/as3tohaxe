package
{
	import flash.display.Sprite;
	import flash.text.TextField;

	public class Test extends Sprite
	{
		public function Test(){
			var txt:TextField = new TextField();

			var o:Object = {red:0xFF0000, green:0x00FF00, blue:0x0000FF, msg:"hello"};

			var r:RegExp = /hello/gi;

			var x:XML = <a>
							<b/>
						</a>;

			//txt.text = o.red;
			var ar:Array = [1,2,3,4];

			var a:int = 0;
            var i:int = (a);

            var f:Function = function lambda():String { return "f";};
			
			txt.appendText(a+" "+i+" "+ f());

			addChild(txt);
			
			if (o.msg == "hello") {
				trace("in if block");
			}
		}
	}
}
