

	import flash.display.Sprite;
	import flash.text.TextField;

	class Test extends Sprite
	{
		static function main() {
			var txt:TextField = new TextField();

			var o:Dynamic = {red:0xFF0000, green:0x00FF00, blue:0x0000FF, msg:"hello"};

			var r:EReg = ~/hello/gi;

			var x:Xml = Xml.parse("<a>
							<b/>
						</a>");

			txt.text = o.red;
			
			if (o.msg == "hello") {
				trace("in if block");
			}
		}
	}
