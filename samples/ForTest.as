package
{
	import flash.display.Sprite;
	import flash.text.TextField;

	public class ForTest extends Sprite
	{
		public function ForTest(){
			var txt:TextField = new TextField();
			
			for(var i:uint = 0; i < 10; i++)
			{
				txt.appendText(i+" ");
			}

			for(var j = 5; j < 10; j++){
				txt.appendText(j+" ");
			}

			addChild(txt);

			var arr:Array = [1,2,3]
			for each( k in arr) {
				trace(k);
			}
			
			for(var l:int = 0; l < 32; l+=8) {
				trace(l);
			}
		}
	}
}
