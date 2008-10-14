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
			
			addChild(txt);
		}
	}
}
