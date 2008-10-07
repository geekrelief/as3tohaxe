package example
{
	public class AsWingApplication extends Sprite
	{
		private static var labelPrefix : String = "Number of button clicks: ";
	    private var numClicks : int = 0;
		
		public function AsWingApplication(){
			super();
			createUI();
		}
	}
}
