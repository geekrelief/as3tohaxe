/**
 *  author: Alva sun
 *  site : www.alvas.cn
 * 
 *  need AsWingA3
 *  2007-05-07
 */
package example
{
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import org.aswing.BorderLayout;
	import org.aswing.FlowLayout;
	import org.aswing.GridLayout;
	import org.aswing.Insets;
	import org.aswing.JButton;
	import org.aswing.JFrame;
	import org.aswing.JLabel;
	import org.aswing.JPanel;
	import org.aswing.border.EmptyBorder;
	import org.aswing.geom.IntDimension;
	
	public class AsWingApplication extends Sprite
	{
		private static var labelPrefix : String = "Number of button clicks: ";
	    private var numClicks : int = 0;
	    private var label : JLabel;
		private var button : JButton;
		
		public function AsWingApplication(){
			super();
			createUI();
		}
		
		private function createUI() : void{
			var frame : JFrame = new JFrame( this, "AsWingApplication" );
			frame.getContentPane().append( createCenterPane() );
			frame.setSize(new IntDimension( 200, 120 ) );
			frame.show();
	    	var test : int = 0;
		}
		
		private function createCenterPane() : JPanel{
			var pane : JPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
			label = new JLabel(labelPrefix+"0");
			button = new JButton("I'm a AsWing button!");
			pane.append(button);
			pane.append(label);
			pane.setBorder(new EmptyBorder(null, new Insets(10,5,10,5)));
			initHandlers();
			return pane;
		}
		
		private function initHandlers() : void{
			//button.addActionListener( __pressButton );
			button.addEventListener(MouseEvent.MOUSE_UP, __pressButton);
		}
		
		private function __pressButton( e : Event ) : void{
			numClicks++;
			label.setText(labelPrefix+numClicks);
			label.revalidate();
		}
	}
}
