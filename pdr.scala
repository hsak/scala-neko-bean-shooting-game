package pdr;

import javax.swing._;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom._;
import java.awt.Color;
import java.awt.BasicStroke;
import java.awt.Shape;
import java.awt.Stroke;

import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.StringBuilder;
import scala.collection.mutable.HashMap

case class PdrPoint(curve:Boolean,x:Double,y:Double)
case class PdrPolygon(lineClose:Boolean, lineWidth:Int,hasLine:Boolean,lineColor:Color,grdMode:Int,color:Color,
	grdX:Double,grdY:Double,grdRotate:Double,grdScaleX:Double,grdScaleY:Double,grdColor:Color,
	group:Boolean,groupno:Int,path:Boolean,pathno:Int,points:ArrayBuffer[PdrPoint]
);

case class PdrShape(width:Int,height:Int,polygons:ArrayBuffer[PdrPolygon])

class String_readAll(filename:String) {
	def readAll():String = {
			var in:InputStreamReader = null
			var strb = new StringBuilder
			try {
				try {
					in = new InputStreamReader(new FileInputStream(filename), "SJIS");
				} catch {
				case _ => in = new InputStreamReader(getClass().getResourceAsStream(filename), "SJIS");
				}
				def loop() {
						val c:Int = in.read()
						if(c == -1) {
						} else {
							strb.append(c.asInstanceOf[Char])
							loop ()
						}
				}
				loop()
				in.close()
			} catch {
			case _ => println("read error:"+filename); return null
			}
			strb.toString();
	}
}

case class SWFShape(color:Color,var shape:Shape,lineColor:Color,stroke:Stroke){
	var shapes:GeneralPath = null;
	def append(s:Shape){
		if(shapes == null) {
			shapes = new GeneralPath()
			shapes.append(shape, false)
			shape = shapes
		}
		shapes.append(s,false)
	}
	def draw(g2:Graphics2D){
		if(color != null) {
			g2.setColor(color);
			g2.fill(shape);
		}
		if(stroke != null) {
			g2.setColor(lineColor);
			g2.setStroke(stroke);
			g2.draw(shape);
		}
	}
}
case class SWFSprite(paths:Array[SWFShape]) {
	def draw(g2:Graphics2D) {
		for(p <- paths ) {
			p.draw(g2);
		}
	}
}

object PDR {
	implicit def toReadAll(filename:String) = new String_readAll(filename);


	def loadSWFSprite(name:String):SWFSprite = {
		loadSWFSprite(name,".")
	}
	def loadSWFSprite(name:String, path:String):SWFSprite = {
		loadSWFSprite(name,path,0)
	}
	def loadSWFSprite(name:String, path:String, mode:Int):SWFSprite = {
		pdr2swf(loadPDR(name,path,mode))
	}

	def loadPDR(name:String):PdrShape = {
		loadPDR(name,".")
	}
	def loadPDR(name:String, path:String):PdrShape = {
		loadPDR(name,path,0)
	}
	def col(c:Int):Color = {
		val r = (c & 0xff);
		val g = ((c>>8) & 0xff);
		val b = ((c>>16) & 0xff);
		var a = ((c>>24) & 0xff);
		new Color(r,g,b,a);
	}
	val LEFT = 0;
	val CENTER = 1;
	val RIGHT = 2;
	val TOP = 0;
	val VCENTER = 4;
	val BOTTOM = 8;
	/// pdrファイル読込
	def loadPDR(name:String, path:String, mode:Int):PdrShape = {

		val filename = path + "/" + name;
		val dt = filename.readAll();
		val line = dt.split("\r\n");
		val numPath = line(3).toInt;
		val line1 = line(1).split(",");

		val width = line1(0).toInt;
		val height = line1(1).toInt;
		val centerX =
			if((mode & CENTER) != 0) width/2
			else if((mode & RIGHT) != 0) width
			else 0;
		val centerY =
			if((mode & VCENTER) != 0) height/2
			else if((mode & BOTTOM) != 0) height
			else 0;
		var lno = 4;

		val polys = new ArrayBuffer[PdrPolygon]();

		for ( i <- 1 to numPath) {

			val pathHead = line(lno).split(",");
			lno += 1;
			val numAnc = pathHead(0).toInt + 1;
			val lineClose = pathHead(1) == "#TRUE#";
			val lineWidth = pathHead(2).toInt;
			val hasLine = pathHead(3).toInt != 0;
			val lineColor = col(pathHead(4).toInt);

			val grdMode = pathHead(5).toInt;

			val color = col(pathHead(6).toInt);

			val grdX = pathHead(7).toDouble/20-centerX;
			val grdY = pathHead(8).toDouble/20-centerY;
			val grdRotate = -pathHead(9).toDouble;
			val grdScaleX = pathHead(10).toDouble;
			val grdScaleY = pathHead(11).toDouble;
			
			val grdColor = col(pathHead(12).toInt);

			val group = pathHead(13)=="#TRUE#";
			val groupno = pathHead(14).toInt;
			val path = pathHead(15)=="#TRUE#";
			val pathno = pathHead(16).toInt;
			val points = new ArrayBuffer[PdrPoint]();
			lno += 1;
			for (j <- 1 to numAnc) {
				val s = line(lno).split(",");
				lno += 1;
				val x = s(1).toDouble;
				val y = s(2).toDouble;
				points += new PdrPoint(s(0).toInt != 0 && (lineClose|| !(j==1||j==numAnc)), x/20-centerX, y/20-centerY);
			}
			val output = new PdrPolygon(lineClose,lineWidth,hasLine, lineColor, grdMode,color,
				grdX,grdY,grdRotate,grdScaleX,grdScaleY,grdColor,
				group,groupno,path,pathno,points
			);

			polys += output;
		}
		return new PdrShape(width,height,polys);
	}

	def pdr2swf(shape:PdrShape):SWFSprite = {
		val paths = new ArrayBuffer[SWFShape];

		val pathMap = new HashMap[Int,Int]();

		for(i <- 0 to shape.polygons.size - 1) {
			val s = new GeneralPath();
			val poly = shape.polygons(i);
			val points = poly.points;
			val max = if(poly.lineClose) points.length else points.length - 1
			for(j <- 0 to max) {
				val no = j % points.length;
				val x1 = points(no).x;
				val y1 = points(no).y;
				
				if(points(no).curve) {
					val no1 = (no + 1) % points.length;
					val (x2,y2) = 
						if(points(no1).curve) (
							(x1+points(no1).x)/2 ,
							(y1+points(no1).y)/2 )
						else (points(no1).x, points(no1).y)
					
					if(j==0) s.moveTo(x2,y2);
					else		 s.quadTo(x1,y1,x2,y2);
				} else {
					if(j==0) s.moveTo(x1,y1);
					else		 s.lineTo(x1,y1);
				}
			}
			//s.closePath();

			def stroke():Stroke = {
				if(poly.hasLine) {
					new BasicStroke(
						poly.lineWidth/20.0f,
						BasicStroke.CAP_ROUND,
						BasicStroke.JOIN_ROUND);
				} else {
					null
				}
			}
			def polyColor():Color = {
				poly.grdMode match {
				case 1 => poly.color
				case 2 => null
				case _ => null
				}
			}
			val no = poly.pathno

			if(poly.path && pathMap.contains(no)) {
				paths(pathMap(no)).append(s)
			} else {
				paths += (poly.hasLine match {
				case false => new SWFShape(polyColor, s, null, null);
				case true =>	new SWFShape(polyColor, s, poly.lineColor, stroke());
				})
				if(poly.path) pathMap(no)=paths.length - 1
			}
		}
		new SWFSprite(paths.toArray)
	}
}


/*
object main extends JPanel{

	val test2 = PDR.loadSWFSprite("test2.pdr");
	def main(args:Array[String] ){
		val frame = new JFrame();

		frame.getContentPane().add(this);

		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setBounds(10, 10, 300, 200);
		frame.setTitle("タイトル");
		frame.setVisible(true);
	}

	override def paintComponent(g:Graphics){
		val g2 = g.asInstanceOf[Graphics2D];

		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
												RenderingHints.VALUE_ANTIALIAS_ON);

		val wideStroke = new BasicStroke(1.0f);
		g2.setStroke(wideStroke);

		val curve1 = 
			new QuadCurve2D.Double(
					0.0d, 100.0d,
					0.0d, 	0.0d,
				100.0d, 100.0d);
		g2.draw(curve1);
		test2.draw(g2);
	}
}*/