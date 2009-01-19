package game
import java.awt.event._
import java.awt.{Graphics,Graphics2D,Color,Dimension}
import java.awt.geom.AffineTransform
import java.awt.RenderingHints
import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import java.lang.Thread
import java.lang.Math
import java.util.Random
import javax.swing._
import scala.actors.Actor
import scala.actors.Actor._
import pdr._
import beep._

class gameApplet extends JApplet {
	override def init() {
		addKeyListener(game.panel)
		val pane = getContentPane()
		pane.add(game.panel)
	}
	override def start() {
		game.start()
	}
	override def stop() {
		game.stop
	}
}

object main extends Application {
	val window = new JFrame
	window getContentPane() add game.panel
	window show()
	window pack()
	window setResizable false
	window setDefaultCloseOperation JFrame.EXIT_ON_CLOSE
	window.addKeyListener(game.panel)
	game.start()
}

class Panel extends JPanel with KeyListener{
	setPreferredSize(new Dimension(300, 400))
	override def paintComponent(g:Graphics) {
		g setColor new Color(0x44ffffff)
		var b=getSize()
		g fillRect (0, 0, b.width, b.height)
		game.draw(g)
	}
	var up:boolean = false
	var down:boolean = false
	var left:boolean = false
	var right:boolean = false
	var shot:boolean = false
	var enter:boolean = false
	def setKey(c:Int,b:boolean) {
		c match {
		case 37 => left = b
		case 38 => up = b
		case 39 => right = b
		case 40 => down = b
		case 90 => shot = b
		case 10 => enter = b
		case _ =>
		}
	}
	def keyPressed(e:KeyEvent) {
		setKey(e.getKeyCode(), true)
	}
	def keyReleased(e:KeyEvent) {
		setKey(e.getKeyCode(), false)
	}
	def keyTyped(e:KeyEvent) {
	}

}

object game extends Actor {
	var panel = new Panel
	var beep:Beep = null
	def stop() {
		beep.stop
		this ! 'stop
	}
	def normalRad(v:Double):Double = {
		v + 2 * Math.PI * (
			if (v >  Math.PI) -1 else
			if (v < -Math.PI)  1 else 0
		);
	}

	def addBullets(x:Double, y:Double, rad:Double,n:int, r:Double) {
		for (i <- 0 until n) {
			new Bullet(x,y,rad+(i-n/2.0)*r)
		}
	}
	def addParticles(x:Double, y:Double, no:int, n:int) {
		for (i <- 0 until n) {
			new Particle(x,y, rnd(100.0), no)
		}
	}

	val bullet1 = PDR.loadSWFSprite("bullet1.pdr","",PDR.CENTER|PDR.VCENTER)
	val bullet2 = PDR.loadSWFSprite("bullet2.pdr","",PDR.CENTER|PDR.VCENTER)
	val ship1 = PDR.loadSWFSprite("ship.pdr","",PDR.CENTER|PDR.VCENTER)
	val enemy1 = PDR.loadSWFSprite("enemy.pdr","",PDR.CENTER|PDR.VCENTER)
	val shot = PDR.loadSWFSprite("shot.pdr","",PDR.CENTER|PDR.VCENTER)
	val title = PDR.loadSWFSprite("title.pdr","",0)
	val particle = PDR.loadSWFSprite("particle1.pdr","",0)
	val particle2 = PDR.loadSWFSprite("particle2.pdr","",0)
	val ship:Ship = new Ship(150,350)
	var rand:Random = new Random
	var enemys = List[Enemy]()
	var shots = List[Shot]()
	var bullets = List[Bullet]()
	var particles = List[Particle]()
	var nextShots = List[Shot]()
	var nextBullets = List[Bullet]()
	var nextEnemys = List[Enemy]()
	var nextParticles = List[Particle]()
	var enemysCnt = 0
	var shotsCnt = 0
	var bulletsCnt = 0
	var particlesCnt = 0
	def init() {
		beep = new Beep
		enemys=List()
		shots=List()
		bullets=List()
		particles=List()
		nextShots=List()
		nextBullets=List()
		nextEnemys=List()
		nextParticles=List()
		enemysCnt=0
		shotsCnt=0
		bulletsCnt=0
		particlesCnt=0
		rank = 0
		score = 0
		hiscore = 10000
		leftShips = 0
		gameOver = true
		gameOverCnt = 1000
	}
	def add(a:Enemy) {
		if(enemysCnt < 5) nextEnemys = a::nextEnemys
	}
	def add(a:Shot) {
		if(shotsCnt < 10) nextShots = a::nextShots
	}
	def add(a:Bullet) {
		if(bulletsCnt < 500) nextBullets = a::nextBullets
	}
	def add(a:Particle) {
		if(particlesCnt < 500) nextParticles = a::nextParticles
	}

	def act {
		init()
		var nextFrameStart = System.nanoTime();
		var loop = true
		this ! 'move
		while(loop) {
			receive {
			case 'move => move
				nextFrameStart += 28571429;
				var remaining = nextFrameStart - System.nanoTime();
				if (remaining > 0) {
					panel.repaint()
				} else {
					this ! 'move
				}
				remaining = nextFrameStart - System.nanoTime();
				if (remaining > 0) {
					try {
						Thread.sleep(remaining / 1000000);
					} catch {
						case _ => 
					}
				}
			case 'stop => loop = false
			}
		}
		println("game stop")
	}
	def rnd(n:Int):Int = {
		rand nextInt n
	}
	def rnd(n:Double):Double = {
		rand.nextDouble * n
	}

	var rank = 0
	var score = 0
	var hiscore = 10000
	var leftShips = 0
	var gameOver = true
	var gameOverCnt = 1000
	def move {
		nextShots = List()
		nextBullets = List()
		nextEnemys = List()
		nextParticles = List()
		if(gameOverCnt > 120 && gameOver) {
			if(game.panel.shot || game.panel.enter) {
				gameOver = false
				leftShips = 3
				ship.init
				score = 0
				rank = 0
			}
		}
		if(enemysCnt == 0 || rnd(1.0) < 0.01 * rank) {
			new Enemy(rnd(300), 0)
		}

		ship.move()

		bulletsCnt = 0
		for (a <- bullets) {
			a move ()
			if(a.move != null) {
				nextBullets = a :: nextBullets
				shotsCnt += 1
			}
			if(ship.exists && Math.abs(ship.x - a.x) < 3 && Math.abs(ship.y - a.y) < 3) {
				addParticles(ship.x, ship.y, 1, 100)
				ship.init
				if(leftShips == 0) {
					beep.play(0,32,32)
					gameOver = true
					gameOverCnt = 0
				} else {
					beep.play(0,32,64)
					leftShips -= 1
				}
			}
		}

		shotsCnt = 0
		for (s <- shots) {
			s move ()
			for(enemy <- enemys) {
				if(Math.abs(enemy.x-s.x)<30 && Math.abs(enemy.y-s.y)<30) {
					beep.play(64,4,-8)
					score += 100;
					if(hiscore < score)hiscore = score;
					rank = score / 1000;
					addParticles(enemy.x, enemy.y,2, 30);
					enemy.move = null
				}
			}
			if(s.move != null) {
				nextShots = s :: nextShots
				shotsCnt += 1
			}
		}
		enemysCnt = 0
		for (a <- enemys) {
			if(a.move != null) a move ()
			if(a.move != null) {
				nextEnemys = a :: nextEnemys
				enemysCnt += 1
			}
		}
		particlesCnt = 0
		for (a <- particles) {
			a move ()
			if(a.move != null) {
				nextParticles = a :: nextParticles
				particlesCnt += 1
			}
		}
		shots = nextShots.reverse
		bullets = nextBullets.reverse
		enemys = nextEnemys.reverse
		particles = nextParticles.reverse
		if(gameOver) {
			gameOverCnt += 1
		}
	}
	def draw(g:Graphics) {
		val g2 = g.asInstanceOf[Graphics2D]
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
												RenderingHints.VALUE_ANTIALIAS_ON);
		g2.setRenderingHint(RenderingHints.KEY_RENDERING, 
												RenderingHints.VALUE_RENDER_SPEED);
		g2.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, 
												RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED);
		g2.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, 
												RenderingHints.VALUE_COLOR_RENDER_SPEED);

		val at = g2.getTransform();
		for (particle <- particles) {
			particle draw g2
			g2.setTransform(at);
		}
		for (a <- shots) {
			a draw g2
			g2.setTransform(at);
		}
		if(!gameOver) ship draw g2
		g2.setTransform(at);
		for (enemy <- enemys) {
			enemy draw g2
			g2.setTransform(at);
		}
		for (bullet <- bullets) {
			bullet draw g2
			g2.setTransform(at);
		}

		g2.setTransform(at);
		for(i <- 0 until leftShips) {
			g2.translate(250+i*10,15);
			g2.scale(0.4,0.4);
			game.ship1.draw(g2)
			g2.setTransform(at);
		}
		g setColor new Color(0x000000)
		g drawString("SCORE "+score,0,20)
		g drawString("HI SCORE "+hiscore,100,20)
		if(gameOver) {
			if(gameOverCnt % 60 > 15) {
				g drawString("GAME OVER", 115, 220)
			}
			if(gameOverCnt > 60*2) {

				g2.setTransform(at);
				g2.translate(40,230);
				g2.scale(5.4,5.4);
				g2.rotate(-Math.PI/180*30);
				game.enemy1.draw(g2)
				g2.setTransform(at);

				game.title.draw(g2)

				g2.translate(250,215);
				g2.scale(5.4,5.4);
				game.ship1.draw(g2)
				g2.setTransform(at);

				g drawString("PRESS Z BUTTON", 98, 250)
			}
		}
		this ! 'move
	}
}

class GameActor{}
class Ship(var x:Double,var y:Double) extends GameActor {
	var exists:boolean = false
	var existsCount = 100
	def init {
		exists = false
		existsCount = 130
	}
	def move1 {
		if(!game.gameOver) {
			if(!exists) {
				existsCount -= 1
				if(existsCount==0) {
					exists = true
				}
			}
			if(existsCount < 100) {
				if(game.panel.up) y -= vy
				if(game.panel.down) y += vy
				if(game.panel.left) x -= vx
				if(game.panel.right) x += vx
				if(game.panel.shot) new Shot(x,y)
			}
		}
		if(x < 0  ) { x = 0 ; }
		if(y < 0  ) { y = 0 ; }
		if(x > 300) { x = 300 ; }
		if(y > 400) { y = 400 ; }
	}
	def draw1(g2:Graphics2D) {
		if(existsCount < 100 && existsCount % 2 == 0) {
			g2.translate(x,y)
			g2.rotate(r)
			game.ship1.draw(g2)
		}
	}

	var vx = 5
	var vy = 5
	var r = 0.0d
	var vr = 0.2d
	var draw = draw1 _
	var move = move1 _

}
class Shot(var x:Double,var y:Double) extends GameActor {

	def move1 {
		x += vx; y += vy
		if(x < 0  ) { remove }
		if(y < 0  ) { remove }
		if(x > 300) { remove }
		if(y > 400) { remove }
	}
	def remove {
		move = null
	}
	def draw1(g2:Graphics2D) {
		if(move != null){
			g2.translate(x,y)
			game.shot.draw(g2)
		}
	}

	var vx = 0
	var vy = -16
	var draw = draw1 _
	var move = move1 _
	game.add(this)

}
import java.lang.Math
class Enemy(var x:Double,var y:Double) extends GameActor {
	var speed:Double = 2
	var rad = Math.atan2(game.ship.y-y,game.ship.x-x)
	var r:Double = 0.02

	var draw = draw1 _
	var move = move1 _
	game.add(this)


	def moveBody:Double = {
		val rad2 = Math.atan2(game.ship.y-y,game.ship.x-x);
		rad = rad + (if(game.normalRad(rad-rad2)<0) r else -r);
		rad = game.normalRad(rad)
		x += Math.cos(rad)*speed
		y += Math.sin(rad)*speed
		if(x < 0  ) { remove }
		if(y < 0  ) { remove }
		if(x > 300) { remove }
		if(y > 400) { remove }
		rad2
	}
	def remove {
		move = null
	}
	var nextMove:()=>Unit = null
	var time:int = 0
	def wait(w:int, next1:()=>Unit) {
		time = w
		move = moveWait _
		nextMove = next1
	}
	def moveWait {
		moveBody
		time -= 1
		if(time <= 0) move = nextMove
	}
	def move1 {
		var r = moveBody
		if(game.rnd(1.0) > 0.8) {
			new Bullet(x,y,r)
			if(game.rnd(1.0) > 0.8) {
				r2 = 0
				wait(30,move2 _)
			}
		}
	}
	private var r2:Double = 0;
	def move2 {
		moveBody
		var rr:Int = (game.rnd(5)+1).asInstanceOf[Int]
		game.addBullets(x,y,r+r2,rr,0.1)
		r2 = r2 + (rr * 0.1)
		if(game.rnd(1.0) < 0.03)wait(60,move1 _)
    }

	def draw1(g2:Graphics2D) {
		g2.translate(x,y)
		g2.rotate(rad-Math.PI/2)
		game.enemy1.draw(g2)
	}

}

class Bullet(var x:Double,var y:Double, rad:Double) extends GameActor {
	var speed = 3.0
	var r = 0.0d
	var vr = 0.2d
	var draw = draw1 _
	var move = move1 _
	game.add(this)

	def move1 {
		r += vr;
		x += Math.cos(rad)*speed
		y += Math.sin(rad)*speed
		if(x < 0  ) { x = 0 ;   remove }
		if(y < 0  ) { y = 0 ;   remove }
		if(x > 300) { x = 300 ; remove }
		if(y > 400) { y = 400 ; remove }
	}
	def remove {
		move = null
	}
	def draw1(g2:Graphics2D) {
		g2.translate(x,y)
		g2.rotate(r)
		game.bullet1.draw(g2)
	}

}

class Particle(var x:Double,var y:Double,rad:Double,no:int) extends GameActor {
	var speed = game.rnd(50.0)+10.0;
	var r = 0.0d
	var vr = 0.2d
	var draw = if(no==1) draw1 _ else draw2 _
	var move = move1 _
	game.add(this)

	def move1 {
		r += vr;
		x += Math.cos(rad)*speed
		y += Math.sin(rad)*speed
		if(x < 0  ) { x = 0 ;   remove }
		if(y < 0  ) { y = 0 ;   remove }
		if(x > 300) { x = 300 ; remove }
		if(y > 400) { y = 400 ; remove }
	}
	def remove {
		move = null
	}
	def draw1(g2:Graphics2D) {
		g2.translate(x,y)
		g2.rotate(r)
		game.particle.draw(g2)
	}
	def draw2(g2:Graphics2D) {
		g2.translate(x,y)
		g2.rotate(r)
		game.particle2.draw(g2)
	}

}
