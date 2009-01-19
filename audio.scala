package audio
import javax.sound.sampled._
import scala.collection.mutable._
import java.io._

class Audio extends Runnable {
	var module:TinySiOPM=new TinySiOPM()
	var frame = 3;
    var frameCounter = 0;
    var bshc = Array("6060003305300031",     "0000620000006001",  "3110011121110010",   "0000000000000000");
    var tone = Array(Array(640,3,11,24,-48), Array(960,4,3,20,0), Array(1280,2,3,32,0), Array(1024,4,3,8,0));
	var pong = Array(60,62,64,67,69);
	var pointer = 0;
    private def _onStream():Array[Byte] = {
    	frameCounter += 1
        if (frameCounter == frame) {
            for (i <- 0 until 3) {
                var v:int = bshc(i).charAt(pointer)-'0';
//                	println(v)
                if (v!=0) {
                	module.noteOn(tone(i)(0), v << tone(i)(1), tone(i)(2), tone(i)(3), tone(i)(4));
				}
            }
/*            if (mouseDown == 1) {
                var p:int = int(Math.random()*5);
                module.noteOn(pong[p]<<4, 64, 2, 8);
                mouseDown = p+2;
            }*//*
            if (bouns > 0) {
                module.noteOn(pong[bouns>>5]<<4, bouns&31, 2, 8);
                bouns = 0;
            }*/
            pointer = (pointer + 1) & 15;
            frameCounter = 0;
        }
        var data = new ByteArrayOutputStream(4096);
        module.buffer(data);
        data.toByteArray() ;
    }

	var thread:Thread = null
	var k = 0
	var hz = 100

	def play() {
		thread = new Thread(this)
		thread.start()
	}

	def getBuffer():Array[Byte] = {
		_onStream()
	}

	def run() {
		try {
			val fmt = new AudioFormat(
				AudioFormat.Encoding.PCM_SIGNED , 44100, 16, 1, 2, 44100, true)
 			val line = AudioSystem.getLine(
				new DataLine.Info(classOf[SourceDataLine], fmt)).asInstanceOf[SourceDataLine]
			line.open(); line.start()
			while (thread != null) {
				val buf = getBuffer()
				line.write(buf, 0, buf.length)
			}
			line.drain()
			line.stop()
			line.close()
		} catch {
		case e => e.printStackTrace()
		}
	}
}

class TinySiOPM {
    private var term:Note = null;
    private var pipe:Array[Double]=null;
    private var bufferSize:int = 2048;
    private var pitchTable = new Array[int](2048);      // 128:note * 16:detune
    private var logTable = new Array[Double](6144);  // 24:cause * 128:fine *2:p/n
    init()

    private def init() {
    	var j:int = 0;
    	var i:int = 0;
        var p:double = 0;
        var v:Double = 0;
        var t:Array[Int] =null;
        for (i <- 0 until 192) {
        	v = Math.pow(2, p)*12441.464342886
            j = i;while (j < 2048) { pitchTable(j) = v.asInstanceOf[Int]; v*=2;j+=192}
        	p+=0.00520833333
        }
        p=0.0078125;i=0;while (i < 256) {
			v=Math.pow(2, 13-p)*0.0001220703125;j=i;
            while (j < 3328) { logTable(j) = v; logTable(j+1) = -v; v*=0.5;j+=256 }
			i+=2;p+=0.0078125
        }
        for (i <- 3328 until 6144) { logTable(i) = 0; }
        var famtri = Array(0,1,2,3,4,5,6,7,6,5,4,3,2,1,0,0,-1,-2,-3,-4,-5,-6,-7,-8,-7,-6,-5,-4,-3,-2,-1,0);
        t=Note_.createTable(10);p=0;for (i<-0 until 1024) { t(i) = _logIndex(Math.sin(p)); p+=0.00613592315 } // sin=0
        t=Note_.createTable(10);p=0.75;for (i<-0 until 1024) { t(i) = _logIndex(p); p-=0.00146484375 }        // saw=1
        t=Note_.createTable(5);for ( i<- 0 until 32) { t(i) = _logIndex(famtri(i)*0.0625); }                      // famtri=2
        t=Note_.createTable(15);for (i<-0 until 32768) { t(i) = _logIndex(Note_.random()-0.5); }                 // wnoize=3
        for (i <- 0 until 8) {
        	t=Note_.createTable(4);
        	for (j<-0 until 16) {
        		t(j) = if(j<=i) 192 else 193
        	}
        }           // pulse=4-11
        term = new Note();
        pipe = new Array[Double](bufferSize);
        for (i<-0 until bufferSize) pipe(i) = 0;
        this.bufferSize = bufferSize;
        //println("init ok");
    }

    private def _logIndex(n:Double) : int = {
        if(n<0) {
           if(n< -0.00390625) {
             (((Math.log(-n) * -184.66496523 + 0.5).asInstanceOf[Int] + 1) << 1) + 1
           } else {
              2047
           }
        }else{
           if(n> 0.00390625) {
                 ( (Math.log( n) * -184.66496523 + 0.5).asInstanceOf[Int] + 1) << 1
            } else {
                 2046
            }
        }
    }
    var maxPipe:Double=0.0;
    var minPipe:Double=0.0;
    def buffer(data:ByteArrayOutputStream) {
        var n:Note = null
        var rep:int = 0
        var i:int = 0
        var imax:int = 0
        var dph:int = 0
        var lout:int = 0
        var v:int = 0
        imax = 1024
        while (imax < bufferSize ) {
            n=term.next; while (n!=term) {
                dph = pitchTable(n.pitch);for (i <- imax-1024 until imax) {
                    lout=n.table(n.ph >> n.shift)+n.gain;
                    pipe(i)+=logTable(lout);
                    n.ph=(n.ph+dph)&0x3ffffff
                }
                if (!n.inc()) n = n.remove();
                n=n.next
            }
            imax += 1024
        }
        var writer = new DataOutputStream(data);
        for (i <- 0 until bufferSize) {
        	var n=pipe(i)*90000;
        	if(n > 32767)n=32767
        	else if(n < -32768)n= -32768
        	writer.writeShort(n.asInstanceOf[Short]);
        	pipe(i)=0;
        }
    }
    def noteOn(pitch:int) {noteOn(pitch,64,0,4,0)}
    def noteOn(pitch:int, velocity:int) {noteOn(pitch,velocity,0,4,0)}
    def noteOn(pitch:int, velocity:int, tone:int) {noteOn(pitch,velocity,tone,4,0)}
    def noteOn(pitch:int, velocity:int, tone:int, decay:int) {noteOn(pitch,velocity,tone,decay,0)}
    def noteOn(pitch:int, velocity:int, tone:int, decay:int, sweep:int) {
        Note_.alloc().reset(tone, pitch, _logIndex(velocity*0.0078125), sweep, (decay<<2)).into(term);
    }
}

class Note {
    var prev:Note=this;
    var next:Note=this;
    var ph:int=0;
    var pitch:int=0;
    var gain:int=0;
    var sweep:int=0;
    var decay:int=0;
    var table:Array[Int]=null
    var shift:Int=0;
    def remove() : Note = {
    	// リストから自分を削除する。
    	var r:Note=prev;
    	prev.next=next;
    	next.prev=prev;
    	// intoする
    	into(Note_.free);// freeの前に自分を追加する
    	r// 前のnoteを返す
    }

    // nの前に自分を追加する
    def into(n:Note) : Note = {
    	prev=n.prev;
    	next=n;
    	prev.next=this;
    	next.prev=this;
    	this
    }

    def inc() : Boolean = {
    	gain+=decay;// gainにdecayを追加
    	pitch+=sweep;// pitchにsweepを追加
    	pitch&=2047;// pitchは0～2047の範囲とする
    	(gain<4000)// gainが4000以下かどうかを返す
    }
    def reset(t:int, p:int, g:int, s:int, d:int) : Note = {
        ph=0; pitch=p; gain=g; sweep=s; decay=d<<1; table=Note_.waveTable(t); shift=Note_.shiftTable(t); this
    }
}
object Note_ {

	var waveTable = new ArrayBuffer[Array[Int]] // テーブルのインデックス

	var shiftTable = new ArrayBuffer[Int] // 数値が入る
	// テーブルの作成
	def createTable(bit:Int) : Array[Int] = {
		var t = new Array[Int](1 << bit)// bit x 2のサイズのint配列を作る。
		Note_.waveTable += t// waveTableに保存する
		Note_.shiftTable += (26 - bit) // shiftTableに26-bitを追加する
		t // 作ったテーブルを返す
		// 疑問は26って何よってこと。
	}

	// freeなnote
	var free:Note = new Note();

	// もうひとつnoteを作成する
	def alloc() : Note = {
		// free.prev==freeなら新しいnoteを作る
		if (free.prev==free) return new Note();

		// free.prev!=freeでないときはfree.prevにfree.prev.prevを入れて、free.prev.nextにはfreeを入れてfree.prevを返す。
		var r:Note=free.prev;
		free.prev=r.prev;
		free.prev.next=free;
		// 最初free=free.prev = free.nextなので、Noteを作って返すだけだ。
		// そして、freeはnoteをremoveしたときだけ、変更される。
		// つまるところ、freeは、使わなくなったNoteの保存用リストだと思われる。
		r
	}
	var rnd = new Random();
	def random():Double = {rnd.nextDouble()}
}
